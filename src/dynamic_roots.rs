use core::{cell::RefCell, fmt, mem};

use alloc::{
    rc::{Rc, Weak},
    vec::Vec,
};

use crate::{arena::Root, metrics::Metrics, Collect, Gc, Mutation, Rootable};

/// A way of registering GC roots dynamically.
///
/// Use this type as (a part of) an [`Arena`](crate::Arena) root to enable dynamic rooting of
/// GC'd objects through [`DynamicRoot`] handles.
//
// SAFETY: This allows us to convert `Gc<'gc>` pointers to `Gc<'static>` and back, and this is VERY
// sketchy. We know it is safe because:
//   1) The `DynamicRootSet` must be created inside an arena and is branded with an invariant `'gc`
//      lifetime.
//   2) When stashing a `Gc<'gc, R>` pointer, the invariant `'gc` lifetimes must match.
//   3) When fetching, we make sure that the `DynamicRoot` `slots` field points to the same object
//      as the `slots` field in the `DynamicRootSet`. We never drop this `Rc` or change the `Weak`
//      held in any `DynamicRoot`, so if they both point to the same object, the original `Gc`
//      pointer *must* have originally been stashed using *this* set. Therefore, it is safe to cast
//      it back to whatever our current `'gc` lifetime is.
#[derive(Copy, Clone)]
pub struct DynamicRootSet<'gc>(Gc<'gc, Inner<'gc>>);

unsafe impl<'gc> Collect for DynamicRootSet<'gc> {
    fn trace(&self, cc: &crate::Collection) {
        self.0.trace(cc);
    }
}

impl<'gc> DynamicRootSet<'gc> {
    /// Creates a new, empty root set.
    pub fn new(mc: &Mutation<'gc>) -> Self {
        DynamicRootSet(Gc::new(
            mc,
            Inner {
                slots: Rc::new(RefCell::new(Slots::new(mc.metrics().clone()))),
            },
        ))
    }

    /// Puts a root inside this root set.
    ///
    /// The returned handle can be freely stored outside the current arena, and will keep the root
    /// alive across garbage collections.
    pub fn stash<R: for<'a> Rootable<'a>>(
        &self,
        mc: &Mutation<'gc>,
        root: Gc<'gc, Root<'gc, R>>,
    ) -> DynamicRoot<R> {
        // SAFETY: We are adopting a new `Gc` pointer, so we must invoke a write barrier.
        mc.backward_barrier(Gc::erase(self.0), Some(Gc::erase(root)));

        let mut slots = self.0.slots.borrow_mut();
        let index = slots.add(unsafe { Gc::cast(root) });

        let ptr =
            unsafe { mem::transmute::<Gc<'gc, Root<'gc, R>>, Gc<'static, Root<'static, R>>>(root) };
        let slots = unsafe {
            mem::transmute::<Weak<RefCell<Slots<'gc>>>, Weak<RefCell<Slots<'static>>>>(
                Rc::downgrade(&self.0.slots),
            )
        };

        DynamicRoot { ptr, slots, index }
    }

    /// Gets immutable access to the given root.
    ///
    /// # Panics
    ///
    /// Panics if the handle doesn't belong to this root set. For the non-panicking variant, use
    /// [`try_fetch`](Self::try_fetch).
    #[inline]
    pub fn fetch<R: for<'r> Rootable<'r>>(&self, root: &DynamicRoot<R>) -> Gc<'gc, Root<'gc, R>> {
        if self.contains(root) {
            unsafe {
                mem::transmute::<Gc<'static, Root<'static, R>>, Gc<'gc, Root<'gc, R>>>(root.ptr)
            }
        } else {
            panic!("mismatched root set")
        }
    }

    /// Gets immutable access to the given root, or returns an error if the handle doesn't belong
    /// to this root set.
    #[inline]
    pub fn try_fetch<R: for<'r> Rootable<'r>>(
        &self,
        root: &DynamicRoot<R>,
    ) -> Result<Gc<'gc, Root<'gc, R>>, MismatchedRootSet> {
        if self.contains(root) {
            Ok(unsafe {
                mem::transmute::<Gc<'static, Root<'static, R>>, Gc<'gc, Root<'gc, R>>>(root.ptr)
            })
        } else {
            Err(MismatchedRootSet(()))
        }
    }

    /// Tests if the given handle belongs to this root set.
    #[inline]
    pub fn contains<R: for<'r> Rootable<'r>>(&self, root: &DynamicRoot<R>) -> bool {
        // NOTE: We are making an assumption about how `Weak` works that is currently true and
        // surely MUST continue to be true, but is possibly under-specified in the stdlib. We are
        // assuming that if the `Weak` pointer held in the given `DynamicRoot` points to a *dropped*
        // root set, that `Weak::as_ptr` will return a pointer that cannot possibly belong to a
        // live `Rc`.
        let ours = unsafe {
            mem::transmute::<*const RefCell<Slots<'gc>>, *const RefCell<Slots<'static>>>(
                Rc::as_ptr(&self.0.slots),
            )
        };
        let theirs = Weak::as_ptr(&root.slots);
        ours == theirs
    }
}

/// Handle to a `Gc` pointer held inside a [`DynamicRootSet`] which is `'static` and can be held
/// outside of the arena.
pub struct DynamicRoot<R: for<'gc> Rootable<'gc>> {
    ptr: Gc<'static, Root<'static, R>>,
    slots: Weak<RefCell<Slots<'static>>>,
    index: Index,
}

impl<R: for<'gc> Rootable<'gc>> Drop for DynamicRoot<R> {
    fn drop(&mut self) {
        if let Some(slots) = self.slots.upgrade() {
            slots.borrow_mut().dec(self.index);
        }
    }
}

impl<R: for<'gc> Rootable<'gc>> Clone for DynamicRoot<R> {
    fn clone(&self) -> Self {
        if let Some(slots) = self.slots.upgrade() {
            slots.borrow_mut().inc(self.index);
        }

        Self {
            ptr: self.ptr,
            slots: self.slots.clone(),
            index: self.index,
        }
    }
}

impl<R: for<'gc> Rootable<'gc>> DynamicRoot<R> {
    /// Get a pointer to the held object.
    ///
    /// This returns [`Gc::as_ptr`] for the [`Gc`] provided when the `DynamicRoot` is stashed.
    ///
    /// # Safety
    ///
    /// It is possible to use this to reconstruct the original `Gc` pointer by calling the unsafe
    /// [`Gc::from_ptr`], but this is incredibly dangerous!
    ///
    /// First, if the [`DynamicRootSet`] in which the `DynamicRoot` was stashed has been collected,
    /// then either the returned pointer or other transitive `Gc` pointers objects may be dangling.
    /// The parent `DynamicRootSet` *must* still be uncollected in order to do this soundly.
    ///
    /// Second, the `'gc` lifetime returned here is unbound, so it is meaningless and can allow
    /// improper mixing of objects across arenas. The returned `'gc` lifetime must be bound to only
    /// the arena that holds the parent `DynamicRootSet`.
    #[inline]
    pub fn as_ptr<'gc>(&self) -> *const Root<'gc, R> {
        unsafe { mem::transmute::<&Root<'static, R>, &Root<'gc, R>>(&self.ptr) as *const _ }
    }
}

/// Error returned when trying to fetch a [`DynamicRoot`] from the wrong [`DynamicRootSet`].
#[derive(Debug)]
pub struct MismatchedRootSet(());

impl fmt::Display for MismatchedRootSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("mismatched root set")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for MismatchedRootSet {}

struct Inner<'gc> {
    slots: Rc<RefCell<Slots<'gc>>>,
}

unsafe impl<'gc> Collect for Inner<'gc> {
    fn trace(&self, cc: &crate::Collection) {
        let slots = self.slots.borrow();
        slots.trace(cc);
    }
}

type Index = usize;

// By avoiding Option<usize>, `Slot` can go from 24 bytes to 16.
//
// usize::MAX can never be a valid index without using more than `usize::MAX` memory in the slots
// vec, which is impossible.
const NULL_INDEX: Index = usize::MAX;

enum Slot<'gc> {
    Vacant { next_free: Index },
    Occupied { root: Gc<'gc, ()>, ref_count: usize },
}

unsafe impl<'gc> Collect for Slot<'gc> {
    fn trace(&self, cc: &crate::Collection) {
        match self {
            Slot::Vacant { .. } => {}
            Slot::Occupied { root, .. } => root.trace(cc),
        }
    }
}

struct Slots<'gc> {
    metrics: Metrics,
    slots: Vec<Slot<'gc>>,
    next_free: Index,
}

impl<'gc> Drop for Slots<'gc> {
    fn drop(&mut self) {
        self.metrics
            .mark_external_deallocation(self.slots.capacity() * mem::size_of::<Slot>());
    }
}

unsafe impl<'gc> Collect for Slots<'gc> {
    fn trace(&self, cc: &crate::Collection) {
        self.slots.trace(cc);
    }
}

impl<'gc> Slots<'gc> {
    fn new(metrics: Metrics) -> Self {
        Self {
            metrics,
            slots: Vec::new(),
            next_free: NULL_INDEX,
        }
    }

    fn add(&mut self, p: Gc<'gc, ()>) -> Index {
        // Occupied slot refcount starts at 0. A refcount of 0 and a set ptr implies that there is
        // *one* live reference.

        if self.next_free != NULL_INDEX {
            let idx = self.next_free;
            let slot = &mut self.slots[idx];
            match *slot {
                Slot::Vacant { next_free } => {
                    self.next_free = next_free;
                }
                Slot::Occupied { .. } => panic!("free slot linked list corrupted"),
            }
            *slot = Slot::Occupied {
                root: p,
                ref_count: 0,
            };
            idx
        } else {
            let idx = self.slots.len();

            let old_capacity = self.slots.capacity();
            self.slots.push(Slot::Occupied {
                root: p,
                ref_count: 0,
            });
            let new_capacity = self.slots.capacity();

            debug_assert!(new_capacity >= old_capacity);
            if new_capacity > old_capacity {
                self.metrics.mark_external_allocation(
                    (new_capacity - old_capacity) * mem::size_of::<Slot>(),
                );
            }

            idx
        }
    }

    fn inc(&mut self, idx: Index) {
        match &mut self.slots[idx] {
            Slot::Occupied { ref_count, .. } => {
                *ref_count = ref_count
                    .checked_add(1)
                    .expect("DynamicRoot refcount overflow!");
            }
            Slot::Vacant { .. } => panic!("taken slot has been improperly freed"),
        }
    }

    fn dec(&mut self, idx: Index) {
        let slot = &mut self.slots[idx];
        match slot {
            Slot::Occupied { ref_count, .. } => {
                if *ref_count == 0 {
                    *slot = Slot::Vacant {
                        next_free: self.next_free,
                    };
                    self.next_free = idx;
                } else {
                    *ref_count -= 1;
                }
            }
            Slot::Vacant { .. } => panic!("taken slot has been improperly freed"),
        }
    }
}
