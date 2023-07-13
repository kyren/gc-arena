use alloc::{
    rc::{Rc, Weak},
    vec::Vec,
};
use core::{fmt, mem};

use crate::lock::RefLock;
use crate::{
    barrier::{unlock, Write},
    metrics::Metrics,
};
use crate::{Collect, Gc, Mutation, Root, Rootable};

/// A way of registering GC roots dynamically.
///
/// Use this type as (a part of) an [`Arena`](crate::Arena) root to enable dynamic rooting of
/// GC'd objects through [`DynamicRoot`] handles.
// SAFETY: Allows us to conert `Gc<'gc>` pointers to `Gc<'static>` and back, and this is VERY
// sketchy. We know it is safe because:
//   1) The `DynamicRootSet` must be created inside an arena and is branded with an invariant `'gc`
//      lifetime.
//   2) The inner `id` inside the `DynamicRootSet` is unique for as long as the `DynamicRootSet` or
//      any `DynamicRoot` still references it.
//   3) The `id` is stored inside each `DynamicRoot` and checked against the one in the set, and
//      a match lets us know that this `Gc` must have originated from *this* set, so it is safe to
//      cast it back to whatever our current `'gc` lifetime is.
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
                handles: RefLock::new(Vec::new()),
                metrics: mc.metrics().clone(),
            },
        ))
    }

    /// Puts a root inside this root set.
    ///
    /// The returned handle can be freely stored outside the current arena,
    /// and will keep the root alive across garbage collections.
    pub fn stash<R: for<'a> Rootable<'a>>(
        &self,
        mc: &Mutation<'gc>,
        root: Root<'gc, R>,
    ) -> DynamicRoot<R> {
        let handle = Handle::new(&self.0.metrics, root);
        Inner::adopt_handle(&Gc::write(mc, self.0), &handle);

        DynamicRoot {
            handle: unsafe {
                mem::transmute::<Rc<Handle<Root<'gc, R>>>, Rc<Handle<Root<'static, R>>>>(handle)
            },
        }
    }

    /// Gets immutable access to the given root.
    ///
    /// # Panics
    ///
    /// Panics if the handle doesn't belong to this root set. For the non-panicking variant, use
    /// [`try_fetch`](Self::try_fetch).
    #[inline]
    pub fn fetch<'a, R: for<'r> Rootable<'r>>(&self, root: &'a DynamicRoot<R>) -> &'a Root<'gc, R> {
        if self.contains(root) {
            unsafe { &*root.as_ptr() }
        } else {
            panic!("mismatched root set")
        }
    }

    /// Gets immutable access to the given root, or returns an error if the handle doesn't belong
    /// to this root set.
    #[inline]
    pub fn try_fetch<'a, R: for<'r> Rootable<'r>>(
        &self,
        root: &'a DynamicRoot<R>,
    ) -> Result<&'a Root<'gc, R>, MismatchedRootSet> {
        if self.contains(root) {
            unsafe { Ok(&*root.as_ptr()) }
        } else {
            Err(MismatchedRootSet(()))
        }
    }

    /// Tests if the given handle belongs to this root set.
    #[inline]
    pub fn contains<R: for<'r> Rootable<'r>>(&self, root: &DynamicRoot<R>) -> bool {
        // We test for whether the handle belongs to this root set based on whether or not the two
        // instances of `Metrics` point to the same object.
        //
        // SAFETY: A unique `Metrics` instance is constructed once per arena and remains the same
        // for the lifetime of the arena.
        Metrics::ptr_eq(&self.0.metrics, &root.handle.metrics)
    }
}

/// An unbranded, reference-counted handle to a GC root held in some [`DynamicRootSet`].
///
/// A `DynamicRoot` can freely be stored outside GC arenas; in exchange, all accesses to the held
/// object must go through the [`DynamicRootSet`] from which it was created.
///
/// This handle is cheaply clonable: all clones will refer to the *same* object, which will be
/// dropped when the last surviving handle goes out of scope.
pub struct DynamicRoot<R: for<'gc> Rootable<'gc>> {
    handle: Rc<Handle<Root<'static, R>>>,
}

impl<R: for<'gc> Rootable<'gc>> Clone for DynamicRoot<R> {
    fn clone(&self) -> Self {
        Self {
            handle: self.handle.clone(),
        }
    }
}

impl<R: for<'gc> Rootable<'gc>> DynamicRoot<R> {
    /// Get a pointer to the held object.
    ///
    /// # Safety
    ///
    /// The pointer will never be dangling as long as at least one `DynamicRoot` is alive, but
    /// using the object behind this pointer is extremely dangerous.
    ///
    /// Firstly, the `'gc` lifetime returned here is unbound, so it is meaningless and can allow
    /// improper mixing of objects across arenas.
    ///
    /// Secondly, though the pointer to the object *itself* will not be dangling, any garbage
    /// collected pointers the object holds *will* be dangling if the [`DynamicRootSet`] backing
    /// this root has been collected.
    #[inline]
    pub fn as_ptr<'gc>(&self) -> *const Root<'gc, R> {
        unsafe { mem::transmute::<&Root<'static, R>, &Root<'gc, R>>(&self.handle.root) as *const _ }
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

type HandleList<'gc> = Vec<Weak<Handle<dyn Collect + 'gc>>>;

struct Inner<'gc> {
    handles: RefLock<HandleList<'gc>>,
    metrics: Metrics,
}

fn size_of_handle_list<'gc>(list: &HandleList<'gc>) -> usize {
    list.capacity() * mem::size_of::<Weak<Handle<dyn Collect + 'gc>>>()
}

impl<'gc> Inner<'gc> {
    fn adopt_handle<R: Collect + 'gc>(this: &Write<Self>, handle: &Rc<Handle<R>>) {
        let handles = unlock!(this, Inner, handles);
        let mut handles = handles.borrow_mut();
        let old_size = size_of_handle_list(&handles);
        handles.push(Rc::<Handle<R>>::downgrade(handle));
        let new_size = size_of_handle_list(&handles);

        if new_size > old_size {
            this.metrics.mark_external_allocation(new_size - old_size);
        } else if old_size > new_size {
            this.metrics.mark_external_deallocation(old_size - new_size);
        }
    }
}

impl<'gc> Drop for Inner<'gc> {
    fn drop(&mut self) {
        self.metrics
            .mark_external_deallocation(size_of_handle_list(&self.handles.borrow()));
    }
}

unsafe impl<'gc> Collect for Inner<'gc> {
    fn trace(&self, cc: &crate::Collection) {
        // SAFETY: We do not adopt any new pointers so we don't need a write barrier.
        // We cheat horribly and filter out dead handles during tracing. Since we have to go
        // through the entire list of roots anyway, this is cheaper than filtering on e.g.
        // stashing new roots.
        let handles = unsafe { self.handles.as_ref_cell() };
        handles.borrow_mut().retain(|handle| {
            if let Some(handle) = handle.upgrade() {
                handle.root.trace(cc);
                true
            } else {
                false
            }
        });
    }
}

struct Handle<T: ?Sized> {
    metrics: Metrics,
    root: T,
}

impl<T> Handle<T> {
    fn new(metrics: &Metrics, root: T) -> Rc<Self> {
        metrics.mark_external_allocation(mem::size_of::<T>());
        Rc::new(Self {
            metrics: metrics.clone(),
            root,
        })
    }
}

impl<T: ?Sized> Drop for Handle<T> {
    fn drop(&mut self) {
        self.metrics
            .mark_external_deallocation(mem::size_of_val(&self.root));
    }
}
