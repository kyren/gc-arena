use alloc::{
    rc::{Rc, Weak},
    vec::Vec,
};
use core::mem;

use crate::{Collect, Gc, MutationContext, RefLock, Root, Rootable};

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
pub struct DynamicRootSet<'gc>(Gc<'gc, RefLock<Inner<'gc>>>);

unsafe impl<'gc> Collect for DynamicRootSet<'gc> {
    fn trace(&self, cc: crate::CollectionContext) {
        // SAFETY: We do not adopt any new pointers so we don't need a write barrier.
        unsafe {
            // We cheat horribly and filter out dead handles during tracing. Since we have to go
            // through the entire list of roots anyway, this is cheaper than filtering on e.g.
            // stashing new roots.
            self.0
                .as_ref_cell()
                .borrow_mut()
                .handles
                .retain(|handle| Weak::strong_count(&handle) > 0);
        }

        self.0.trace(cc);
    }
}

impl<'gc> DynamicRootSet<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>) -> Self {
        DynamicRootSet(Gc::allocate(
            mc,
            RefLock::new(Inner {
                handles: Vec::new(),
                set_id: Rc::new(SetId {}),
            }),
        ))
    }

    pub fn stash<R: for<'a> Rootable<'a> + ?Sized>(
        &self,
        mc: MutationContext<'gc, '_>,
        root: Root<'gc, R>,
    ) -> DynamicRoot<R> {
        let mut inner = self.0.write_ref_cell(mc).borrow_mut();

        let handle = Rc::new(Handle {
            set_id: inner.set_id.clone(),
            root,
        });

        let weak_handle: Weak<Handle<dyn Collect>> = Rc::<Handle<Root<'gc, R>>>::downgrade(&handle);
        inner.handles.push(weak_handle);

        DynamicRoot {
            handle: unsafe {
                mem::transmute::<Rc<Handle<Root<'gc, R>>>, Rc<Handle<Root<'static, R>>>>(handle)
            },
        }
    }

    pub fn fetch<'a, R: for<'b> Rootable<'b> + ?Sized>(
        &self,
        root: &'a DynamicRoot<R>,
    ) -> &'a Root<'gc, R> {
        assert_eq!(
            Rc::as_ptr(&self.0.borrow().set_id),
            Rc::as_ptr(&root.handle.set_id),
            "provided `DynamicRoot` does not originate from this `DynamicRootSet`",
        );

        unsafe { mem::transmute::<&'a Root<'static, R>, &'a Root<'gc, R>>(&root.handle.root) }
    }
}

pub struct DynamicRoot<R: for<'gc> Rootable<'gc> + ?Sized> {
    handle: Rc<Handle<Root<'static, R>>>,
}

impl<R: for<'gc> Rootable<'gc> + ?Sized> Clone for DynamicRoot<R> {
    fn clone(&self) -> Self {
        Self {
            handle: self.handle.clone(),
        }
    }
}

impl<R: for<'gc> Rootable<'gc> + ?Sized> DynamicRoot<R> {
    // Get a pointer to the held object.
    //
    // The pointer will never be dangling, as the `DynamicRoot` is the owner of the held type, but
    // using the object behind this pointer is extremely dangerous.
    //
    // Firstly, the 'gc lifetime returned here is unbound, so it is meaningless and can allow
    // improper mixing of objects across arenas.
    //
    // Secondly, though the pointer to the object *itself* will not be dangling, any garbage
    // collected pointers the object holds *will* be dangling if the arena backing this root has
    // been dropped.
    pub fn as_ptr<'gc>(&self) -> *const Root<'gc, R> {
        unsafe { mem::transmute::<&Root<'static, R>, &Root<'gc, R>>(&self.handle.root) as *const _ }
    }
}

// The address of an allocated `SetId` type uniquely identifies a single `DynamicRootSet`.
struct SetId {}

struct Inner<'gc> {
    handles: Vec<Weak<Handle<dyn Collect + 'gc>>>,
    set_id: Rc<SetId>,
}

unsafe impl<'gc> Collect for Inner<'gc> {
    fn trace(&self, cc: crate::CollectionContext) {
        for handle in &self.handles {
            if let Some(handle) = handle.upgrade() {
                handle.root.trace(cc);
            }
        }
    }
}

struct Handle<T: ?Sized> {
    // Store a clone of `Rc<SetId>` so that we ensure the `Rc<SetId>` lives as long as any extant
    // handle.
    set_id: Rc<SetId>,
    root: T,
}
