use alloc::{
    rc::{Rc, Weak},
    vec::Vec,
};
use core::{cell::RefCell, mem, ptr::NonNull};

use crate::{types::GcBox, Collect, Gc, MutationContext, Root, Rootable};

// SAFETY: We conert `Gc<'gc>` pointers to `Gc<'static>` and back, and this is VERY sketchy. We know
// it is safe because:
//   1) The `DynamicRootSet` must be created inside an arena and is branded with an invariant `'gc`
//      lifetime.
//   2) The `id` held inside the `DynamicRootSet` is a non-zero allocation with a *unique* address.
//   3) The `id` is stored inside each `DynamicRoot` and checked against the set `id`, and a match
//      lets us know that this `Gc` must have originated from *this* set, so it is safe to cast it
//      back to whatever our current `'gc` lifetime is.
pub struct DynamicRootSet<'gc> {
    id: Gc<'gc, u8>,
    handles: RefCell<Vec<Handle<'gc>>>,
}

#[derive(Clone)]
pub struct DynamicRoot<R: for<'gc> Rootable<'gc> + ?Sized> {
    ptr: Gc<'static, Root<'static, R>>,
    id: *const u8,
    // We identify dropped handles by checking an `Rc` handle count.
    _rc: Rc<()>,
}

struct Handle<'gc> {
    ptr: NonNull<GcBox<dyn Collect + 'gc>>,
    rc: Weak<()>,
}

unsafe impl<'gc> Collect for DynamicRootSet<'gc> {
    fn trace(&self, cc: crate::CollectionContext) {
        unsafe {
            self.id.trace(cc);

            // We cheat horribly and filter out dead handles during tracing. Since we have to go through
            // the entire list of roots anyway, this is cheaper than filtering on e.g. stashing new
            // roots.
            self.handles.borrow_mut().retain(|handle| {
                if Weak::strong_count(&handle.rc) > 0 {
                    cc.trace(handle.ptr);
                    true
                } else {
                    false
                }
            });
        }
    }
}

impl<'gc> DynamicRootSet<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>) -> Self {
        DynamicRootSet {
            id: Gc::allocate(mc, 0),
            handles: RefCell::new(Vec::new()),
        }
    }

    pub fn stash<R: for<'a> Rootable<'a> + ?Sized>(
        &mut self,
        root: Gc<'gc, Root<'gc, R>>,
    ) -> DynamicRoot<R> {
        let rc = Rc::new(());

        self.handles.get_mut().push(Handle {
            ptr: root.ptr,
            rc: Rc::downgrade(&rc),
        });

        DynamicRoot {
            ptr: unsafe {
                mem::transmute::<Gc<'gc, Root<'gc, R>>, Gc<'static, Root<'static, R>>>(root)
            },
            id: Gc::as_ptr(self.id),
            _rc: rc,
        }
    }

    pub fn fetch<R: for<'a> Rootable<'a> + ?Sized>(
        &self,
        root: &DynamicRoot<R>,
    ) -> Gc<'gc, Root<'gc, R>> {
        assert_eq!(
            Gc::as_ptr(self.id),
            root.id,
            "provided `DynamicRoot` does not come from the `DynamicRootSet` it was provided to"
        );

        unsafe { mem::transmute::<Gc<'static, Root<'static, R>>, Gc<'gc, Root<'gc, R>>>(root.ptr) }
    }
}
