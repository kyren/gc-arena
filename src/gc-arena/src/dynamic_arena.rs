use alloc::{
    rc::{Rc, Weak},
    vec::Vec,
};
use core::{mem, ptr::NonNull};

use crate::{types::GcBox, Collect, Gc, GcCell, MutationContext, Root, Rootable};

// SAFETY: Allows us to conert `Gc<'gc>` pointers to `Gc<'static>` and back, and this is VERY
// sketchy. We know it is safe because:
//   1) The `DynamicRootSet` must be created inside an arena and is branded with an invariant `'gc`
//      lifetime.
//   2) The inner type inside the `DynamicRootSet` is a non-zero allocation with a *unique* address.
//   3) The pointer to this type is stored inside each `DynamicRoot` and checked against the set
//      `pointer, and a match lets us know that this `Gc` must have originated from *this* set, so
//      it is safe to cast it back to whatever our current `'gc` lifetime is.
#[derive(Copy, Clone)]
pub struct DynamicRootSet<'gc>(GcCell<'gc, Vec<Handle<'gc>>>);

#[derive(Clone)]
pub struct DynamicRoot<R: for<'gc> Rootable<'gc> + ?Sized> {
    ptr: Gc<'static, Root<'static, R>>,
    id: *const (),
    // We identify dropped handles by checking an `Rc` handle count.
    _rc: Rc<()>,
}

struct Handle<'gc> {
    ptr: NonNull<GcBox<dyn Collect + 'gc>>,
    rc: Weak<()>,
}

unsafe impl<'gc> Collect for Handle<'gc> {
    fn trace(&self, cc: crate::CollectionContext) {
        unsafe {
            cc.trace(self.ptr);
        }
    }
}

unsafe impl<'gc> Collect for DynamicRootSet<'gc> {
    fn trace(&self, cc: crate::CollectionContext) {
        // SAFETY: We do not adopt any new pointers so we don't need a write barrier.
        unsafe {
            // We cheat horribly and filter out dead handles during tracing. Since we have to go
            // through the entire list of roots anyway, this is cheaper than filtering on e.g.
            // stashing new roots.
            self.0
                .borrow_mut()
                .retain(|handle| Weak::strong_count(&handle.rc) > 0);
        }

        self.0.trace(cc);
    }
}

impl<'gc> DynamicRootSet<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>) -> Self {
        DynamicRootSet(GcCell::allocate(mc, Vec::new()))
    }

    pub fn stash<R: for<'a> Rootable<'a> + ?Sized>(
        &self,
        mc: MutationContext<'gc, '_>,
        root: Gc<'gc, Root<'gc, R>>,
    ) -> DynamicRoot<R> {
        let rc = Rc::new(());

        self.0.write(mc).push(Handle {
            ptr: root.ptr,
            rc: Rc::downgrade(&rc),
        });

        DynamicRoot {
            ptr: unsafe {
                mem::transmute::<Gc<'gc, Root<'gc, R>>, Gc<'static, Root<'static, R>>>(root)
            },
            id: GcCell::as_ptr(self.0) as *const (),
            _rc: rc,
        }
    }

    pub fn fetch<R: for<'a> Rootable<'a> + ?Sized>(
        &self,
        root: &DynamicRoot<R>,
    ) -> Gc<'gc, Root<'gc, R>> {
        assert_eq!(
            GcCell::as_ptr(self.0) as *const (),
            root.id,
            "provided `DynamicRoot` does not come from the `DynamicRootSet` it was provided to"
        );

        unsafe { mem::transmute::<Gc<'static, Root<'static, R>>, Gc<'gc, Root<'gc, R>>>(root.ptr) }
    }
}
