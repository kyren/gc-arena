use alloc::{
    rc::{Rc, Weak},
    vec::Vec,
};
use core::mem;

use crate::{types::GcBoxPtr, Collect, Gc, GcCell, MutationContext, Root, Rootable};

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
pub struct DynamicRootSet<'gc>(GcCell<'gc, Inner>);

unsafe impl<'gc> Collect for DynamicRootSet<'gc> {
    fn trace(&self, cc: crate::CollectionContext) {
        // SAFETY: We do not adopt any new pointers so we don't need a write barrier.
        unsafe {
            // We cheat horribly and filter out dead handles during tracing. Since we have to go
            // through the entire list of roots anyway, this is cheaper than filtering on e.g.
            // stashing new roots.
            self.0
                .borrow_mut()
                .handles
                .retain(|handle| Weak::strong_count(&handle.rc) > 0);
        }

        self.0.trace(cc);
    }
}

impl<'gc> DynamicRootSet<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>) -> Self {
        DynamicRootSet(GcCell::allocate(
            mc,
            Inner {
                handles: Vec::new(),
                set_id: Rc::new(SetId {}),
            },
        ))
    }

    pub fn stash<R: for<'a> Rootable<'a> + ?Sized>(
        &self,
        mc: MutationContext<'gc, '_>,
        root: Gc<'gc, Root<'gc, R>>,
    ) -> DynamicRoot<R> {
        let mut inner = self.0.write(mc);

        let rc = Rc::new(inner.set_id.clone());
        inner.handles.push(Handle {
            ptr: unsafe { GcBoxPtr::erase(root.ptr) },
            rc: Rc::downgrade(&rc),
        });

        DynamicRoot {
            ptr: unsafe {
                mem::transmute::<Gc<'gc, Root<'gc, R>>, Gc<'static, Root<'static, R>>>(root)
            },
            rc,
        }
    }

    pub fn fetch<R: for<'a> Rootable<'a> + ?Sized>(
        &self,
        root: &DynamicRoot<R>,
    ) -> Gc<'gc, Root<'gc, R>> {
        assert_eq!(
            Rc::as_ptr(&self.0.read().set_id),
            Rc::as_ptr(root.rc.as_ref()),
            "provided `DynamicRoot` does not originate from this `DynamicRootSet`",
        );

        unsafe { mem::transmute::<Gc<'static, Root<'static, R>>, Gc<'gc, Root<'gc, R>>>(root.ptr) }
    }
}

#[derive(Clone)]
pub struct DynamicRoot<R: for<'gc> Rootable<'gc> + ?Sized> {
    ptr: Gc<'static, Root<'static, R>>,
    // We identify dropped handles by checking an `Rc` handle count. We store a clone of the
    // `Rc<SetId>` so that we ensure the `Rc<SetId>` lives as long as any extant handle.
    rc: Rc<Rc<SetId>>,
}

// The address of an allocated `SetId` type uniquely identifies a single `DynamicRootSet`.
struct SetId {}

struct Inner {
    handles: Vec<Handle>,
    set_id: Rc<SetId>,
}

unsafe impl<'gc> Collect for Inner {
    fn trace(&self, cc: crate::CollectionContext) {
        for handle in &self.handles {
            unsafe {
                cc.trace(handle.ptr);
            }
        }
    }
}

struct Handle {
    ptr: GcBoxPtr,
    rc: Weak<Rc<SetId>>,
}
