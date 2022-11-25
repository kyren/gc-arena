use alloc::{
    rc::{Rc, Weak},
    vec::Vec,
};
use core::mem;

use crate::{
    cell::{Mutable, RefCell},
    types::GcBox,
    Collect, Gc, MutationContext, Root, Rootable,
};

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
pub struct DynamicRootSet<'gc>(Gc<'gc, Inner>);

unsafe impl<'gc> Collect for DynamicRootSet<'gc> {
    fn trace(&self, cc: crate::CollectionContext) {
        self.0.trace(cc);
    }
}

impl<'gc> DynamicRootSet<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>) -> Self {
        DynamicRootSet(Gc::allocate(
            mc,
            Inner {
                handles: RefCell::new(Vec::new()),
                set_id: Rc::new(SetId {}),
            },
        ))
    }

    pub fn stash<R: for<'a> Rootable<'a> + ?Sized>(
        &self,
        mc: MutationContext<'gc, '_>,
        root: Gc<'gc, Root<'gc, R>>,
    ) -> DynamicRoot<R> {
        Gc::write_barrier(mc, self.0);
        // SAFETY: we just called the write barrier above.
        let handles = unsafe { Mutable::assume(&self.0.handles) };

        let rc = Rc::new(self.0.set_id.clone());
        handles.write().push(Handle {
            ptr: unsafe { GcBox::erase(root.ptr) },
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
            Rc::as_ptr(&self.0.set_id),
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
    handles: RefCell<Vec<Handle>>,
    set_id: Rc<SetId>,
}

unsafe impl<'gc> Collect for Inner {
    fn trace(&self, cc: crate::CollectionContext) {
        // SAFETY: We do not adopt any new pointers so we don't need a write barrier.
        let mut handles = unsafe { Mutable::assume(&self.handles).write() };

        // We cheat horribly and filter out dead handles during tracing. Since we have to go
        // through the entire list of roots anyway, this is cheaper than filtering on e.g.
        // stashing new roots.
        handles.retain(|handle| {
            let is_live = Weak::strong_count(&handle.rc) > 0;
            if is_live {
                unsafe { cc.trace(handle.ptr) }
            }
            is_live
        });
    }
}

struct Handle {
    ptr: GcBox,
    rc: Weak<Rc<SetId>>,
}
