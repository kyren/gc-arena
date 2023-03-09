use crate::collect::Collect;
use crate::gc::Gc;
use crate::types::GcBox;
use crate::{CollectionContext, MutationContext};

use core::fmt::{self, Debug};

pub struct GcWeak<'gc, T: ?Sized + 'gc> {
    pub(crate) inner: Gc<'gc, T>,
}

impl<'gc, T: ?Sized + 'gc> Copy for GcWeak<'gc, T> {}

impl<'gc, T: ?Sized + 'gc> Clone for GcWeak<'gc, T> {
    fn clone(&self) -> GcWeak<'gc, T> {
        *self
    }
}

impl<'gc, T: ?Sized + 'gc> Debug for GcWeak<'gc, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "(GcWeak)")
    }
}

unsafe impl<'gc, T: ?Sized + 'gc> Collect for GcWeak<'gc, T> {
    fn trace(&self, _cc: CollectionContext) {
        unsafe {
            let gc = GcBox::erase(self.inner.ptr);
            gc.header().set_traced_weak_ref(true);
        }
    }
}

impl<'gc, T: ?Sized + 'gc> GcWeak<'gc, T> {
    pub fn upgrade(&self, mc: MutationContext<'gc, '_>) -> Option<Gc<'gc, T>> {
        unsafe {
            let ptr = GcBox::erase(self.inner.ptr);
            mc.upgrade(ptr).then(|| self.inner)
        }
    }

    pub fn ptr_eq(this: GcWeak<'gc, T>, other: GcWeak<'gc, T>) -> bool {
        this.as_ptr() == other.as_ptr()
    }

    pub fn as_ptr(self) -> *const T {
        Gc::as_ptr(self.inner)
    }
}
