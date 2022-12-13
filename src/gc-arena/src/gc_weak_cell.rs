use crate::types::GcBox;
use crate::GcCell;
use crate::{collect::Collect, MutationContext};

use core::fmt::{self, Debug};

pub struct GcWeakCell<'gc, T: 'gc + Collect> {
    pub(crate) inner: GcCell<'gc, T>,
}

impl<'gc, T: Collect + 'gc> Copy for GcWeakCell<'gc, T> {}

impl<'gc, T: Collect + 'gc> Clone for GcWeakCell<'gc, T> {
    fn clone(&self) -> GcWeakCell<'gc, T> {
        *self
    }
}

impl<'gc, T: 'gc + Collect> Debug for GcWeakCell<'gc, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "(GcWeakCell)")
    }
}

unsafe impl<'gc, T: 'gc + Collect> Collect for GcWeakCell<'gc, T> {
    fn trace(&self, _cc: crate::CollectionContext) {
        unsafe {
            let gc = GcBox::erase(self.inner.0.ptr);
            gc.flags().set_traced_weak_ref(true);
        }
    }
}

impl<'gc, T: Collect + 'gc> GcWeakCell<'gc, T> {
    pub fn upgrade(&self, mc: MutationContext<'gc, '_>) -> Option<GcCell<'gc, T>> {
        unsafe {
            let ptr = GcBox::erase(self.inner.0.ptr);
            mc.upgrade(ptr).then(|| self.inner)
        }
    }
}
