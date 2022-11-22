use crate::GcCell;
use crate::{collect::Collect, MutationContext};

use core::fmt::{self, Debug};

pub struct GcWeakCell<'gc, T> {
    pub(crate) inner: GcCell<'gc, T>,
}

impl<'gc, T> Copy for GcWeakCell<'gc, T> {}

impl<'gc, T> Clone for GcWeakCell<'gc, T> {
    fn clone(&self) -> GcWeakCell<'gc, T> {
        *self
    }
}

impl<'gc, T> Debug for GcWeakCell<'gc, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "(GcWeakCell)")
    }
}

unsafe impl<'gc, T: 'gc + Collect> Collect for GcWeakCell<'gc, T> {
    fn trace(&self, _cc: crate::CollectionContext) {
        unsafe {
            let gc = self.inner.get_inner().ptr.as_ref();

            gc.flags.set_traced_weak_ref(true);
        }
    }
}

impl<'gc, T: Collect + 'gc> GcWeakCell<'gc, T> {
    pub fn upgrade(&self, mc: MutationContext<'gc, '_>) -> Option<GcCell<'gc, T>> {
        unsafe { mc.upgrade(self.inner.get_inner().ptr).then(|| self.inner) }
    }
}
