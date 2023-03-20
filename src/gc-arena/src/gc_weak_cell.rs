use crate::types::GcBox;
use crate::GcCell;
use crate::{collect::Collect, MutationContext};

use core::fmt::{self, Debug};

pub struct GcWeakCell<'gc, T: ?Sized + 'gc> {
    pub(crate) inner: GcCell<'gc, T>,
}

impl<'gc, T: ?Sized + 'gc> Copy for GcWeakCell<'gc, T> {}

impl<'gc, T: ?Sized + 'gc> Clone for GcWeakCell<'gc, T> {
    fn clone(&self) -> GcWeakCell<'gc, T> {
        *self
    }
}

impl<'gc, T: ?Sized + 'gc> Debug for GcWeakCell<'gc, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "(GcWeakCell)")
    }
}

unsafe impl<'gc, T: ?Sized + 'gc> Collect for GcWeakCell<'gc, T> {
    fn trace(&self, cc: crate::CollectionContext) {
        unsafe {
            cc.trace_weak(GcBox::erase(self.inner.0.ptr));
        }
    }
}

impl<'gc, T: ?Sized + 'gc> GcWeakCell<'gc, T> {
    pub fn upgrade(&self, mc: MutationContext<'gc, '_>) -> Option<GcCell<'gc, T>> {
        unsafe {
            let ptr = GcBox::erase(self.inner.0.ptr);
            mc.upgrade(ptr).then(|| self.inner)
        }
    }

    /// Returns whether the value referenced by this `GcWeakCell` has been dropped.
    ///
    /// Note that calling `upgrade` may still fail even when this method returns `false`.
    pub fn is_dropped(self) -> bool {
        let ptr = unsafe { GcBox::erase(self.inner.0.ptr) };
        !ptr.header().is_live()
    }

    pub fn ptr_eq(this: GcWeakCell<'gc, T>, other: GcWeakCell<'gc, T>) -> bool {
        this.as_ptr() == other.as_ptr()
    }

    pub fn as_ptr(self) -> *mut T {
        self.inner.as_ptr()
    }
}
