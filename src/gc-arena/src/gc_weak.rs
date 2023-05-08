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
    fn trace(&self, cc: CollectionContext) {
        unsafe {
            cc.trace_weak(GcBox::erase(self.inner.ptr));
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

    /// Returns whether the value referenced by this `GcWeak` has been dropped.
    ///
    /// Note that calling `upgrade` may still fail even when this method returns `false`.
    pub fn is_dropped(self) -> bool {
        let ptr = unsafe { GcBox::erase(self.inner.ptr) };
        !ptr.header().is_live()
    }

    pub fn ptr_eq(this: GcWeak<'gc, T>, other: GcWeak<'gc, T>) -> bool {
        this.as_ptr() == other.as_ptr()
    }

    pub fn as_ptr(self) -> *const T {
        Gc::as_ptr(self.inner)
    }
}

impl<'gc, T: Collect + 'gc> GcWeak<'gc, T> {
    /// Cast the internal pointer to a different type.
    ///
    /// SAFETY:
    /// It must be valid to dereference a `*mut U` that has come from casting a `*mut T`.
    pub unsafe fn cast<U>(this: GcWeak<'gc, T>) -> GcWeak<'gc, U> {
        GcWeak {
            inner: Gc::cast::<U>(this.inner),
        }
    }
}
