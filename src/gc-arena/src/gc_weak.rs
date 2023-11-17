use crate::collect::Collect;
use crate::gc::Gc;
use crate::types::GcBox;
use crate::{Collection, Mutation};

use core::fmt::{self, Debug};

pub struct GcWeak<'gc, T: ?Sized + 'gc> {
    pub(crate) inner: Gc<'gc, T>,
}

impl<'gc, T: ?Sized + 'gc> Copy for GcWeak<'gc, T> {}

impl<'gc, T: ?Sized + 'gc> Clone for GcWeak<'gc, T> {
    #[inline]
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
    #[inline]
    fn trace(&self, cc: &Collection) {
        unsafe {
            cc.trace_weak(GcBox::erase(self.inner.ptr));
        }
    }
}

impl<'gc, T: ?Sized + 'gc> GcWeak<'gc, T> {
    #[inline]
    pub fn upgrade(self, mc: &Mutation<'gc>) -> Option<Gc<'gc, T>> {
        unsafe {
            let ptr = GcBox::erase(self.inner.ptr);
            mc.upgrade(ptr).then(|| self.inner)
        }
    }

    /// Returns whether the value referenced by this `GcWeak` has been dropped.
    ///
    /// Note that calling `upgrade` may still fail even when this method returns `false`.
    #[inline]
    pub fn is_dropped(self) -> bool {
        !unsafe { self.inner.ptr.as_ref() }.header.is_live()
    }

    #[inline]
    pub fn ptr_eq(this: GcWeak<'gc, T>, other: GcWeak<'gc, T>) -> bool {
        this.as_ptr() == other.as_ptr()
    }

    #[inline]
    pub fn as_ptr(self) -> *const T {
        Gc::as_ptr(self.inner)
    }
}

impl<'gc, T: 'gc> GcWeak<'gc, T> {
    /// Cast the internal pointer to a different type.
    ///
    /// SAFETY:
    /// It must be valid to dereference a `*mut U` that has come from casting a `*mut T`.
    #[inline]
    pub unsafe fn cast<U: 'gc>(this: GcWeak<'gc, T>) -> GcWeak<'gc, U> {
        GcWeak {
            inner: Gc::cast::<U>(this.inner),
        }
    }

    /// Retrieve a `GcWeak` from a raw pointer obtained from `GcWeak::as_ptr`
    ///
    /// SAFETY:
    /// The provided pointer must have been obtained from `GcWeak::as_ptr` or `Gc::as_ptr`, and
    /// the pointer must not have been *fully* collected yet (it may be a dropped but live weak
    /// pointer).
    #[inline]
    pub unsafe fn from_ptr(ptr: *const T) -> GcWeak<'gc, T> {
        GcWeak {
            inner: Gc::from_ptr(ptr),
        }
    }
}
