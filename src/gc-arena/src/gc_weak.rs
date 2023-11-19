use crate::collect::Collect;
use crate::gc::Gc;
use crate::types::{GcBox, GcColor};
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
        let ptr = unsafe { GcBox::erase(self.inner.ptr) };
        mc.upgrade(ptr).then(|| self.inner)
    }

    /// Returns whether the value referenced by this `GcWeak` has been dropped.
    ///
    /// Note that calling `upgrade` may still fail even when this method returns `false`.
    #[inline]
    pub fn is_dropped(self) -> bool {
        !unsafe { self.inner.ptr.as_ref() }.header.is_live()
    }

    /// Returns true if this pointer has been marked during this collection cycle and the held value
    /// will not be dropped.
    ///
    /// If this method returns `false`, it does not necessarily imply that the held value *will* be
    /// dropped this cycle, only that it hasn't been marked *yet* as part of the `Marking` phase.
    ///
    /// This method is most useful when called when the arena is precisely in the `Marked` state. IF
    /// the arena is fully marked AND this method returns `false` AND the arena does not transition
    /// back to `Marking` due to a write barrier or `GcWeak::mark`, then you can know that the
    /// object pointed to by this `GcWeak` is destined to be dropped during the current cycle's
    /// `Collecting` phase.
    #[inline]
    pub fn is_marked(self) -> bool {
        matches!(
            unsafe { self.inner.ptr.as_ref() }.header.color(),
            GcColor::Gray | GcColor::Black
        )
    }

    /// Manually mark a `GcWeak` during marking.
    ///
    /// If the pointer can be manually marked, marks it and returns a strong version of the pointer,
    /// otherwise `None`.
    ///
    /// May return `None` if the arena is in the wrong phase of the collection cycle *or* if the
    /// pointed-to value has already been dropped.
    ///
    /// Will mark pointers if called in the `Marking` *or* the `Marked` phase, but if called in the
    /// `Marked` phase *and* the pointer was not already marked, it may transition the collection
    /// phase back to `Marking` (for transitively held objects).
    #[inline]
    pub fn mark(self, mc: &Mutation<'gc>) -> Option<Gc<'gc, T>> {
        let ptr = unsafe { GcBox::erase(self.inner.ptr) };
        mc.mark(ptr).then(|| self.inner)
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
