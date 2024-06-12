use crate::collect::Collect;
use crate::context::Finalization;
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
        let ptr = unsafe { GcBox::erase(self.inner.ptr) };
        mc.upgrade(ptr).then(|| self.inner)
    }

    /// During collection, return whether the value referenced by this `GcWeak` has already been
    /// dropped.
    #[inline]
    pub fn is_dropped(self, _cc: &Collection) -> bool {
        !unsafe { self.inner.ptr.as_ref() }.header.is_live()
    }

    /// Returns true when a pointer is *dead* during finalization.
    ///
    /// This is a weaker condition than being *dropped*, as the pointer *may* still be valid. Being
    /// *dead* means that there were no strong pointers pointing to this weak pointer that were
    /// found by the marking phase, and if it is not already dropped, it *will* be dropped as soon
    /// as collection resumes.
    ///
    /// If the pointer is still valid, it may be resurrected using `GcWeak::upgrade` or
    /// `GcWeak::resurrect`.
    ///
    /// NOTE: This returns true if the pointer was destined to be collected at the **start** of the
    /// current finalization callback. Resurrecting one pointer can transitively resurrect others,
    /// and this method does not reflect this from within the same finalization call! If transitive
    /// resurrection is important, you may have to carefully call finalize multiple times for one
    /// collection cycle with marking stages in-between, and in the precise order that you want.
    #[inline]
    pub fn is_dead(self, fc: &Finalization<'gc>) -> bool {
        Gc::is_dead(fc, self.inner)
    }

    /// Manually marks a dead (but non-dropped) `GcWeak` as strongly reachable and keeps it alive.
    ///
    /// This is similar to a write barrier in that it moves the collection phase back to `Marking`
    /// if it is not already there. All transitively held pointers from this will also be marked as
    /// reachable once marking resumes.
    ///
    /// Returns the upgraded `Gc` pointer as a convenience. Whether or not the strong pointer is
    /// stored anywhere, the value and all transitively reachable values are still guaranteed to not
    /// be dropped this collection cycle.
    #[inline]
    pub fn resurrect(self, fc: &Finalization<'gc>) -> Option<Gc<'gc, T>> {
        // SAFETY: We know that we are currently marking, so any non-dropped pointer is safe to
        // resurrect.
        if unsafe { self.inner.ptr.as_ref() }.header.is_live() {
            Gc::resurrect(fc, self.inner);
            Some(self.inner)
        } else {
            None
        }
    }

    /// Returns true if two `GcWeak`s point to the same allocation.
    ///
    /// Similarly to `Rc::ptr_eq` and `Arc::ptr_eq`, this function ignores the metadata of `dyn`
    /// pointers.
    #[inline]
    pub fn ptr_eq(this: GcWeak<'gc, T>, other: GcWeak<'gc, T>) -> bool {
        // TODO: Equivalent to `core::ptr::addr_eq`:
        // https://github.com/rust-lang/rust/issues/116324
        this.as_ptr() as *const () == other.as_ptr() as *const ()
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
    /// the pointer must not have been *fully* collected yet (it may be a dropped but valid weak
    /// pointer).
    #[inline]
    pub unsafe fn from_ptr(ptr: *const T) -> GcWeak<'gc, T> {
        GcWeak {
            inner: Gc::from_ptr(ptr),
        }
    }
}
