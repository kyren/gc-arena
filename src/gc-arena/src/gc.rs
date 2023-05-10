use core::{
    fmt::{self, Debug, Display, Pointer},
    marker::PhantomData,
    mem,
    ops::Deref,
    ptr::{self, NonNull},
};

use crate::{
    collect::Collect,
    context::{CollectionContext, MutationContext},
    gc_weak::GcWeak,
    lock::Unlock,
    types::{GcBox, GcBoxInner, Invariant},
};

/// A garbage collected pointer to a type T. Implements Copy, and is implemented as a plain machine
/// pointer. You can only allocate `Gc` pointers through a `MutationContext` inside an arena type,
/// and through "generativity" such `Gc` pointers may not escape the arena they were born in or
/// be stored inside TLS. This, combined with correct `Collect` implementations, means that `Gc`
/// pointers will never be dangling and are always safe to access.
pub struct Gc<'gc, T: ?Sized + 'gc> {
    pub(crate) ptr: NonNull<GcBoxInner<T>>,
    pub(crate) _invariant: Invariant<'gc>,
}

impl<'gc, T: Debug + ?Sized + 'gc> Debug for Gc<'gc, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&**self, fmt)
    }
}

impl<'gc, T: ?Sized + 'gc> Pointer for Gc<'gc, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Pointer::fmt(&Gc::as_ptr(*self), fmt)
    }
}

impl<'gc, T: Display + ?Sized + 'gc> Display for Gc<'gc, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&**self, fmt)
    }
}

impl<'gc, T: ?Sized + 'gc> Copy for Gc<'gc, T> {}

impl<'gc, T: ?Sized + 'gc> Clone for Gc<'gc, T> {
    fn clone(&self) -> Gc<'gc, T> {
        *self
    }
}

unsafe impl<'gc, T: ?Sized + 'gc> Collect for Gc<'gc, T> {
    fn trace(&self, cc: CollectionContext) {
        unsafe {
            cc.trace(GcBox::erase(self.ptr));
        }
    }
}

impl<'gc, T: ?Sized + 'gc> Deref for Gc<'gc, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &self.ptr.as_ref().value }
    }
}

impl<'gc, T: Collect + 'gc> Gc<'gc, T> {
    pub fn new(mc: MutationContext<'gc, '_>, t: T) -> Gc<'gc, T> {
        Gc {
            ptr: mc.allocate(t),
            _invariant: PhantomData,
        }
    }
}

impl<'gc, T: 'gc> Gc<'gc, T> {
    /// Cast the internal pointer to a different type.
    ///
    /// SAFETY:
    /// It must be valid to dereference a `*mut U` that has come from casting a `*mut T`.
    pub unsafe fn cast<U: 'gc>(this: Gc<'gc, T>) -> Gc<'gc, U> {
        Gc {
            ptr: NonNull::cast(this.ptr),
            _invariant: PhantomData,
        }
    }

    /// Retrieve a `Gc` from a raw pointer obtained from `Gc::as_ptr`
    ///
    /// SAFETY:
    /// The provided pointer must have been obtained from `Gc::as_ptr`, and the pointer must not
    /// have been collected yet.
    pub unsafe fn from_ptr(ptr: *const T) -> Gc<'gc, T> {
        let header_offset = {
            let base = mem::MaybeUninit::<GcBoxInner<T>>::uninit();
            let base_ptr = base.as_ptr();
            let val_ptr = ptr::addr_of!((*base_ptr).value);
            (base_ptr as isize) - (val_ptr as isize)
        };
        let ptr = (ptr as *mut T)
            .cast::<u8>()
            .offset(header_offset)
            .cast::<GcBoxInner<T>>();
        Gc {
            ptr: NonNull::new_unchecked(ptr),
            _invariant: PhantomData,
        }
    }
}

impl<'gc, T: Unlock + ?Sized + 'gc> Gc<'gc, T> {
    /// Unlock the contents of this `Gc` safely by ensuring that the write barrier is called.
    pub fn unlock(&self, mc: MutationContext<'gc, '_>) -> &T::Unlocked {
        Gc::write_barrier(mc, *self);
        // SAFETY: see doc-comment.
        unsafe { self.unlock_unchecked() }
    }
}

impl<'gc, T: ?Sized + 'gc> Gc<'gc, T> {
    pub fn downgrade(this: Gc<'gc, T>) -> GcWeak<'gc, T> {
        GcWeak { inner: this }
    }

    /// When implementing `Collect` on types with internal mutability containing `Gc` pointers, this
    /// method must be used to ensure safe mutability. Safe to call, but only necessary from unsafe
    /// code.
    pub fn write_barrier(mc: MutationContext<'gc, '_>, gc: Self) {
        unsafe {
            mc.write_barrier(GcBox::erase(gc.ptr));
        }
    }

    pub fn ptr_eq(this: Gc<'gc, T>, other: Gc<'gc, T>) -> bool {
        Gc::as_ptr(this) == Gc::as_ptr(other)
    }

    pub fn as_ptr(gc: Gc<'gc, T>) -> *const T {
        unsafe {
            let inner = gc.ptr.as_ptr();
            core::ptr::addr_of!((*inner).value) as *const T
        }
    }
}
