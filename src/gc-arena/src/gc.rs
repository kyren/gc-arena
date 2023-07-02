use core::{
    fmt::{self, Debug, Display, Pointer},
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem,
    ops::Deref,
    ptr::{self, NonNull},
};

use crate::{
    barrier::{Unlock, Write},
    collect::Collect,
    context::{Collection, Mutation},
    gc_weak::GcWeak,
    types::{GcBox, GcBoxInner, Invariant},
};

/// A garbage collected pointer to a type T. Implements Copy, and is implemented as a plain machine
/// pointer. You can only allocate `Gc` pointers through a `&Mutation<'gc>` inside an arena type,
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
    #[inline]
    fn clone(&self) -> Gc<'gc, T> {
        *self
    }
}

unsafe impl<'gc, T: ?Sized + 'gc> Collect for Gc<'gc, T> {
    #[inline]
    fn trace(&self, cc: &Collection) {
        unsafe {
            cc.trace(GcBox::erase(self.ptr));
        }
    }
}

impl<'gc, T: ?Sized + 'gc> Deref for Gc<'gc, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        unsafe { &self.ptr.as_ref().value }
    }
}

impl<'gc, T: ?Sized + 'gc> AsRef<T> for Gc<'gc, T> {
    #[inline]
    fn as_ref(&self) -> &T {
        unsafe { &self.ptr.as_ref().value }
    }
}

impl<'gc, T: Collect + 'gc> Gc<'gc, T> {
    #[inline]
    pub fn new(mc: &Mutation<'gc>, t: T) -> Gc<'gc, T> {
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
    #[inline]
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
    #[inline]
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
    /// Shorthand for [`Gc::write`]`(mc, self).`[`unlock()`](Write::unlock).
    #[inline]
    pub fn unlock(self, mc: &Mutation<'gc>) -> &'gc T::Unlocked {
        Gc::write(mc, self);
        // SAFETY: see doc-comment.
        unsafe { self.as_ref().unlock_unchecked() }
    }
}

impl<'gc, T: ?Sized + 'gc> Gc<'gc, T> {
    /// Obtains a long-lived reference to the contents of this `Gc`.
    ///
    /// Unlike `AsRef` or `Deref`, the returned reference isn't bound to the `Gc` itself, and
    /// will stay valid for the entirety of the current arena callback.
    #[inline]
    pub fn as_ref(self: Gc<'gc, T>) -> &'gc T {
        // SAFETY: The returned reference cannot escape the current arena callback, as `&'gc T`
        // never implements `Collect` (unless `'gc` is `'static`, which is impossible here), and
        // so cannot be stored inside the GC root.
        unsafe { &self.ptr.as_ref().value }
    }

    #[inline]
    pub fn downgrade(this: Gc<'gc, T>) -> GcWeak<'gc, T> {
        GcWeak { inner: this }
    }

    /// Triggers a write barrier on this `Gc`, allowing for further safe mutation.
    #[inline]
    pub fn write(mc: &Mutation<'gc>, gc: Self) -> &'gc Write<T> {
        unsafe {
            mc.write_barrier(GcBox::erase(gc.ptr));
            // SAFETY: the write barrier stays valid until the end of the current callback.
            Write::assume(gc.as_ref())
        }
    }

    #[inline]
    pub fn ptr_eq(this: Gc<'gc, T>, other: Gc<'gc, T>) -> bool {
        Gc::as_ptr(this) == Gc::as_ptr(other)
    }

    #[inline]
    pub fn as_ptr(gc: Gc<'gc, T>) -> *const T {
        unsafe {
            let inner = gc.ptr.as_ptr();
            core::ptr::addr_of!((*inner).value) as *const T
        }
    }
}

impl<'gc, T: PartialEq + ?Sized + 'gc> PartialEq for Gc<'gc, T> {
    fn eq(&self, other: &Self) -> bool {
        (**self).eq(other)
    }

    fn ne(&self, other: &Self) -> bool {
        (**self).ne(other)
    }
}

impl<'gc, T: Eq + ?Sized + 'gc> Eq for Gc<'gc, T> {}

impl<'gc, T: PartialOrd + ?Sized + 'gc> PartialOrd for Gc<'gc, T> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        (**self).partial_cmp(other)
    }

    fn le(&self, other: &Self) -> bool {
        (**self).le(other)
    }

    fn lt(&self, other: &Self) -> bool {
        (**self).lt(other)
    }

    fn ge(&self, other: &Self) -> bool {
        (**self).ge(other)
    }

    fn gt(&self, other: &Self) -> bool {
        (**self).gt(other)
    }
}

impl<'gc, T: Ord + ?Sized + 'gc> Ord for Gc<'gc, T> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        (**self).cmp(other)
    }
}

impl<'gc, T: Hash + ?Sized + 'gc> Hash for Gc<'gc, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state)
    }
}
