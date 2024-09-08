use core::{
    alloc::Layout,
    borrow::Borrow,
    fmt::{self, Debug, Display, Pointer},
    hash::{Hash, Hasher},
    marker::PhantomData,
    ops::Deref,
    ptr::NonNull,
};

use crate::{
    barrier::{Unlock, Write},
    collect::Collect,
    context::{Collection, Mutation},
    gc_weak::GcWeak,
    static_collect::Static,
    types::{GcBox, GcBoxHeader, GcBoxInner, GcColor, Invariant},
    Finalization,
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

impl<'gc, T: ?Sized + 'gc> Borrow<T> for Gc<'gc, T> {
    #[inline]
    fn borrow(&self) -> &T {
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

impl<'gc, T: 'static> Gc<'gc, T> {
    /// Create a new `Gc` pointer from a static value.
    ///
    /// This method does not require that the type `T` implement `Collect`. This uses [`Static`]
    /// internally to automatically provide a trivial `Collect` impl and is equivalent to the
    /// following code:
    ///
    /// ```rust
    /// # use gc_arena::{Gc, Static};
    /// # fn main() {
    /// # gc_arena::arena::rootless_mutate(|mc| {
    /// struct MyStaticStruct;
    /// let p = Gc::new(mc, Static(MyStaticStruct));
    /// // This is allowed because `Static` is `#[repr(transparent)]`
    /// let p: Gc<MyStaticStruct> = unsafe { Gc::cast(p) };
    /// # });
    /// # }
    /// ```
    #[inline]
    pub fn new_static(mc: &Mutation<'gc>, t: T) -> Gc<'gc, T> {
        let p = Gc::new(mc, Static(t));
        // SAFETY: `Static` is `#[repr(transparent)]`.
        unsafe { Gc::cast::<T>(p) }
    }
}

impl<'gc, T: ?Sized + 'gc> Gc<'gc, T> {
    /// Cast a `Gc` pointer to a different type.
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

    /// Cast a `Gc` to the unit type.
    ///
    /// This is exactly the same as `unsafe { Gc::cast::<()>(this) }`, but we can provide this
    /// method safely because it is always safe to dereference a `*mut ()` that has come from
    /// casting a `*mut T`.
    #[inline]
    pub fn erase(this: Gc<'gc, T>) -> Gc<'gc, ()> {
        unsafe { Gc::cast(this) }
    }

    /// Retrieve a `Gc` from a raw pointer obtained from `Gc::as_ptr`
    ///
    /// SAFETY:
    /// The provided pointer must have been obtained from `Gc::as_ptr`, and the pointer must not
    /// have been collected yet.
    #[inline]
    pub unsafe fn from_ptr(ptr: *const T) -> Gc<'gc, T> {
        let layout = Layout::new::<GcBoxHeader>();
        let (_, header_offset) = layout.extend(Layout::for_value(&*ptr)).unwrap();
        let header_offset = -(header_offset as isize);
        let ptr = (ptr as *mut T).byte_offset(header_offset) as *mut GcBoxInner<T>;
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

    /// Triggers a write barrier on this `Gc`, allowing for safe mutation.
    ///
    /// This triggers an unrestricted *backwards* write barrier on this pointer, meaning that it is
    /// guaranteed that this pointer can safely adopt *any* arbitrary child pointers (until the next
    /// time that collection is triggered).
    ///
    /// It returns a reference to the inner `T` wrapped in a `Write` marker to allow for
    /// unrestricted mutation on the held type or any of its directly held fields.
    #[inline]
    pub fn write(mc: &Mutation<'gc>, gc: Self) -> &'gc Write<T> {
        unsafe {
            mc.backward_barrier(Gc::erase(gc), None);
            // SAFETY: the write barrier stays valid until the end of the current callback.
            Write::assume(gc.as_ref())
        }
    }

    /// Returns true if two `Gc`s point to the same allocation.
    ///
    /// Similarly to `Rc::ptr_eq` and `Arc::ptr_eq`, this function ignores the metadata of `dyn`
    /// pointers.
    #[inline]
    pub fn ptr_eq(this: Gc<'gc, T>, other: Gc<'gc, T>) -> bool {
        // TODO: Equivalent to `core::ptr::addr_eq`:
        // https://github.com/rust-lang/rust/issues/116324
        Gc::as_ptr(this) as *const () == Gc::as_ptr(other) as *const ()
    }

    #[inline]
    pub fn as_ptr(gc: Gc<'gc, T>) -> *const T {
        unsafe {
            let inner = gc.ptr.as_ptr();
            core::ptr::addr_of!((*inner).value) as *const T
        }
    }

    /// Returns true when a pointer is *dead* during finalization. This is equivalent to
    /// `GcWeak::is_dead` for strong pointers.
    ///
    /// Any strong pointer reachable from the root will never be dead, BUT there can be strong
    /// pointers reachable only through other weak pointers that can be dead.
    #[inline]
    pub fn is_dead(_: &Finalization<'gc>, gc: Gc<'gc, T>) -> bool {
        let inner = unsafe { gc.ptr.as_ref() };
        matches!(inner.header.color(), GcColor::White | GcColor::WhiteWeak)
    }

    /// Manually marks a dead `Gc` pointer as reachable and keeps it alive.
    ///
    /// Equivalent to `GcWeak::resurrect` for strong pointers. Manually marks this pointer and
    /// all transitively held pointers as reachable, thus keeping them from being dropped this
    /// collection cycle.
    #[inline]
    pub fn resurrect(fc: &Finalization<'gc>, gc: Gc<'gc, T>) {
        unsafe {
            fc.resurrect(GcBox::erase(gc.ptr));
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
