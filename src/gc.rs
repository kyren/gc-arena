use core::{
    borrow::Borrow,
    fmt::{self, Debug, Display, Pointer},
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem::{self, MaybeUninit},
    ops::Deref,
    ptr,
};

use crate::{
    barrier::{Unlock, Write},
    collect::{Collect, Trace},
    context::{Finalization, Mutation},
    gc_ptr::GcPtr,
    gc_weak::GcWeak,
    header_slice::HeaderSlice,
    static_wrapper::Static,
    types::{GcColor, Invariant},
};

/// A garbage collected pointer to a type T.
///
/// This type is `Copy` and is implemented as a plain machine pointer to `T`.
///
/// You can only allocate `Gc` pointers through a `&Mutation<'gc>` inside an arena type, and through
/// "generativity" such `Gc` pointers may not escape the arena they were born in or be stored inside
/// TLS. This, combined with correct `Collect` implementations, means that `Gc` pointers will never
/// be dangling and are always safe to access.
pub struct Gc<'gc, T: ?Sized + 'gc> {
    pub(crate) ptr: GcPtr<T>,
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

unsafe impl<'gc, T: ?Sized + 'gc> Collect<'gc> for Gc<'gc, T> {
    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        cc.trace_gc(Self::erase(*self))
    }
}

impl<'gc, T: ?Sized + 'gc> Deref for Gc<'gc, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<'gc, T: ?Sized + 'gc> AsRef<T> for Gc<'gc, T> {
    #[inline]
    fn as_ref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<'gc, T: ?Sized + 'gc> Borrow<T> for Gc<'gc, T> {
    #[inline]
    fn borrow(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<'gc, T: Collect<'gc> + 'gc> Gc<'gc, T> {
    #[inline]
    pub fn new(mc: &Mutation<'gc>, t: T) -> Gc<'gc, T> {
        let gc_ptr = mc.allocate::<T>();
        unsafe { gc_ptr.as_ptr().write(t) };
        gc_ptr.header().set_live(true);
        Gc {
            ptr: gc_ptr,
            _invariant: PhantomData,
        }
    }
}

impl<'gc, H: Collect<'gc> + 'gc, E: Collect<'gc> + 'gc> Gc<'gc, HeaderSlice<H, E>> {
    /// Allocate a new [`HeaderSlice`] with the given `header` and elements created from
    /// `create_element`.
    #[inline]
    pub fn new_header_slice_with(
        mc: &Mutation<'gc>,
        header: H,
        len: usize,
        mut create_element: impl FnMut(usize) -> E,
    ) -> Self {
        let gc_ptr = mc.allocate_slice::<H, E>(len);

        unsafe {
            ptr::write(&raw mut (*gc_ptr.as_ptr()).header, header);

            struct DropGuard<H, E> {
                gc_ptr: GcPtr<HeaderSlice<H, E>>,
                init_length: usize,
            }

            impl<H, E> Drop for DropGuard<H, E> {
                fn drop(&mut self) {
                    unsafe {
                        let ptr = HeaderSlice::to_thin_ptr(self.gc_ptr.as_ptr());
                        let ptr = HeaderSlice::<H, E>::from_thin_ptr(ptr, self.init_length);
                        core::ptr::drop_in_place(ptr.cast_mut());
                    }
                }
            }

            let mut guard = DropGuard {
                gc_ptr,
                init_length: 0,
            };

            let slice_ptr = &raw mut (*gc_ptr.as_ptr()).slice;
            for (i, element) in (slice_ptr as *mut [MaybeUninit<E>])
                .as_mut_unchecked()
                .iter_mut()
                .enumerate()
            {
                element.write(create_element(i));
                guard.init_length = i + 1;
            }

            mem::forget(guard);
        }

        gc_ptr.header().set_live(true);

        Gc {
            ptr: gc_ptr,
            _invariant: PhantomData,
        }
    }
}

impl<'gc, H: Collect<'gc> + 'gc, E: Collect<'gc> + Copy + 'gc> Gc<'gc, HeaderSlice<H, E>> {
    #[inline]
    pub fn new_header_slice_copy(mc: &Mutation<'gc>, header: H, elements: &[E]) -> Self {
        let gc_ptr = mc.allocate_slice::<H, E>(elements.len());

        unsafe {
            ptr::write(&raw mut (*gc_ptr.as_ptr()).header, header);
            ptr::copy_nonoverlapping(
                elements.as_ptr(),
                (&raw mut (*gc_ptr.as_ptr()).slice) as *mut E,
                elements.len(),
            );
        }

        gc_ptr.header().set_live(true);

        Gc {
            ptr: gc_ptr,
            _invariant: PhantomData,
        }
    }
}

impl<'gc, H: Collect<'gc> + 'gc, E: 'static> Gc<'gc, HeaderSlice<H, E>> {
    /// Allocate a new [`HeaderSlice`] with the given `header` and elements created from
    /// `create_element`.
    #[inline]
    pub fn new_static_header_slice_with(
        mc: &Mutation<'gc>,
        header: H,
        len: usize,
        mut create_element: impl FnMut(usize) -> E,
    ) -> Self {
        let header_slice: Gc<HeaderSlice<H, Static<E>>> =
            Gc::new_header_slice_with(mc, header, len, move |i| Static(create_element(i)));

        // SAFETY: `Static` is `#[repr(transparent)]`
        unsafe { Gc::from_ptr(Gc::as_ptr(header_slice) as *const HeaderSlice<H, E>) }
    }
}

impl<'gc, H: Collect<'gc> + 'gc, E: Copy + 'static> Gc<'gc, HeaderSlice<H, E>> {
    #[inline]
    pub fn new_static_header_slice_copy(mc: &Mutation<'gc>, header: H, elements: &[E]) -> Self {
        unsafe {
            // SAFETY: `Static` is `#[repr(transparent)]`

            let header_slice: Gc<HeaderSlice<H, Static<E>>> =
                Gc::new_header_slice_copy(mc, header, mem::transmute::<_, &[Static<E>]>(elements));
            Gc::from_ptr(Gc::as_ptr(header_slice) as *const HeaderSlice<H, E>)
        }
    }
}

impl<'gc, E: Collect<'gc> + 'gc> Gc<'gc, [E]> {
    /// Allocate a new bare slice with elements created from `create_element`.
    #[inline]
    pub fn new_slice_with(
        mc: &Mutation<'gc>,
        len: usize,
        create_element: impl FnMut(usize) -> E,
    ) -> Self {
        let header_slice: Gc<HeaderSlice<(), E>> =
            Gc::new_header_slice_with(mc, (), len, create_element);

        // SAFETY: `HeaderSlice` is `#[repr(C)]` and the header is a ZST.
        unsafe { Gc::from_ptr(Gc::as_ptr(header_slice) as *const [E]) }
    }
}

impl<'gc, E: Collect<'gc> + Copy + 'gc> Gc<'gc, [E]> {
    /// Allocate a slice with elements copied from the given slice.
    #[inline]
    pub fn new_slice_copy(mc: &Mutation<'gc>, elements: &[E]) -> Self {
        let header_slice: Gc<HeaderSlice<(), E>> = Gc::new_header_slice_copy(mc, (), elements);

        // SAFETY: `HeaderSlice` is `#[repr(C)]` and the header is a ZST.
        unsafe { Gc::from_ptr(Gc::as_ptr(header_slice) as *const [E]) }
    }
}

impl<'gc, E: Collect<'gc> + Clone + 'gc> Gc<'gc, [E]> {
    /// Allocate a slice with elements cloned from the given slice.
    #[inline]
    pub fn new_slice_clone(mc: &Mutation<'gc>, elements: &[E]) -> Self {
        Self::new_slice_with(mc, elements.len(), |i| elements[i].clone())
    }
}

impl<'gc, E: 'static> Gc<'gc, [E]> {
    /// Allocate a new bare slice with elements created from `create_element`.
    #[inline]
    pub fn new_static_slice_with(
        mc: &Mutation<'gc>,
        len: usize,
        create_element: impl FnMut(usize) -> E,
    ) -> Self {
        let header_slice: Gc<HeaderSlice<(), E>> =
            Gc::new_static_header_slice_with(mc, (), len, create_element);

        // SAFETY: `HeaderSlice` is `#[repr(C)]` and the header is a ZST.
        unsafe { Gc::from_ptr(Gc::as_ptr(header_slice) as *const [E]) }
    }
}

impl<'gc, E: Copy + 'static> Gc<'gc, [E]> {
    /// Allocate a slice with elements copied from the given slice.
    #[inline]
    pub fn new_static_slice_copy(mc: &Mutation<'gc>, elements: &[E]) -> Self {
        let header_slice: Gc<HeaderSlice<(), E>> =
            Gc::new_static_header_slice_copy(mc, (), elements);

        // SAFETY: `HeaderSlice` is `#[repr(C)]` and the header is a ZST.
        unsafe { Gc::from_ptr(Gc::as_ptr(header_slice) as *const [E]) }
    }
}

impl<'gc, E: Clone + 'static> Gc<'gc, [E]> {
    /// Allocate a slice with elements cloned from the given slice.
    #[inline]
    pub fn new_static_slice_clone(mc: &Mutation<'gc>, elements: &[E]) -> Self {
        Self::new_static_slice_with(mc, elements.len(), |i| elements[i].clone())
    }
}

impl<'gc, T: 'static> Gc<'gc, T> {
    /// Create a new `Gc` pointer from a `'static` value.
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
    /// # Safety
    ///
    /// It must be valid to dereference a `*mut U` that has come from casting a `*mut T`.
    #[inline]
    pub unsafe fn cast<U: 'gc>(this: Gc<'gc, T>) -> Gc<'gc, U> {
        Gc {
            ptr: this.ptr.cast(),
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

    /// Retrieve a `Gc` from a raw pointer obtained from [`Gc::as_ptr`].
    ///
    /// # Safety
    ///
    /// The provided pointer must have been obtained from `Gc::as_ptr`, must not have been collected
    /// yet, and must be dereferencable as its current type.
    #[inline]
    pub unsafe fn from_ptr(ptr: *const T) -> Gc<'gc, T> {
        unsafe {
            Gc {
                ptr: GcPtr::from_ptr(ptr.cast_mut()),
                _invariant: PhantomData,
            }
        }
    }
}

impl<'gc, T: Unlock + ?Sized + 'gc> Gc<'gc, T> {
    /// Shorthand for [`Gc::write`]`(mc, self).`[`unlock()`](Write::unlock).
    #[inline]
    pub fn unlock(self, mc: &Mutation<'gc>) -> &'gc T::Unlocked {
        Gc::write(mc, self).unlock()
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
        unsafe { self.ptr.as_ref() }
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
        this.ptr.addr_eq(other.ptr)
    }

    #[inline]
    pub fn as_ptr(gc: Gc<'gc, T>) -> *const T {
        gc.ptr.as_ptr()
    }

    /// Returns true when a pointer is *dead* during finalization. This is equivalent to
    /// `GcWeak::is_dead` for strong pointers.
    ///
    /// Any strong pointer reachable from the root will never be dead, BUT there can be strong
    /// pointers reachable only through other weak pointers that can be dead.
    #[inline]
    pub fn is_dead(_: &Finalization<'gc>, gc: Gc<'gc, T>) -> bool {
        matches!(gc.ptr.header().color(), GcColor::White | GcColor::WhiteWeak)
    }

    /// Manually marks a dead `Gc` pointer as reachable and keeps it alive.
    ///
    /// Equivalent to `GcWeak::resurrect` for strong pointers. Manually marks this pointer and
    /// all transitively held pointers as reachable, thus keeping them from being dropped this
    /// collection cycle.
    #[inline]
    pub fn resurrect(fc: &Finalization<'gc>, gc: Gc<'gc, T>) {
        fc.resurrect(gc.ptr.erase());
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
