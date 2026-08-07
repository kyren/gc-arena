use crate::{Gc, GcWeak, gc::IsGcKind};

/// Unsizes a [`Gc`] or [`GcWeak`] pointer.
///
/// This macro is a `gc_arena`-specific replacement for the nightly-only `CoerceUnsized` trait.
///
/// ## Usage
///
/// ```rust
/// # use std::fmt::Display;
/// # use gc_arena::{Gc, unsize};
/// # fn main() {
/// # gc_arena::arena::rootless_mutate(|mc| {
/// // Unsizing arrays to slices.
/// let mut slice;
/// slice = unsize!(Gc::new(mc, [1, 2]) => [u8]);
/// assert_eq!(slice.len(), 2);
/// slice = unsize!(Gc::new(mc, [42; 4]) => [u8]);
/// assert_eq!(slice.len(), 4);
///
/// // Unsizing values to trait objects.
/// let mut display;
/// display = unsize!(Gc::new(mc, "Hello world!".to_owned()) => dyn Display);
/// assert_eq!(display.to_string(), "Hello world!");
/// display = unsize!(Gc::new(mc, 123456) => dyn Display);
/// assert_eq!(display.to_string(), "123456");
/// # })
/// # }
/// ```
///
/// The `unsize` macro is safe, and will fail to compile when trying to coerce between
/// incompatible types.
/// ```rust,compile_fail
/// # use std::error::Error;
/// # use gc_arena::{Gc, unsize};
/// # fn main() {
/// # gc_arena::arena::rootless_mutate(|mc| {
/// // Error: `Option<char>` doesn't implement `Error`.
/// let _ = unsize!(Gc::new(mc, Some('💥')) => dyn Error);
/// # })
/// # }
/// ```
#[macro_export]
macro_rules! unsize {
    ($gc:expr => $ty:ty) => {{
        let gc = $gc;
        // SAFETY: the closure has a trivial body and must be a valid pointer
        // coercion, if it compiles. Additionally, the `__CoercePtrInternal` trait
        // ensures that the resulting GC pointer has the correct `'gc` lifetime.
        unsafe {
            $crate::__CoercePtrInternal::__coerce_unchecked(gc, |p: *const _| -> *const $ty { p })
        }
    }};
}

// Not public API; implementation detail of the `unsize` macro.
//
// Maps a raw pointer coercion (`*const FromPtr -> *const ToPtr`) to
// a smart pointer coercion (`Self -> Dst`).
#[doc(hidden)]
pub unsafe trait __CoercePtrInternal<Dst> {
    type FromPtr;
    type ToPtr: ?Sized;
    // SAFETY: `coerce` must be a valid pointer coercion; in particular, the coerced
    // pointer must have the same address and provenance as the original.
    unsafe fn __coerce_unchecked<F>(self, coerce: F) -> Dst
    where
        F: FnOnce(*const Self::FromPtr) -> *const Self::ToPtr;
}

unsafe impl<'gc, T, U: ?Sized, K> __CoercePtrInternal<Gc<'gc, U>> for Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
{
    type FromPtr = T;
    type ToPtr = U;

    #[inline(always)]
    unsafe fn __coerce_unchecked<F>(self, coerce: F) -> Gc<'gc, U>
    where
        F: FnOnce(*const T) -> *const U,
    {
        unsafe { Gc::from_ptr(coerce(Gc::as_ptr(self))) }
    }
}

unsafe impl<'gc, T, U: ?Sized, K> __CoercePtrInternal<GcWeak<'gc, U>> for GcWeak<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
{
    type FromPtr = T;
    type ToPtr = U;

    #[inline(always)]
    unsafe fn __coerce_unchecked<F>(self, coerce: F) -> GcWeak<'gc, U>
    where
        F: FnOnce(*const T) -> *const U,
    {
        unsafe {
            let inner = self.inner.__coerce_unchecked(coerce);
            GcWeak { inner }
        }
    }
}
