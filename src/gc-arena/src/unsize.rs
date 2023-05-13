use core::ptr::NonNull;
use core::{marker::PhantomData, mem};

use crate::{
    types::GcBoxInner,
    Root, Rootable, {Gc, GcWeak},
};

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
/// # gc_arena::rootless_arena(|mc| {
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
/// # gc_arena::rootless_arena(|mc| {
/// // Error: `Option<char>` doesn't implement `Error`.
/// let _ = unsize!(Gc::new(mc, Some('💥')) => dyn Error);
/// # })
/// # }
/// ```
///
/// There are two allowed forms this macro can take:
///
/// `unsize!(gc => dyn Trait)`
///
/// ...as we have seen, and the strictly more powerful...
///
/// `unsize!(gc, T => U)`
///
/// ...where T and U are types that implement `for<'a> Rootable<'a>`. This form of the macro takes
/// an extra step vs the first form, and projects the 'gc lifetime of the pointer types to 'static
/// before projecting back to the correct lifetime. This is safe to do, it only affects the
/// internals of pointer coercion, and it and is useful in exactly one situation: where a
/// `Gc<'gc, T>` is Send if 'gc is 'static.
///
/// This can produce a `Gc<'gc, dyn MyTrait<'gc> + Send>` type by round tripping through the 'static
/// lifetime.
///
/// This is safe to do because `Gc<'static, T>` is Send if `T: Send + 'static`, and "at rest",
/// the entire root object is stored projected to the 'static lifetime. Morally speaking, `'gc`
/// *actually is* `'static`, but code that uses the arena cannot know the lifetime and must work
/// *for any* `'gc`. In any case, the `'gc` lifetime provided can only be used to produce `gc-
/// arena` related types, so the potential for damage is limited... Any other type using the
/// generative 'gc lifetime for anything is already unsound.
///
/// An example of using this to cast a `Gc<'gc, T> to `Gc<'gc, dyn MyTrait + Send>`...
/// ```rust
/// # use std::fmt::Display;
/// # use gc_arena::{Gc, Rootable, unsize};
/// # fn main() {
///   trait MyTrait<'gc> {}
///
///   struct MyStruct<'gc>(Gc<'gc, i32>);
///   impl<'gc> MyTrait<'gc> for MyStruct<'gc> {}
///
///   fn convert<'gc>(p: Gc<'gc, MyStruct<'gc>>) -> Gc<'gc, dyn MyTrait<'gc> + Send> {
///       unsize!(
///           p,
///           Rootable!['a => Gc<'a, MyStruct<'a>>] =>
///           Rootable!['a => Gc<'a, dyn MyTrait<'a> + Send>]
///       )
///   }
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
            $crate::__CoercePtrInternal::__coerce_unchecked(gc, |p: *mut _| -> *mut $ty { p })
        }
    }};

    ($gc:expr, $fr:ty => $tr:ty) => {{
        unsafe {
            $crate::__coerce_rooted_ptr_internal::<$fr, $tr, _>($gc, |p: *mut _| -> *mut _ { p })
        }
    }};
}

// Not public API; implementation detail of the `unsize` macro.
//
// Maps a raw pointer coercion (`*mut FromPtr -> *mut ToPtr`) to
// a smart pointer coercion (`Self -> Dst`).
#[doc(hidden)]
pub unsafe trait __CoercePtrInternal<Dst> {
    type FromPtr;
    type ToPtr: ?Sized;
    // SAFETY: `coerce` must be a valid pointer coercion; in particular, the coerced
    // pointer must have the same address and provenance as the original.
    unsafe fn __coerce_unchecked<F>(self, coerce: F) -> Dst
    where
        F: FnOnce(*mut Self::FromPtr) -> *mut Self::ToPtr;
}

#[doc(hidden)]
pub unsafe fn __coerce_rooted_ptr_internal<'gc, T, U, F>(
    from: Root<'gc, T>,
    coerce: F,
) -> Root<'gc, U>
where
    T: ?Sized + for<'a> Rootable<'a>,
    U: ?Sized + for<'a> Rootable<'a>,
    for<'a> Root<'a, T>: __CoercePtrInternal<Root<'a, U>>,
    F: FnOnce(
        *mut <Root<'static, T> as __CoercePtrInternal<Root<'static, U>>>::FromPtr,
    ) -> *mut <Root<'static, T> as __CoercePtrInternal<Root<'static, U>>>::ToPtr,
{
    let from_static = mem::transmute::<_, Root<'static, T>>(from);
    let to_static = __CoercePtrInternal::__coerce_unchecked(from_static, coerce);
    mem::transmute::<_, Root<'gc, U>>(to_static)
}

unsafe impl<'gc, T, U: ?Sized> __CoercePtrInternal<Gc<'gc, U>> for Gc<'gc, T> {
    type FromPtr = T;
    type ToPtr = U;

    #[inline(always)]
    unsafe fn __coerce_unchecked<F>(self, coerce: F) -> Gc<'gc, U>
    where
        F: FnOnce(*mut T) -> *mut U,
    {
        let ptr = self.ptr.as_ptr() as *mut T;
        let ptr = NonNull::new_unchecked(coerce(ptr) as *mut GcBoxInner<U>);
        Gc {
            ptr,
            _invariant: PhantomData,
        }
    }
}

unsafe impl<'gc, T, U: ?Sized> __CoercePtrInternal<GcWeak<'gc, U>> for GcWeak<'gc, T> {
    type FromPtr = T;
    type ToPtr = U;

    #[inline(always)]
    unsafe fn __coerce_unchecked<F>(self, coerce: F) -> GcWeak<'gc, U>
    where
        F: FnOnce(*mut T) -> *mut U,
    {
        let inner = self.inner.__coerce_unchecked(coerce);
        GcWeak { inner }
    }
}
