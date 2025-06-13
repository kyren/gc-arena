//! Write barrier management.

use core::borrow::Borrow;
use core::mem;
use core::ops::{
    Deref, DerefMut, Index, Range, RangeFrom, RangeInclusive, RangeTo, RangeToInclusive,
};

use alloc::collections::{BTreeMap, VecDeque};
use alloc::vec::Vec;

#[cfg(feature = "std")]
use std::{collections::HashMap, hash::BuildHasher, hash::Hash};

#[cfg(doc)]
use crate::Gc;
#[cfg(doc)]
use core::ops::IndexMut;

/// An (interiorly-)mutable reference inside a GC'd object graph.
///
/// This type can only exist behind a reference; it is typically obtained by calling
/// [`Gc::write`] on a [`Gc`] pointer or by using the [`field!`] projection macro
/// on a pre-existing `&Write<T>`.
#[non_exhaustive]
#[repr(transparent)]
pub struct Write<T: ?Sized> {
    // Public so that the `field!` macro can pattern-match on it; the `non_exhaustive` attribute
    // prevents 3rd-party code from instanciating the struct directly.
    #[doc(hidden)]
    pub __inner: T,
}

impl<T: ?Sized> Deref for Write<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.__inner
    }
}

impl<T: ?Sized> DerefMut for Write<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.__inner
    }
}

impl<T: IndexWrite<I> + ?Sized, I> Index<I> for Write<T> {
    type Output = Write<T::Output>;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        // SAFETY: `IndexWrite` guarantees that the `Index` impl is `Write`-compatible.
        unsafe { Write::assume(&self.__inner[index]) }
    }
}

impl<T: ?Sized> Write<T> {
    /// Asserts that the given reference can be safely written to.
    ///
    /// # Safety
    /// In order to maintain the invariants of the garbage collector, no new [`Gc`] pointers
    /// may be adopted by the referenced value as a result of the interior mutability enabled
    /// by this wrapper, unless [`Gc::write`] is invoked manually on the parent [`Gc`]
    /// pointer during the current arena callback.
    #[inline(always)]
    pub unsafe fn assume(v: &T) -> &Self {
        // SAFETY: `Self` is `repr(transparent)`.
        mem::transmute(v)
    }

    /// Gets a writable reference to non-GC'd data.
    ///
    /// This is safe, as `'static` types can never hold [`Gc`] pointers.
    #[inline]
    pub fn from_static(v: &T) -> &Self
    where
        T: 'static,
    {
        // SAFETY: `Self` is `repr(transparent)`.
        unsafe { mem::transmute(v) }
    }

    /// Gets a writable reference from a `&mut T`.
    ///
    /// This is safe, as exclusive access already implies writability.
    #[inline]
    pub fn from_mut(v: &mut T) -> &mut Self {
        // SAFETY: `Self` is `repr(transparent)`.
        unsafe { mem::transmute(v) }
    }

    /// Implementation detail of `write_field!`; same safety requirements as `assume`.
    #[inline(always)]
    #[doc(hidden)]
    pub unsafe fn __from_ref_and_ptr(v: &T, _: *const T) -> &Self {
        // SAFETY: `Self` is `repr(transparent)`.
        mem::transmute(v)
    }

    /// Unlocks the referenced value, providing full interior mutability.
    #[inline]
    pub fn unlock(&self) -> &T::Unlocked
    where
        T: Unlock,
    {
        // SAFETY: a `&Write<T>` implies that a write barrier was triggered on the parent `Gc`.
        unsafe { self.__inner.unlock_unchecked() }
    }

    /// Propagates a write barrier through a smart pointer dereference.
    #[inline(always)]
    pub fn as_deref(&self) -> &Write<T::Target>
    where
        T: DerefWrite,
    {
        // SAFETY: `DerefWrite` guarantees that the `Deref` impl is `Write`-compatible.
        unsafe { Write::assume(&*self) }
    }
}

impl<T> Write<Option<T>> {
    /// Converts from `&Write<Option<T>>` to `Option<&Write<T>>`.
    #[inline(always)]
    pub fn as_write(&self) -> Option<&Write<T>> {
        // SAFETY: this is simple destructuring
        unsafe {
            match &self.__inner {
                None => None,
                Some(v) => Some(Write::assume(v)),
            }
        }
    }
}

impl<T, E> Write<Result<T, E>> {
    /// Converts from `&Write<Result<T, E>>` to `Result<&Write<T>, &Write<E>>`.
    #[inline(always)]
    pub fn as_write(&self) -> Result<&Write<T>, &Write<E>> {
        // SAFETY: this is simple destructuring
        unsafe {
            match &self.__inner {
                Ok(v) => Ok(Write::assume(v)),
                Err(e) => Err(Write::assume(e)),
            }
        }
    }
}

/// Types which preserve write barriers when dereferenced.
///
/// # Safety
/// Implementing this trait is a promise that the corresponding [`Deref`] impl
/// (and [`DerefMut`], if it exists) can be used to project a [`Write`] reference.
///
/// In particular, both `Deref::deref` and `DerefMut::deref_mut`:
/// - must return a reference into the *same* GC'd object as the input;
/// - must not adopt new [`Gc`] pointers.
pub unsafe trait DerefWrite: Deref {}

// SAFETY: All these types have pure & non-GC-traversing Deref(Mut) impls
unsafe impl<T: ?Sized> DerefWrite for &T {}
unsafe impl<T: ?Sized> DerefWrite for alloc::boxed::Box<T> {}
unsafe impl<T> DerefWrite for Vec<T> {}
unsafe impl<T: ?Sized> DerefWrite for alloc::rc::Rc<T> {}
#[cfg(target_has_atomic = "ptr")]
unsafe impl<T: ?Sized> DerefWrite for alloc::sync::Arc<T> {}

/// Types which preserve write barriers when indexed.
///
/// # Safety
/// Implementing this trait is a promise that the corresponding [`Index<I>`] impl
/// (and [`IndexMut<I>`], if it exists) can be used to project a [`Write`] reference.
///
/// In particular, both `Index::index` and `IndexMut::index_mut`:
/// - must return a reference into the *same* GC'd object as the input;
/// - must not adopt new [`Gc`] pointers.
pub unsafe trait IndexWrite<I: ?Sized>: Index<I> {}

// SAFETY: All these types have pure & non-GC-traversing Index(Mut) impls
// Note that we don't write `impl<..., I> IndexWrite<I> for ... where ...: Index<I>`,
// as this would allow arbitrary implementations through third-party `I` types.
unsafe impl<T> IndexWrite<usize> for [T] {}
unsafe impl<T> IndexWrite<Range<usize>> for [T] {}
unsafe impl<T> IndexWrite<RangeFrom<usize>> for [T] {}
unsafe impl<T> IndexWrite<RangeInclusive<usize>> for [T] {}
unsafe impl<T> IndexWrite<RangeTo<usize>> for [T] {}
unsafe impl<T> IndexWrite<RangeToInclusive<usize>> for [T] {}
unsafe impl<T, I, const N: usize> IndexWrite<I> for [T; N] where [T]: IndexWrite<I> {}
unsafe impl<T, I> IndexWrite<I> for Vec<T>
where
    [T]: IndexWrite<I>,
    Self: Index<I>,
{
}
unsafe impl<T> IndexWrite<usize> for VecDeque<T> {}
unsafe impl<K, V, Q> IndexWrite<&Q> for BTreeMap<K, V>
where
    K: Borrow<Q> + Ord,
    Q: Ord + ?Sized,
{
}
#[cfg(feature = "std")]
unsafe impl<K, V, S, Q> IndexWrite<&Q> for HashMap<K, V, S>
where
    K: Eq + Hash + Borrow<Q>,
    Q: Eq + Hash + ?Sized,
    S: BuildHasher,
{
}

/// Types that support additional operations (typically, mutation) when behind a write barrier.
pub trait Unlock {
    /// This will typically be a cell-like type providing some sort of interior mutability.
    type Unlocked: ?Sized;

    /// Provides unsafe access to the unlocked type, *without* triggering a write barrier.
    ///
    /// # Safety
    ///
    /// In order to maintain the invariants of the garbage collector, no new `Gc` pointers
    /// may be adopted by as a result of the interior mutability afforded by the unlocked value,
    /// unless the write barrier for the containing `Gc` pointer is invoked manually before
    /// collection is triggered.
    unsafe fn unlock_unchecked(&self) -> &Self::Unlocked;
}

/// Macro for named field projection behind [`Write`] references.
///
/// # Usage
///
/// ```
/// # use gc_arena::barrier::{field, Write};
/// struct Container<T> {
///     field: T,
/// }
///
/// fn project<T>(v: &Write<Container<T>>) -> &Write<T> {
///     field!(v, Container, field)
/// }
/// ```
///
/// # Limitations
///
/// This macro only support structs with named fields; tuples and enums aren't supported.
#[doc(inline)]
pub use crate::__field as field;

// Actual macro item, hidden so that it doesn't show at the crate root.
#[macro_export]
#[doc(hidden)]
macro_rules! __field {
    ($value:expr, $type:path, $field:ident) => {
        // SAFETY:
        // For this to be sound, we need to prevent deref coercions from happening, as they may
        // access nested `Gc` pointers, which would violate the write barrier invariant. This is
        // guaranteed as follows:
        // - the destructuring pattern, unlike a simple field access, cannot call `Deref`;
        // - similarly, the `__from_ref_and_ptr` method takes both a reference (for the lifetime)
        //   and a pointer, causing a compilation failure if the first argument was coerced.
        match $value {
            $crate::barrier::Write {
                __inner: $type { ref $field, .. },
                ..
            } => unsafe { $crate::barrier::Write::__from_ref_and_ptr($field, $field as *const _) },
        }
    };
}

/// Shorthand for [`field!`]`(...).`[`unlock()`](Write::unlock).
#[doc(inline)]
pub use crate::__unlock as unlock;

// Actual macro item, hidden so that it doesn't show at the crate root.
#[macro_export]
#[doc(hidden)]
macro_rules! __unlock {
    ($value:expr, $type:path, $field:ident) => {
        $crate::barrier::field!($value, $type, $field).unlock()
    };
}
