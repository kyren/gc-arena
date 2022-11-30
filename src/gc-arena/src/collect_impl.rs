use alloc::boxed::Box;
use alloc::collections::VecDeque;
use alloc::collections::{BTreeMap, BTreeSet};
use alloc::rc::Rc;
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::cell::{Cell, RefCell};
#[cfg(feature = "std")]
use core::hash::{BuildHasher, Hash};
use core::marker::PhantomData;
#[cfg(feature = "std")]
use std::collections::{HashMap, HashSet};

use crate::collect::Collect;
use crate::context::CollectionContext;

/// If a type will never hold `Gc` pointers, you can use this macro to provide a simple empty
/// `Collect` implementation.
#[macro_export]
macro_rules! unsafe_empty_collect {
    ($type:ty) => {
        unsafe impl Collect for $type {
            #[inline]
            fn needs_trace() -> bool {
                false
            }
        }
    };
}

/// If a type is static, we know that it can never hold `Gc` pointers, so it is safe to provide a
/// simple empty `Collect` implementation.
/// `Collect` implementation.
#[macro_export]
macro_rules! static_collect {
    ($type:ty) => {
        unsafe impl Collect for $type
        where
            $type: 'static,
        {
            #[inline]
            fn needs_trace() -> bool {
                false
            }
        }
    };
}

static_collect!(bool);
static_collect!(char);
static_collect!(u8);
static_collect!(u16);
static_collect!(u32);
static_collect!(u64);
static_collect!(usize);
static_collect!(i8);
static_collect!(i16);
static_collect!(i32);
static_collect!(i64);
static_collect!(isize);
static_collect!(f32);
static_collect!(f64);
static_collect!(String);

unsafe impl<'a, T: ?Sized> Collect for &'a T {
    #[inline]
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl<'a, T: ?Sized> Collect for &'a mut T {
    #[inline]
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl<T: ?Sized + Collect> Collect for Box<T> {
    #[inline]
    fn trace(&self, cc: CollectionContext) {
        (**self).trace(cc)
    }
}

unsafe impl<T: Collect> Collect for Box<[T]> {
    #[inline]
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        for t in self.iter() {
            t.trace(cc)
        }
    }
}

unsafe impl<T: Collect> Collect for Option<T> {
    #[inline]
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        if let Some(t) = self.as_ref() {
            t.trace(cc)
        }
    }
}

unsafe impl<T: Collect, E: Collect> Collect for Result<T, E> {
    #[inline]
    fn needs_trace() -> bool {
        T::needs_trace() || E::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        match self {
            Ok(r) => r.trace(cc),
            Err(e) => e.trace(cc),
        }
    }
}

unsafe impl<T: Collect> Collect for Vec<T> {
    #[inline]
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        for t in self {
            t.trace(cc)
        }
    }
}

unsafe impl<T: Collect> Collect for VecDeque<T> {
    #[inline]
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        for t in self {
            t.trace(cc)
        }
    }
}

#[cfg(feature = "std")]
unsafe impl<K, V, S> Collect for HashMap<K, V, S>
where
    K: Eq + Hash + Collect,
    V: Collect,
    S: BuildHasher + 'static,
{
    #[inline]
    fn needs_trace() -> bool {
        K::needs_trace() || V::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        for (k, v) in self {
            k.trace(cc);
            v.trace(cc);
        }
    }
}

#[cfg(feature = "std")]
unsafe impl<T, S> Collect for HashSet<T, S>
where
    T: Eq + Hash + Collect,
    S: BuildHasher + 'static,
{
    #[inline]
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        for v in self {
            v.trace(cc);
        }
    }
}

unsafe impl<K, V> Collect for BTreeMap<K, V>
where
    K: Eq + Ord + Collect,
    V: Collect,
{
    #[inline]
    fn needs_trace() -> bool {
        K::needs_trace() || V::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        for (k, v) in self {
            k.trace(cc);
            v.trace(cc);
        }
    }
}

unsafe impl<T> Collect for BTreeSet<T>
where
    T: Eq + Ord + Collect,
{
    #[inline]
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        for v in self {
            v.trace(cc);
        }
    }
}

unsafe impl<T> Collect for Rc<T>
where
    T: ?Sized + Collect,
{
    #[inline]
    fn trace(&self, cc: CollectionContext) {
        (**self).trace(cc);
    }
}

unsafe impl<T> Collect for Arc<T>
where
    T: ?Sized + Collect,
{
    #[inline]
    fn trace(&self, cc: CollectionContext) {
        (**self).trace(cc);
    }
}

unsafe impl<T> Collect for Cell<T>
where
    T: 'static,
{
    #[inline]
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl<T> Collect for RefCell<T>
where
    T: 'static,
{
    #[inline]
    fn needs_trace() -> bool {
        false
    }
}

// SAFETY: `PhantomData` is a ZST, and therefore doesn't store anything
unsafe impl<T> Collect for PhantomData<T> {
    #[inline]
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl<T: Collect, const N: usize> Collect for [T; N] {
    #[inline]
    fn needs_trace() -> bool {
        T::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: CollectionContext) {
        for t in self {
            t.trace(cc)
        }
    }
}

macro_rules! impl_tuple {
    () => (
        unsafe impl Collect for () {
            #[inline]
            fn needs_trace() -> bool {
                false
            }
        }
    );

    ($($name:ident)+) => (
        unsafe impl<$($name,)*> Collect for ($($name,)*)
            where $($name: Collect,)*
        {
            #[inline]
            fn needs_trace() -> bool {
                $($name::needs_trace() ||)* false
            }

            #[allow(non_snake_case)]
            #[inline]
            fn trace(&self, cc: CollectionContext) {
                let ($($name,)*) = self;
                $($name.trace(cc);)*
            }
        }
    );
}

impl_tuple! {}
impl_tuple! {A}
impl_tuple! {A B}
impl_tuple! {A B C}
impl_tuple! {A B C D}
impl_tuple! {A B C D E}
impl_tuple! {A B C D E F}
impl_tuple! {A B C D E F G}
impl_tuple! {A B C D E F G H}
impl_tuple! {A B C D E F G H I}
impl_tuple! {A B C D E F G H I J}
impl_tuple! {A B C D E F G H I J K}
impl_tuple! {A B C D E F G H I J K L}
impl_tuple! {A B C D E F G H I J K L M}
impl_tuple! {A B C D E F G H I J K L M N}
impl_tuple! {A B C D E F G H I J K L M N O}
impl_tuple! {A B C D E F G H I J K L M N O P}
