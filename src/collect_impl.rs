use alloc::boxed::Box;
use alloc::collections::{BTreeMap, BTreeSet, BinaryHeap, LinkedList, VecDeque};
use alloc::rc::Rc;
use alloc::string::String;
use alloc::vec::Vec;
use core::cell::{Cell, RefCell};
use core::marker::PhantomData;
#[cfg(feature = "std")]
use std::collections::{HashMap, HashSet};

use crate::collect::{Collect, Trace};

/// If a type is static, we know that it can never hold `Gc` pointers, so it is safe to provide a
/// simple empty `Collect` implementation.
#[macro_export]
macro_rules! static_collect {
    ($type:ty) => {
        unsafe impl<'gc> Collect<'gc> for $type
        where
            $type: 'static,
        {
            const NEEDS_TRACE: bool = false;
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
static_collect!(str);
static_collect!(alloc::ffi::CString);
static_collect!(core::ffi::CStr);
static_collect!(core::any::TypeId);
#[cfg(feature = "std")]
static_collect!(std::path::Path);
#[cfg(feature = "std")]
static_collect!(std::path::PathBuf);
#[cfg(feature = "std")]
static_collect!(std::ffi::OsStr);
#[cfg(feature = "std")]
static_collect!(std::ffi::OsString);

/// SAFETY: We know that a `&'static` reference cannot possibly point to `'gc` data, so it is safe
/// to keep in a rooted objet and we do not have to trace through it.
///
/// HOWEVER, There is an extra bound here that seems superfluous. If we have a `&'static T`, why do
/// we require `T: 'static`, shouldn't this be implied, otherwise a `&'static T` would not be well-
/// formed? WELL, there are currently some neat compiler bugs, observe...
///
/// ```rust,compile_fail
/// let arena = Arena::<Rootable![&'static Gc<'gc, i32>]>::new(Default::default(), |mc| {
///     Box::leak(Box::new(Gc::new(mc, 4)))
/// });
/// ```
///
/// At the time of this writing, without the extra `T: static` bound, the above code compiles and
/// produces an arena with a reachable but un-traceable `Gc<'gc, i32>`, and this is unsound. This
/// *is* ofc the stored type of the root, since the Arena is actually constructing a `&'static
/// Gc<'static, i32>` as the root object, but this should still not rightfully compile due to the
/// signature of the constructor callback passed to `Arena::new`. In fact, the 'static lifetime is a
/// red herring, it is possible to change the internals of `Arena` such that the 'gc lifetime given
/// to the callback is *not* 'static, and the problem persists.
///
/// It should not be required to have this extra lifetime bound, and yet! It fixes the above issue
/// perfectly and the given example of unsoundness no longer compiles. So, until this rustc bug
/// is fixed...
///
/// DO NOT REMOVE THIS EXTRA `T: 'static` BOUND
unsafe impl<'gc, T: ?Sized + 'static> Collect<'gc> for &'static T {
    const NEEDS_TRACE: bool = false;
}

unsafe impl<'gc, T: ?Sized + Collect<'gc>> Collect<'gc> for Box<T> {
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        cc.trace(&**self)
    }
}

unsafe impl<'gc, T: Collect<'gc>> Collect<'gc> for [T] {
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        for t in self.iter() {
            cc.trace(t)
        }
    }
}

unsafe impl<'gc, T: Collect<'gc>> Collect<'gc> for Option<T> {
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        if let Some(t) = self.as_ref() {
            cc.trace(t)
        }
    }
}

unsafe impl<'gc, T: Collect<'gc>, E: Collect<'gc>> Collect<'gc> for Result<T, E> {
    const NEEDS_TRACE: bool = T::NEEDS_TRACE || E::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        match self {
            Ok(r) => cc.trace(r),
            Err(e) => cc.trace(e),
        }
    }
}

unsafe impl<'gc, T: Collect<'gc>> Collect<'gc> for Vec<T> {
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        for t in self {
            cc.trace(t)
        }
    }
}

unsafe impl<'gc, T: Collect<'gc>> Collect<'gc> for VecDeque<T> {
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        for t in self {
            cc.trace(t)
        }
    }
}

unsafe impl<'gc, T: Collect<'gc>> Collect<'gc> for LinkedList<T> {
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        for t in self {
            cc.trace(t)
        }
    }
}

#[cfg(feature = "std")]
unsafe impl<'gc, K, V, S> Collect<'gc> for HashMap<K, V, S>
where
    K: Collect<'gc>,
    V: Collect<'gc>,
    S: 'static,
{
    const NEEDS_TRACE: bool = K::NEEDS_TRACE || V::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        for (k, v) in self {
            cc.trace(k);
            cc.trace(v);
        }
    }
}

#[cfg(feature = "std")]
unsafe impl<'gc, T, S> Collect<'gc> for HashSet<T, S>
where
    T: Collect<'gc>,
    S: 'static,
{
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        for v in self {
            cc.trace(v);
        }
    }
}

unsafe impl<'gc, K, V> Collect<'gc> for BTreeMap<K, V>
where
    K: Collect<'gc>,
    V: Collect<'gc>,
{
    const NEEDS_TRACE: bool = K::NEEDS_TRACE || V::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        for (k, v) in self {
            cc.trace(k);
            cc.trace(v);
        }
    }
}

unsafe impl<'gc, T> Collect<'gc> for BTreeSet<T>
where
    T: Collect<'gc>,
{
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        for v in self {
            cc.trace(v);
        }
    }
}

unsafe impl<'gc, T> Collect<'gc> for BinaryHeap<T>
where
    T: Collect<'gc>,
{
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        for v in self {
            cc.trace(v);
        }
    }
}

unsafe impl<'gc, T> Collect<'gc> for Rc<T>
where
    T: ?Sized + Collect<'gc>,
{
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        cc.trace(&**self);
    }
}

#[cfg(target_has_atomic = "ptr")]
unsafe impl<'gc, T> Collect<'gc> for alloc::sync::Arc<T>
where
    T: ?Sized + Collect<'gc>,
{
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        cc.trace(&**self);
    }
}

unsafe impl<'gc, T> Collect<'gc> for Cell<T>
where
    T: 'static,
{
    const NEEDS_TRACE: bool = false;
}

unsafe impl<'gc, T> Collect<'gc> for RefCell<T>
where
    T: 'static,
{
    const NEEDS_TRACE: bool = false;
}

// SAFETY: `PhantomData` is a ZST, and therefore doesn't store anything
unsafe impl<'gc, T> Collect<'gc> for PhantomData<T> {
    const NEEDS_TRACE: bool = false;
}

unsafe impl<'gc, T: Collect<'gc>, const N: usize> Collect<'gc> for [T; N] {
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        for t in self {
            cc.trace(t)
        }
    }
}

macro_rules! impl_tuple {
    () => (
        unsafe impl<'gc> Collect<'gc> for () {
            const NEEDS_TRACE: bool = false;
        }
    );

    ($($name:ident)+) => (
        unsafe impl<'gc, $($name,)*> Collect<'gc> for ($($name,)*)
            where $($name: Collect<'gc>,)*
        {
            const NEEDS_TRACE: bool = false $(|| $name::NEEDS_TRACE)*;

            #[allow(non_snake_case)]
            #[inline]
            fn trace<TR: Trace<'gc> >(&self, cc: &mut TR) {
                let ($($name,)*) = self;
                $(cc.trace($name);)*
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
