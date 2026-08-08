use alloc::borrow::{Borrow, BorrowMut};
use core::{
    convert::{AsMut, AsRef},
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use crate::{
    arena::Rootable,
    collect::Collect,
    meta::{AllocMeta, PtrMeta},
};

/// A wrapper type that implements Collect whenever the contained T is 'static, which is useful in
/// generic contexts
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
#[repr(transparent)]
pub struct Static<T: ?Sized>(pub T);

impl<'a, T: ?Sized + 'static> Rootable<'a> for Static<T> {
    type Root = Static<T>;
}

unsafe impl<'gc, T: ?Sized + 'static> Collect<'gc> for Static<T> {
    const NEEDS_TRACE: bool = false;
}

impl<T> From<T> for Static<T> {
    #[inline]
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<T: ?Sized> AsRef<T> for Static<T> {
    #[inline]
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T: ?Sized> AsMut<T> for Static<T> {
    #[inline]
    fn as_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T: ?Sized> Deref for Static<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: ?Sized> DerefMut for Static<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: ?Sized> Borrow<T> for Static<T> {
    #[inline]
    fn borrow(&self) -> &T {
        &self.0
    }
}

impl<T: ?Sized> BorrowMut<T> for Static<T> {
    #[inline]
    fn borrow_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

/// Takes a `P` which implements [`PtrMeta`] for a type `T` and makes an implementation of `PtrMeta`
/// which works on `Static<T>`.
///
/// Does not modify the inner implementation and converts all `*const Static<T>` to `*const T`,
/// which is safe to do since `Static` is `#[repr(transparent)]`.
pub struct StaticPtrMeta<P>(PhantomData<P>);

impl<T: ?Sized, M, P: PtrMeta<T, M>> PtrMeta<Static<T>, M> for StaticPtrMeta<P> {
    type PtrMetadata = P::PtrMetadata;
    type Thin = Static<P::Thin>;

    #[inline]
    fn to_thin(type_meta: M, fat: *const Static<T>) -> *const Static<P::Thin> {
        P::to_thin(type_meta, fat as *const T) as *const Static<P::Thin>
    }

    #[inline]
    fn from_thin(
        type_meta: M,
        thin: *const Static<P::Thin>,
        ptr_meta: P::PtrMetadata,
    ) -> *const Static<T> {
        P::from_thin(type_meta, thin as *const P::Thin, ptr_meta) as *const Static<T>
    }
}

impl<T: ?Sized, M, P: AllocMeta<T, M>> AllocMeta<Static<T>, M> for StaticPtrMeta<P> {
    #[inline]
    fn layout(type_meta: M, ptr_meta: Self::PtrMetadata) -> Option<core::alloc::Layout> {
        P::layout(type_meta, ptr_meta)
    }
}
