use crate::collect::Collect;
use crate::Rootable;

use alloc::borrow::{Borrow, BorrowMut};
use core::convert::{AsMut, AsRef};
use core::ops::{Deref, DerefMut};

/// A wrapper type that implements Collect whenever the contained T is 'static, which is useful in
/// generic contexts
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
#[repr(transparent)]
pub struct Static<T: ?Sized>(pub T);

impl<'a, T: ?Sized + 'static> Rootable<'a> for Static<T> {
    type Root = Static<T>;
}

unsafe impl<T: ?Sized + 'static> Collect for Static<T> {
    #[inline]
    fn needs_trace() -> bool {
        false
    }
}

impl<T> From<T> for Static<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<T: ?Sized> AsRef<T> for Static<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T: ?Sized> AsMut<T> for Static<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T: ?Sized> Deref for Static<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: ?Sized> DerefMut for Static<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: ?Sized> Borrow<T> for Static<T> {
    fn borrow(&self) -> &T {
        &self.0
    }
}

impl<T: ?Sized> BorrowMut<T> for Static<T> {
    fn borrow_mut(&mut self) -> &mut T {
        &mut self.0
    }
}
