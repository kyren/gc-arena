use crate::collect::Collect;
use crate::Rootable;

use alloc::borrow::{Borrow, BorrowMut};
use core::convert::{AsMut, AsRef};
use core::ops::{Deref, DerefMut};

/// A wrapper type that implements Collect whenever the contained T is 'static, which is useful in
/// generic contexts
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
#[repr(transparent)]
pub struct StaticCollect<T>(pub T);

impl<'a, T: 'static> Rootable<'a> for StaticCollect<T> {
    type Root = StaticCollect<T>;
}

unsafe impl<T: 'static> Collect for StaticCollect<T> {
    #[inline]
    fn needs_trace() -> bool {
        false
    }
}

impl<T> From<T> for StaticCollect<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<T> AsRef<T> for StaticCollect<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}
impl<T> AsMut<T> for StaticCollect<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T> Deref for StaticCollect<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T> DerefMut for StaticCollect<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> Borrow<T> for StaticCollect<T> {
    fn borrow(&self) -> &T {
        &self.0
    }
}
impl<T> BorrowMut<T> for StaticCollect<T> {
    fn borrow_mut(&mut self) -> &mut T {
        &mut self.0
    }
}
