use core::cell::{BorrowError, BorrowMutError, Ref, RefCell as StdRefCell, RefMut};
use core::fmt::{self, Debug};

use crate::collect::Collect;
use crate::context::{CollectionContext, MutationContext};
use crate::Gc;

use super::{CellLike, Mutable};

/// A GC-aware [`RefCell`](StdRefCell).
pub struct RefCell<T: ?Sized>(StdRefCell<T>);

impl<T: Debug> Debug for RefCell<T> {
    #[inline(always)]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&self.0, fmt)
    }
}

unsafe impl<T: Collect + ?Sized> Collect for RefCell<T> {
    #[inline(always)]
    fn trace(&self, cc: CollectionContext) {
        self.0.borrow().trace(cc)
    }
}

impl<T: Collect> CellLike for RefCell<T> {
    type Target = T;

    #[inline(always)]
    fn wrap(value: Self::Target) -> Self {
        Self(StdRefCell::new(value))
    }
}

impl<T> RefCell<T> {
    #[inline(always)]
    pub fn new(value: T) -> Self {
        Self(StdRefCell::new(value))
    }
}

impl<T: ?Sized> RefCell<T> {
    #[inline(always)]
    pub fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }

    #[inline(always)]
    #[track_caller]
    pub fn read(&self) -> Ref<'_, T> {
        self.0.borrow()
    }

    #[inline(always)]
    pub fn try_read(&self) -> Result<Ref<'_, T>, BorrowError> {
        self.0.try_borrow()
    }

    #[inline(always)]
    #[track_caller]
    pub fn write(this: &Mutable<Self>) -> RefMut<'_, T> {
        this.0.borrow_mut()
    }

    #[inline(always)]
    pub fn try_write(this: &Mutable<Self>) -> Result<RefMut<'_, T>, BorrowMutError> {
        this.0.try_borrow_mut()
    }
}

/// Convenience aliases for mutating [`RefCell`] methods.
impl<T: ?Sized> Mutable<RefCell<T>> {
    #[inline(always)]
    #[track_caller]
    pub fn write(&self) -> RefMut<'_, T> {
        self.0.borrow_mut()
    }

    #[inline(always)]
    #[track_caller]
    pub fn try_write(&self) -> Result<RefMut<'_, T>, BorrowMutError> {
        self.0.try_borrow_mut()
    }
}

/// Convenience aliases for mutating [`RefCell`] methods.
impl<'gc, T: 'gc + Collect> Gc<'gc, RefCell<T>> {
    #[inline]
    #[track_caller]
    pub fn write<'a>(&'a self, mc: MutationContext<'gc, '_>) -> RefMut<'a, T> {
        Gc::write_barrier(mc, *self);
        self.0.borrow_mut()
    }

    #[inline]
    #[track_caller]
    pub fn try_write<'a>(
        &'a self,
        mc: MutationContext<'gc, '_>,
    ) -> Result<RefMut<'a, T>, BorrowMutError> {
        Gc::write_barrier(mc, *self);
        self.0.try_borrow_mut()
    }
}
