use core::cell::Cell as StdCell;
use core::fmt::{self, Debug};

use crate::{Collect, CollectionContext, Gc, MutationContext};

use super::{CellLike, Mutable};

/// A GC-aware version of `std`'s [`Cell`](StdCell).
pub struct Cell<T: ?Sized>(StdCell<T>);

impl<T: Debug + Copy> Debug for Cell<T> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

unsafe impl<T: Collect + ?Sized> Collect for Cell<T> {
    fn trace(&self, cc: CollectionContext) {
        // The inner value is never mutated while the GC is running.
        let val = unsafe { &*self.0.as_ptr() };
        val.trace(cc);
    }
}

impl<T> CellLike for Cell<T> {
    type Target = T;

    #[inline]
    fn wrap(value: Self::Target) -> Self {
        Self(StdCell::new(value))
    }
}

impl<T> Cell<T> {
    #[inline(always)]
    pub fn new(value: T) -> Cell<T> {
        Self(StdCell::new(value))
    }

    #[inline(always)]
    pub fn get(&self) -> T
    where
        T: Copy,
    {
        self.0.get()
    }

    #[inline(always)]
    pub fn set(this: &Mutable<Self>, val: T) {
        this.0.set(val)
    }

    #[inline(always)]
    pub fn replace(this: &Mutable<Self>, val: T) -> T {
        this.0.replace(val)
    }

    #[inline(always)]
    pub fn into_inner(self) -> T {
        self.0.into_inner()
    }
}

impl<T: ?Sized> Cell<T> {
    #[inline(always)]
    pub fn get_mut(&mut self) -> &mut T {
        self.0.get_mut()
    }
}

/// Convenience aliases for mutating [`Cell`] methods.
impl<T> Mutable<Cell<T>> {
    #[inline(always)]
    pub fn set(&self, val: T) {
        self.0.set(val)
    }

    #[inline(always)]
    pub fn replace(&self, val: T) -> T {
        self.0.replace(val)
    }
}

/// Convenience aliases for mutating [`Cell`] methods.
impl<'gc, T: 'gc + Collect> Gc<'gc, Cell<T>> {
    #[inline(always)]
    pub fn set(&self, mc: MutationContext<'gc, '_>, val: T) {
        Gc::write_barrier(mc, *self);
        self.0.set(val)
    }

    #[inline(always)]
    pub fn replace(&self, mc: MutationContext<'gc, '_>, val: T) -> T {
        Gc::write_barrier(mc, *self);
        self.0.replace(val)
    }
}
