use core::{mem::ManuallyDrop, ops::Deref, panic::AssertUnwindSafe, pin::Pin};

use alloc::{boxed::Box, rc::Rc, sync::Arc, vec::Vec};

#[cfg(doc)]
use crate::Gc;

/// An (interiorly-)mutable reference inside a GC'd object graph.
///
/// This type can only exist behind a shared reference; see the [module documentation](super)
/// for more details.
#[repr(transparent)]
pub struct Mutable<T: ?Sized>(T);

impl<T: ?Sized> Deref for Mutable<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: ?Sized> Mutable<T> {
    /// Asserts that the given reference can be safely mutated.
    ///
    /// # Safety
    /// In order to maintain the invariants of the garbage collector, no new [`Gc`] pointers
    /// may be adopted by the referenced value as a result of the interior mutability enabled
    /// by this wrapper, unless [`Gc::write_barrier`] is invoked manually on the parent [`Gc`]
    /// pointer before the next collection is triggered.
    #[inline(always)]
    pub unsafe fn assume(v: &T) -> &Self {
        // Safe thanks to #[repr(transparent)]
        core::mem::transmute(v)
    }

    /// Calls the [`Deref`] impl of the wrapped reference.
    ///
    /// The [`DerefNoGc`] bound (which is satisfied by all the common `std` pointer types)
    /// guarantees that this is sound.
    #[inline(always)]
    pub fn as_deref(&self) -> &Mutable<T::Target>
    where
        T: DerefNoGc,
    {
        unsafe { Mutable::assume(self.0.deref()) }
    }
}

/// Marker trait asserting that the type's [`Deref`] implementation never traverses [`Gc`] pointers.
pub unsafe trait DerefNoGc: Deref {}

// A bunch of implementations for std types.
unsafe impl<T: ?Sized> DerefNoGc for &'_ T {}
unsafe impl<T: ?Sized> DerefNoGc for &'_ mut T {}
unsafe impl<P: DerefNoGc> DerefNoGc for Pin<P> {}
unsafe impl<T: ?Sized> DerefNoGc for Box<T> {}
unsafe impl<T: ?Sized> DerefNoGc for Rc<T> {}
unsafe impl<T: ?Sized> DerefNoGc for Arc<T> {}
unsafe impl<T> DerefNoGc for Vec<T> {}
unsafe impl<T> DerefNoGc for AssertUnwindSafe<T> {}
unsafe impl<T: ?Sized> DerefNoGc for ManuallyDrop<T> {}
