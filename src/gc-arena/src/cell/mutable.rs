use core::ops::Deref;

#[cfg(doc)]
use crate::Gc;

/// A mutable value inside a GC'd object graph.
///
/// This type is only used by reference, and asserts that [`Gc::write_barrier`] was
/// called on the parent [`Gc`] pointer. This allows for further mutation without
/// needing to emit a write barrier again.
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
    pub unsafe fn assume(v: &T) -> &Mutable<T> {
        // Safe thanks to #[repr(transparent)]
        core::mem::transmute(v)
    }
}
