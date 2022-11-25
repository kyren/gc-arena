//! GC-aware interior mutability types. When a type that may hold [`Gc`] pointers is mutated,
//! it may adopt new [`Gc`] pointers, and in order for this to be safe this must be accompanied
//! by a call to [`Gc::write_barrier`]. This module provide types that wrap a given `T` in such
//! a way that mutating the inner value is always accompanied by a call to [`Gc::write_barrier`].

use crate::Collect;

#[cfg(doc)]
use crate::Gc;

mod mutable;
mod ref_cell;

pub use mutable::Mutable;
pub use ref_cell::RefCell;

/// Trait implemented by all GC-aware interior mutability types provided
/// in this module.
///
/// Implementing this trait allows for convenient allocation through the
/// [`Gc::allocate_cell`] method.
pub trait CellLike: Collect {
    /// The type this cell wraps.
    type Target: Collect;

    /// Wraps a value inside this cell.
    fn wrap(value: Self::Target) -> Self;
}
