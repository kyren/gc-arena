//! GC-aware interior mutability types, enforcing the required write barriers.
//!
//! The core of this module is the [`Mutable<T>`] type, which allows distinguishing
//! shared references that allow interior mutability behind [`Gc`](crate::Gc)
//! pointers from those that do not. Combined with custom cell types implementing
//! [`Collect`](crate::Collect) and requiring `&Mutable<Self>` to perform mutation,
//! this provides safe mutation inside GC'd object graphs.
//!
//! The canonical way to safely obtain a `&Mutable<T>` reference is to call
//! [`Gc::mutate`](crate::Gc::mutate) on a [`Gc`](crate::Gc) pointer; this will emit
//! a write barrier, notifying the garbage collected [`Arena`](crate::Arena) that this
//! GC'd object is undergoing mutation and will need to be re-traced.

// Must be declared first to have clean documentation on `Mutable`.
mod mutable;

mod cell;
mod ref_cell;

pub use cell::Cell;
pub use mutable::{DerefNoGc, Mutable};
pub use ref_cell::RefCell;

/// Trait implemented by all GC-aware interior mutability types provided
/// in the [`cell`](self) module.
///
/// Implementing this trait allows for convenient allocation through the
/// [`Gc::allocate_cell`](crate::Gc::allocate_cell) method.
pub trait CellLike {
    /// The type this cell wraps.
    type Target;

    /// Wraps a value inside this cell.
    fn wrap(value: Self::Target) -> Self;
}
