#![no_std]
#![cfg_attr(miri, feature(strict_provenance))]

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;

pub mod arena;
pub mod barrier;
mod collect;
mod collect_impl;
mod context;
pub mod dynamic_roots;
mod gc;
mod gc_weak;
pub mod lock;
pub mod metrics;
mod no_drop;
mod static_collect;
mod types;
mod unsize;

#[cfg(feature = "allocator-api2")]
pub mod allocator_api;

#[cfg(feature = "hashbrown")]
mod hashbrown;

#[doc(hidden)]
pub use gc_arena_derive::*;

#[doc(hidden)]
pub use self::{arena::__DynRootable, no_drop::__MustNotImplDrop, unsize::__CoercePtrInternal};

pub use self::{
    arena::{Arena, Rootable},
    collect::Collect,
    context::{Collection, Finalization, Mutation},
    dynamic_roots::{DynamicRoot, DynamicRootSet},
    gc::Gc,
    gc_weak::GcWeak,
    lock::{GcLock, GcRefLock, Lock, RefLock},
    static_collect::Static,
};
