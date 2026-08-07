#![no_std]

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;

pub mod arena;
pub mod barrier;
pub mod zst_cache;
#[macro_use]
pub mod collect;
mod collect_impl;
mod context;
pub mod dynamic_roots;
pub mod gc;
mod gc_ptr;
mod gc_weak;
pub mod lock;
pub mod meta;
pub mod metrics;
mod no_drop;
pub mod slice;
mod static_wrapper;
mod types;
mod unsize;

#[cfg(feature = "enum-map")]
mod enum_map;

#[cfg(feature = "hashbrown")]
mod hashbrown;

#[cfg(feature = "indexmap")]
mod indexmap;

#[cfg(feature = "slotmap")]
mod slotmap;

#[cfg(feature = "smallvec")]
mod smallvec;

#[doc(hidden)]
pub use gc_arena_derive::__unelide_lifetimes;

#[doc(hidden)]
pub use self::{arena::__DynRootable, no_drop::__MustNotImplDrop, unsize::__CoercePtrInternal};

pub use self::{
    arena::{Arena, Rootable},
    collect::Collect,
    context::{Finalization, Mutation},
    dynamic_roots::{DynamicRoot, DynamicRootSet},
    gc::{Gc, GcBuilder, GcFat, GcThin},
    gc_weak::GcWeak,
    lock::{GcLock, GcRefLock, Lock, RefLock},
    slice::{
        GcSlice, GcSliceBuilder, GcSliceWithHeader, GcSliceWithHeaderBuilder, GcThinSlice,
        GcThinSliceWithHeader, SliceWithHeader,
    },
    static_wrapper::Static,
};
