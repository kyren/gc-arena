#![no_std]
#![cfg_attr(miri, feature(strict_provenance))]

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;

#[doc(hidden)]
pub use gc_arena_derive::*;

mod arena;
mod collect;
mod collect_impl;
mod context;
mod dynamic_roots;
mod gc;
mod gc_cell;
mod gc_weak;
mod gc_weak_cell;
mod no_drop;
mod static_collect;
mod types;
mod unsize;

// Not public API.
#[doc(hidden)]
pub use unsize::__CoercePtrInternal;

pub use self::{
    arena::{rootless_arena, Arena, ArenaParameters, Root, Rootable},
    collect::Collect,
    context::{CollectionContext, MutationContext},
    dynamic_roots::{DynamicRoot, DynamicRootSet},
    gc::Gc,
    gc_cell::GcCell,
    gc_weak::GcWeak,
    gc_weak_cell::GcWeakCell,
    no_drop::MustNotImplDrop,
    static_collect::StaticCollect,
};
