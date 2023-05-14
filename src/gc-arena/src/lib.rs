#![no_std]
#![cfg_attr(miri, feature(strict_provenance))]

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;

mod arena;
mod collect;
mod collect_impl;
mod context;
mod dynamic_roots;
mod gc;
mod gc_weak;
pub mod lock;
mod no_drop;
mod static_collect;
mod types;
mod unsize;

#[doc(hidden)]
pub use gc_arena_derive::*;

#[doc(hidden)]
pub use self::{arena::__DynRootable, no_drop::__MustNotImplDrop, unsize::__CoercePtrInternal};

pub use self::{
    arena::{rootless_arena, Arena, ArenaParameters, Root, Rootable},
    collect::Collect,
    context::{CollectionContext, MutationContext},
    dynamic_roots::{DynamicRoot, DynamicRootSet},
    gc::Gc,
    gc_weak::GcWeak,
    static_collect::StaticCollect,
};
