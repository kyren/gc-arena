#![no_std]

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;

#[doc(hidden)]
pub use gc_arena_derive::*;

mod arena;
mod collect;
mod collect_impl;
mod context;
mod gc;
mod gc_cell;
mod no_drop;
mod static_collect;
mod types;

pub use self::{
    arena::{rootless_arena, Arena, ArenaParameters, RootProvider},
    collect::Collect,
    context::{CollectionContext, MutationContext},
    gc::Gc,
    gc_cell::GcCell,
    no_drop::MustNotImplDrop,
    static_collect::StaticCollect,
};
