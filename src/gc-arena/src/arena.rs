use core::{f64, mem::ManuallyDrop, usize};

use crate::{
    context::{Context, MutationContext},
    Collect,
};

#[derive(Debug, Clone)]
pub struct ArenaParameters {
    pub(crate) pause_factor: f64,
    pub(crate) timing_factor: f64,
    pub(crate) min_sleep: usize,
}

/// Creates a default ArenaParameters with `pause_factor` set to 0.5, `timing_factor` set to 1.5,
/// and `min_sleep` set to 4096.
impl Default for ArenaParameters {
    fn default() -> ArenaParameters {
        const PAUSE_FACTOR: f64 = 0.5;
        const TIMING_FACTOR: f64 = 1.5;
        const MIN_SLEEP: usize = 4096;

        ArenaParameters {
            pause_factor: PAUSE_FACTOR,
            timing_factor: TIMING_FACTOR,
            min_sleep: MIN_SLEEP,
        }
    }
}

impl ArenaParameters {
    /// The garbage collector will wait until the live size reaches <current heap size> + <previous
    /// retained size> * `pause_multiplier` before beginning a new collection.  Must be >= 0.0,
    /// setting this to 0.0 causes the collector to never sleep longer than `min_sleep` before
    /// beginning a new collection.
    pub fn set_pause_factor(mut self, pause_factor: f64) -> ArenaParameters {
        assert!(pause_factor >= 0.0);
        self.pause_factor = pause_factor;
        self
    }

    /// The garbage collector will try and finish a collection by the time <current heap size> *
    /// `timing_factor` additional bytes are allocated.  For example, if the collection is started
    /// when the arena has 100KB live data, and the timing_multiplier is 1.0, the collector should
    /// finish its final phase of this collection after another 100KB has been allocated.  Must be
    /// >= 0.0, setting this to 0.0 causes the collector to behave like a stop-the-world collector.
    pub fn set_timing_factor(mut self, timing_factor: f64) -> ArenaParameters {
        assert!(timing_factor >= 0.0);
        self.timing_factor = timing_factor;
        self
    }

    /// The minimum allocation amount during sleep before the arena starts collecting again.  This
    /// is mostly useful when the heap is very small to prevent rapidly restarting collections.
    pub fn set_min_sleep(mut self, min_sleep: usize) -> ArenaParameters {
        self.min_sleep = min_sleep;
        self
    }
}

/// Creates a new "garbage collected arena" type.  The macro takes two parameters, the name you
/// would like to give the arena type, and the type of the arena root.  The root type must implement
/// the `Collect` trait, and be a type that takes a single generic lifetime parameter which is used
/// for any held `Gc` pointer types.
///
/// An eample:
/// ```
/// # use gc_arena::{Collect, Gc, make_arena};
/// #
/// # fn main() {
/// #[derive(Collect)]
/// #[collect(no_drop)]
/// struct MyRoot<'gc> {
///     ptr: Gc<'gc, i32>,
/// }
/// make_arena!(MyArena, MyRoot);
/// # }
/// ```
///
/// Garbage collected arenas allow for isolated sets of garbage collected objects with zero-overhead
/// garbage collected pointers.  It provides incremental mark and sweep garbage collection which
/// must be manually triggered outside the `mutate` method, and works best when units of work inside
/// `mutate` can be kept relatively small.  It is designed primarily to be a garbage collector for
/// scripting language runtimes.
///
/// The arena API is able to provide extremely cheap Gc pointers because it is based around
/// "generativity".  During construction and access, the root type is branded by a unique, invariant
/// lifetime `'gc` which ensures that `Gc` pointers must be contained inside the root object
/// hierarchy and cannot escape the arena callbacks or be smuggled inside another arena.  This way,
/// the arena can be sure that during mutation, all `Gc` pointers come from the arena we expect them
/// to come from, and that they're all either reachable from root or have been allocated during the
/// current `mutate` call.  When not inside the `mutate` callback, the arena knows that all `Gc`
/// pointers must be either reachable from root or they are unreachable and safe to collect.  In
/// this way, incremental garbage collection can be achieved (assuming "sufficiently small" calls to
/// `mutate`) that is both extremely safe and zero overhead vs what you would write in C with raw
/// pointers and manually ensuring that invariants are held.
#[macro_export]
macro_rules! make_arena {
    ($arena:ident, $root:ident) => {
        make_arena!(pub(self) $arena, $root);
    };

    ($vis:vis $arena:ident, $root:ident) => {
        // Instead of generating an impl of `RootProvider`, we use a trait object.
        // The projection `<R as RootProvider<'gc>>::Root` is used to obtain the root
        // type with the lifetime `'gc` applied
        // By using a trait object, we avoid the need to generate a new type for each
        // invocation of this macro, which would lead to name conflicts if the macro was
        // used multiple times in the same scope.
        $vis type $arena = $crate::Arena<dyn for<'a> $crate::RootProvider<'a, Root = $root<'a>>>;
    };
}

#[doc(hidden)]
/// A helper trait for the `make_arena` macro. Writing
/// <R as RootProvider<'gc>>::Root is similar to writing a GAT `R::Root<'gc>`,
/// but it works on older versions of Rust. Additionally, GATs currently
/// prevent a trait from being object-safe, which prevents the trait object
/// trick we use in `make_arena` from working.
pub trait RootProvider<'a> {
    type Root: Collect;
}

pub struct Arena<R: for<'a> RootProvider<'a> + ?Sized> {
    context: Context,
    root: ManuallyDrop<<R as RootProvider<'static>>::Root>,
}

impl<R: for<'a> RootProvider<'a> + ?Sized> Arena<R> {
    /// Create a new arena with the given garbage collector tuning parameters.  You must
    /// provide a closure that accepts a `MutationContext` and returns the appropriate root.
    /// The held root type is immutable inside the arena, in order to provide mutation, you
    /// must use `GcCell` types inside the root.
    #[allow(unused)]
    pub fn new<F>(arena_parameters: crate::ArenaParameters, f: F) -> Arena<R>
    where
        F: for<'gc> FnOnce(crate::MutationContext<'gc, '_>) -> <R as RootProvider<'gc>>::Root,
    {
        unsafe {
            let context = Context::new(arena_parameters);
            // Note - we transmute the `MutationContext` to a `'static` lifetime here,
            // instead of transmuting the root type returned by `f`. Transmuting the root
            // type is allowed in nightly versions of rust
            // (see https://github.com/rust-lang/rust/pull/101520#issuecomment-1252016235)
            // but is not yet stable. Transmuting the `MutationContext` is completely invisible
            // to the callback `f` (since it needs to handle an arbitrary lifetime),
            // and lets us stay compatible with older versions of Rust
            let mutation_context: MutationContext<'static, '_> =
                ::core::mem::transmute(context.mutation_context());
            let root: <R as RootProvider<'static>>::Root = f(mutation_context);
            Arena {
                context: context,
                root: ::core::mem::ManuallyDrop::new(root),
            }
        }
    }

    /// Similar to `new`, but allows for constructor that can fail.
    #[allow(unused)]
    pub fn try_new<F, E>(arena_parameters: crate::ArenaParameters, f: F) -> Result<Arena<R>, E>
    where
        F: for<'gc> FnOnce(
            crate::MutationContext<'gc, '_>,
        ) -> Result<<R as RootProvider<'gc>>::Root, E>,
    {
        unsafe {
            let context = Context::new(arena_parameters);
            let mutation_context: MutationContext<'static, '_> =
                ::core::mem::transmute(context.mutation_context());
            let root: <R as RootProvider<'static>>::Root = f(mutation_context)?;
            Ok(Arena {
                context: context,
                root: ::core::mem::ManuallyDrop::new(root),
            })
        }
    }

    /// The primary means of interacting with a garbage collected arena.  Accepts a callback
    /// which receives a `MutationContext` and a reference to the root, and can return any
    /// non garbage collected value.  The callback may "mutate" any part of the object graph
    /// during this call, but no garbage collection will take place during this method.
    #[allow(unused)]
    #[inline]
    pub fn mutate<F, T>(&self, f: F) -> T
    where
        F: for<'gc> FnOnce(crate::MutationContext<'gc, '_>, &<R as RootProvider<'gc>>::Root) -> T,
    {
        unsafe {
            f(
                self.context.mutation_context(),
                ::core::mem::transmute::<&<R as RootProvider<'static>>::Root, _>(&*self.root),
            )
        }
    }

    /// Return total currently used memory
    #[allow(unused)]
    #[inline]
    pub fn total_allocated(&self) -> usize {
        self.context.total_allocated()
    }

    /// When the garbage collector is not sleeping, all allocated objects cause the arena to
    /// accumulate "allocation debt".  This debt is then be used to time incremental garbage
    /// collection based on the tuning parameters set in `ArenaParameters`.  The allocation
    /// debt is measured in bytes, but will generally increase at a rate faster than that of
    /// allocation so that collection will always complete.
    #[allow(unused)]
    #[inline]
    pub fn allocation_debt(&self) -> f64 {
        self.context.allocation_debt()
    }

    /// Run the incremental garbage collector until the allocation debt is <= 0.0.  There is
    /// no minimum unit of work enforced here, so it may be faster to only call this method
    /// when the allocation debt is above some threshold.
    #[allow(unused)]
    #[inline]
    pub fn collect_debt(&mut self) {
        unsafe {
            let debt = self.context.allocation_debt();
            if debt > 0.0 {
                self.context.do_collection(&*self.root, debt);
            }
        }
    }

    /// Run the current garbage collection cycle to completion, stopping once the garbage
    /// collector has entered the sleeping phase.  If the garbage collector is currently
    /// sleeping, starts a new cycle and runs that cycle to completion.
    #[allow(unused)]
    pub fn collect_all(&mut self) {
        self.context.wake();
        unsafe {
            self.context
                .do_collection(&*self.root, ::core::f64::INFINITY);
        }
    }
}

impl<R: for<'a> RootProvider<'a> + ?Sized> Drop for Arena<R> {
    fn drop(&mut self) {
        unsafe {
            ::core::mem::ManuallyDrop::drop(&mut self.root);
        }
    }
}

/// Create a temporary arena without a root object and perform the given operation on it.  No
/// garbage collection will be done until the very end of the call, at which point all allocations
/// will be collected.
pub fn rootless_arena<F, R>(f: F) -> R
where
    F: for<'gc> FnOnce(MutationContext<'gc, '_>) -> R,
{
    unsafe {
        let context = Context::new(ArenaParameters::default());
        f(context.mutation_context())
    }
}
