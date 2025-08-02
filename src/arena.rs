use alloc::boxed::Box;
use core::marker::PhantomData;

use crate::{
    Collect,
    context::{Context, Finalization, Mutation, Phase, RunUntil, Stop},
    metrics::Metrics,
};

/// A trait that produces a [`Collect`]-able type for the given lifetime. This is used to produce
/// the root [`Collect`] instance in an [`Arena`].
///
/// In order to use an implementation of this trait in an [`Arena`], it must implement
/// `Rootable<'a>` for *any* possible `'a`. This is necessary so that the `Root` types can be
/// branded by the unique, invariant lifetimes that makes an `Arena` sound.
pub trait Rootable<'a> {
    type Root: ?Sized + 'a;
}

/// A marker type used by the `Rootable!` macro instead of a bare trait object.
///
/// Prevents having to include extra ?Sized bounds on every `for<'a> Rootable<'a>`.
#[doc(hidden)]
pub struct __DynRootable<T: ?Sized>(PhantomData<T>);

impl<'a, T: ?Sized + Rootable<'a>> Rootable<'a> for __DynRootable<T> {
    type Root = <T as Rootable<'a>>::Root;
}

/// A convenience macro for quickly creating a type that implements `Rootable`.
///
/// The macro takes a single argument, which should be a generic type with elided lifetimes.
/// When used as a root object, every instance of the elided lifetime will be replaced with
/// the branding lifetime.
///
/// ```
/// # use gc_arena::{Arena, Collect, Gc, Rootable};
/// #
/// # fn main() {
/// #[derive(Collect)]
/// #[collect(no_drop)]
/// struct MyRoot<'gc> {
///     ptr: Gc<'gc, i32>,
/// }
///
/// type MyArena = Arena<Rootable![MyRoot<'_>]>;
///
/// // If desired, the branding lifetime can also be explicitely named:
/// type MyArena2 = Arena<Rootable!['gc => MyRoot<'gc>]>;
/// # }
/// ```
///
/// The macro can also be used to create implementations of `Rootable` that use other generic
/// parameters, though in complex cases it may be better to implement `Rootable` directly.
///
/// ```
/// # use gc_arena::{Arena, Collect, Gc, Rootable};
/// #
/// # fn main() {
/// #[derive(Collect)]
/// #[collect(no_drop)]
/// struct MyGenericRoot<'gc, T> {
///     ptr: Gc<'gc, T>,
/// }
///
/// type MyGenericArena<T> = Arena<Rootable![MyGenericRoot<'_, T>]>;
/// # }
/// ```
#[macro_export]
macro_rules! Rootable {
    ($gc:lifetime => $root:ty) => {
        // Instead of generating an impl of `Rootable`, we use a trait object. Thus, we avoid the
        // need to generate a new type for each invocation of this macro.
        $crate::__DynRootable::<dyn for<$gc> $crate::Rootable<$gc, Root = $root>>
    };
    ($root:ty) => {
        $crate::Rootable!['__gc => $crate::__unelide_lifetimes!('__gc; $root)]
    };
}

/// A helper type alias for a `Rootable::Root` for a specific lifetime.
pub type Root<'a, R> = <R as Rootable<'a>>::Root;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum CollectionPhase {
    /// The arena is done with a collection cycle and is waiting to be restarted.
    Sleeping,
    /// The arena is currently tracing objects from the root to determine reachability.
    Marking,
    /// The arena has finished tracing, all reachable objects are marked. This may transition
    /// back to `Marking` if write barriers occur.
    Marked,
    /// The arena has determined a set of unreachable objects and has started freeing them. At this
    /// point, marking is no longer taking place so the root may have reachable, unmarked pointers.
    Sweeping,
}

/// A generic, garbage collected arena.
///
/// Garbage collected arenas allow for isolated sets of garbage collected objects with zero-overhead
/// garbage collected pointers. It provides incremental mark and sweep garbage collection which
/// must be manually triggered outside the `mutate` method, and works best when units of work inside
/// `mutate` can be kept relatively small. It is designed primarily to be a garbage collector for
/// scripting language runtimes.
///
/// The arena API is able to provide extremely cheap Gc pointers because it is based around
/// "generativity". During construction and access, the root type is branded by a unique, invariant
/// lifetime `'gc` which ensures that `Gc` pointers must be contained inside the root object
/// hierarchy and cannot escape the arena callbacks or be smuggled inside another arena. This way,
/// the arena can be sure that during mutation, all `Gc` pointers come from the arena we expect
/// them to come from, and that they're all either reachable from root or have been allocated during
/// the current `mutate` call. When not inside the `mutate` callback, the arena knows that all `Gc`
/// pointers must be either reachable from root or they are unreachable and safe to collect. In
/// this way, incremental garbage collection can be achieved (assuming "sufficiently small" calls
/// to `mutate`) that is both extremely safe and zero overhead vs what you would write in C with raw
/// pointers and manually ensuring that invariants are held.
pub struct Arena<R>
where
    R: for<'a> Rootable<'a>,
{
    context: Box<Context>,
    root: Root<'static, R>,
}

impl<R> Arena<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized,
{
    /// Create a new arena with the given garbage collector tuning parameters. You must provide a
    /// closure that accepts a `&Mutation<'gc>` and returns the appropriate root.
    pub fn new<F>(f: F) -> Arena<R>
    where
        F: for<'gc> FnOnce(&'gc Mutation<'gc>) -> Root<'gc, R>,
    {
        unsafe {
            let context = Box::new(Context::new());
            // Note - we cast the `&Mutation` to a `'static` lifetime here,
            // instead of transmuting the root type returned by `f`. Transmuting the root
            // type is allowed in nightly versions of rust
            // (see https://github.com/rust-lang/rust/pull/101520#issuecomment-1252016235)
            // but is not yet stable. Casting the `&Mutation` is completely invisible
            // to the callback `f` (since it needs to handle an arbitrary lifetime),
            // and lets us stay compatible with older versions of Rust
            let mc: &'static Mutation<'_> = &*(context.mutation_context() as *const _);
            let root: Root<'static, R> = f(mc);
            Arena { context, root }
        }
    }

    /// Similar to `new`, but allows for constructor that can fail.
    pub fn try_new<F, E>(f: F) -> Result<Arena<R>, E>
    where
        F: for<'gc> FnOnce(&'gc Mutation<'gc>) -> Result<Root<'gc, R>, E>,
    {
        unsafe {
            let context = Box::new(Context::new());
            let mc: &'static Mutation<'_> = &*(context.mutation_context() as *const _);
            let root: Root<'static, R> = f(mc)?;
            Ok(Arena { context, root })
        }
    }

    #[inline]
    pub fn map_root<R2>(
        mut self,
        f: impl for<'gc> FnOnce(&'gc Mutation<'gc>, Root<'gc, R>) -> Root<'gc, R2>,
    ) -> Arena<R2>
    where
        R2: for<'a> Rootable<'a>,
        for<'a> Root<'a, R2>: Sized,
    {
        self.context.root_barrier();
        let new_root: Root<'static, R2> = unsafe {
            let mc: &'static Mutation<'_> = &*(self.context.mutation_context() as *const _);
            f(mc, self.root)
        };
        Arena {
            context: self.context,
            root: new_root,
        }
    }

    #[inline]
    pub fn try_map_root<R2, E>(
        mut self,
        f: impl for<'gc> FnOnce(&'gc Mutation<'gc>, Root<'gc, R>) -> Result<Root<'gc, R2>, E>,
    ) -> Result<Arena<R2>, E>
    where
        R2: for<'a> Rootable<'a>,
        for<'a> Root<'a, R2>: Sized,
    {
        self.context.root_barrier();
        let new_root: Root<'static, R2> = unsafe {
            let mc: &'static Mutation<'_> = &*(self.context.mutation_context() as *const _);
            f(mc, self.root)?
        };
        Ok(Arena {
            context: self.context,
            root: new_root,
        })
    }
}

impl<R> Arena<R>
where
    R: for<'a> Rootable<'a>,
{
    /// The primary means of interacting with a garbage collected arena. Accepts a callback which
    /// receives a `&Mutation<'gc>` and a reference to the root, and can return any non garbage
    /// collected value. The callback may "mutate" any part of the object graph during this call,
    /// but no garbage collection will take place during this method.
    #[inline]
    pub fn mutate<F, T>(&self, f: F) -> T
    where
        F: for<'gc> FnOnce(&'gc Mutation<'gc>, &'gc Root<'gc, R>) -> T,
    {
        unsafe {
            let mc: &'static Mutation<'_> = &*(self.context.mutation_context() as *const _);
            let root: &'static Root<'_, R> = &*(&self.root as *const _);
            f(mc, root)
        }
    }

    /// An alternative version of [`Arena::mutate`] which allows mutating the root set, at the
    /// cost of an extra write barrier.
    #[inline]
    pub fn mutate_root<F, T>(&mut self, f: F) -> T
    where
        F: for<'gc> FnOnce(&'gc Mutation<'gc>, &'gc mut Root<'gc, R>) -> T,
    {
        self.context.root_barrier();
        unsafe {
            let mc: &'static Mutation<'_> = &*(self.context.mutation_context() as *const _);
            let root: &'static mut Root<'_, R> = &mut *(&mut self.root as *mut _);
            f(mc, root)
        }
    }

    #[inline]
    pub fn metrics(&self) -> &Metrics {
        self.context.metrics()
    }

    #[inline]
    pub fn collection_phase(&self) -> CollectionPhase {
        match self.context.phase() {
            Phase::Mark => {
                if self.context.gray_remaining() {
                    CollectionPhase::Marking
                } else {
                    CollectionPhase::Marked
                }
            }
            Phase::Sweep => CollectionPhase::Sweeping,
            Phase::Sleep => CollectionPhase::Sleeping,
            Phase::Drop => unreachable!(),
        }
    }
}

impl<R> Arena<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Collect<'a>,
{
    /// Run incremental garbage collection until the allocation debt is zero.
    ///
    /// This will run through ALL phases of the collection cycle until the debt is zero, including
    /// implicitly finishing the current cycle and starting a new one (transitioning from
    /// [`CollectionPhase::Sweeping`] to [`CollectionPhase::Sleeping`]). Since this method runs
    /// until debt is zero with no guaranteed return at any specific transition, you may need to use
    /// other methods like [`Arena::mark_debt`] and [`Arena::cycle_debt`] if you need to keep close
    /// track of the current collection phase.
    ///
    /// There is no minimum unit of work enforced here, so it may be faster to only call this method
    /// when the allocation debt is above some minimum threshold.
    #[inline]
    pub fn collect_debt(&mut self) {
        unsafe {
            self.context
                .do_collection(&self.root, RunUntil::PayDebt, Stop::Full);
        }
    }

    /// Run only the *marking* part of incremental garbage collection until allocation debt is zero.
    ///
    /// This does *not* transition collection past the [`CollectionPhase::Marked`]
    /// phase. Does nothing if the collection phase is [`CollectionPhase::Marked`] or
    /// [`CollectionPhase::Sweeping`], otherwise acts like [`Arena::collect_debt`].
    ///
    /// If this method stops because the arena is now fully marked (the collection phase is
    /// [`CollectionPhase::Marked`]), then a [`MarkedArena`] object will be returned to allow
    /// you to examine the state of the fully marked arena.
    #[inline]
    pub fn mark_debt(&mut self) -> Option<MarkedArena<'_, R>> {
        unsafe {
            self.context
                .do_collection(&self.root, RunUntil::PayDebt, Stop::FullyMarked);
        }

        if self.context.phase() == Phase::Mark && !self.context.gray_remaining() {
            Some(MarkedArena(self))
        } else {
            None
        }
    }

    /// Runs ALL of the remaining *marking* part of the current garbage collection cycle.
    ///
    /// Similarly to [`Arena::mark_debt`], this does not transition collection past the
    /// [`CollectionPhase::Marked`] phase, and does nothing if the collector is currently in the
    /// [`CollectionPhase::Marked`] phase or the [`CollectionPhase::Sweeping`] phase.
    ///
    /// This method will always fully mark the arena and return a [`MarkedArena`] object as long as
    /// the current phase is not [`CollectionPhase::Sweeping`].
    #[inline]
    pub fn finish_marking(&mut self) -> Option<MarkedArena<'_, R>> {
        unsafe {
            self.context
                .do_collection(&self.root, RunUntil::Stop, Stop::FullyMarked);
        }

        if self.context.phase() == Phase::Mark && !self.context.gray_remaining() {
            Some(MarkedArena(self))
        } else {
            None
        }
    }

    /// Run the *current* collection cycle until the allocation debt is zero.
    ///
    /// This is nearly identical to the [`Arena::collect_debt`] method, except it
    /// *always* returns immediately when a cycle is finished (when phase transitions
    /// to [`CollectionPhase::Sleeping`]), and will never transition directly from
    /// [`CollectionPhase::Sweeping`] to [`CollectionPhase::Marking`] within a single call, even if
    /// there is enough outstanding debt to do so.
    ///
    /// This mostly only important when the user of an `Arena` needs to closely track collection
    /// phases, otherwise [`Arena::collect_debt`] simpler to use.
    #[inline]
    pub fn cycle_debt(&mut self) {
        unsafe {
            self.context
                .do_collection(&self.root, RunUntil::PayDebt, Stop::FinishCycle);
        }
    }

    /// Run the current garbage collection cycle to completion, stopping once garbage collection
    /// has entered the [`CollectionPhase::Sleeping`] phase. If the collector is currently sleeping,
    /// then this restarts the collector and performs a full collection before transitioning back to
    /// the sleep phase.
    #[inline]
    pub fn finish_cycle(&mut self) {
        unsafe {
            self.context
                .do_collection(&self.root, RunUntil::Stop, Stop::FinishCycle);
        }
    }
}

pub struct MarkedArena<'a, R: for<'b> Rootable<'b>>(&'a mut Arena<R>);

impl<'a, R> MarkedArena<'a, R>
where
    R: for<'b> Rootable<'b>,
    for<'b> Root<'b, R>: Collect<'b>,
{
    /// Examine the state of a fully marked arena.
    ///
    /// Allows you to determine whether `GcWeak` pointers are "dead" (aka, soon-to-be-dropped) and
    /// potentially resurrect them for this cycle.
    ///
    /// Note that the arena is guaranteed to be *fully marked* only at the *beginning* of this
    /// callback, any mutation that resurrects a pointer or triggers a write barrier can immediately
    /// invalidate this.
    #[inline]
    pub fn finalize<F, T>(self, f: F) -> T
    where
        F: for<'gc> FnOnce(&'gc Finalization<'gc>, &'gc Root<'gc, R>) -> T,
    {
        unsafe {
            let mc: &'static Finalization<'_> =
                &*(self.0.context.finalization_context() as *const _);
            let root: &'static Root<'_, R> = &*(&self.0.root as *const _);
            f(mc, root)
        }
    }

    /// Immediately transition the arena out of [`CollectionPhase::Marked`] to
    /// [`CollectionPhase::Sweeping`].
    #[inline]
    pub fn start_sweeping(self) {
        unsafe {
            self.0
                .context
                .do_collection(&self.0.root, RunUntil::Stop, Stop::AtSweep);
        }
        assert_eq!(self.0.context.phase(), Phase::Sweep);
    }
}

/// Create a temporary arena without a root object and perform the given operation on it.
///
/// No garbage collection will be done until the very end of the call, at which point all
/// allocations will be collected.
///
/// This is a convenience function that makes it a little easier to quickly test code that uses
/// `gc-arena`, it is not very useful on its own.
pub fn rootless_mutate<F, R>(f: F) -> R
where
    F: for<'gc> FnOnce(&'gc Mutation<'gc>) -> R,
{
    unsafe {
        let context = Context::new();
        f(context.mutation_context())
    }
}
