use alloc::boxed::Box;
use core::{f64, marker::PhantomData};

use crate::{
    context::{Context, Finalization, Mutation, Phase},
    metrics::Metrics,
    Collect,
};

/// A trait that produces a [`Collect`]-able type for the given lifetime. This is used to produce
/// the root [`Collect`] instance in an [`Arena`].
///
/// In order to use an implementation of this trait in an [`Arena`], it must implement
/// `Rootable<'a>` for *any* possible `'a`. This is necessary so that the `Root` types can be
/// branded by the unique, invariant lifetimes that makes an `Arena` sound.
pub trait Rootable<'a>: 'static {
    type Root: Collect + 'a;
}

impl<'a> Rootable<'a> for () {
    type Root = ();
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
/// # use gc_arena::{Arena, Collect, Gc, Rootable, StaticCollect};
/// #
/// # fn main() {
/// #[derive(Collect)]
/// #[collect(no_drop)]
/// struct MyGenericRoot<'gc, T: 'static> {
///     ptr: Gc<'gc, StaticCollect<T>>,
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CollectionPhase {
    /// The arena is done with a collection cycle and is waiting to be restarted.
    Sleeping,
    /// The arena is currently tracing objects from the root to determine reachability.
    Marking,
    /// The arena has finished tracing, all reachable objects are marked. This may transition
    /// back to `Marking` if write barriers occur.
    Marked,
    /// The arena has determined a set of unreachable objects and has started collecting them.
    /// At this point, marking is no longer taking place so the root may have reachable, unmarked
    /// pointers
    Collecting,
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
pub struct Arena<R: for<'a> Rootable<'a>> {
    context: Box<Context>,
    root: Root<'static, R>,
}

impl<R: for<'a> Rootable<'a>> Arena<R> {
    /// Create a new arena with the given garbage collector tuning parameters. You must provide a
    /// closure that accepts a `&Mutation<'gc>` and returns the appropriate root.
    pub fn new<F>(f: F) -> Arena<R>
    where
        F: for<'gc> FnOnce(&'gc Mutation<'gc>) -> Root<'gc, R>,
    {
        Self::try_new::<_, ()>(move |mc| Ok(f(mc))).unwrap()
    }

    /// Similar to `new`, but allows for constructor that can fail.
    pub fn try_new<F, E>(f: F) -> Result<Arena<R>, E>
    where
        F: for<'gc> FnOnce(&'gc Mutation<'gc>) -> Result<Root<'gc, R>, E>,
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
            let root: Root<'static, R> = f(mc)?;
            Ok(Arena { context, root })
        }
    }

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
    pub fn map_root<R2: for<'a> Rootable<'a>>(
        self,
        f: impl for<'gc> FnOnce(&'gc Mutation<'gc>, Root<'gc, R>) -> Root<'gc, R2>,
    ) -> Arena<R2> {
        Self::try_map_root(self, move |mc, root| Result::<_, ()>::Ok(f(mc, root))).unwrap()
    }

    #[inline]
    pub fn try_map_root<R2: for<'a> Rootable<'a>, E>(
        self,
        f: impl for<'gc> FnOnce(&'gc Mutation<'gc>, Root<'gc, R>) -> Result<Root<'gc, R2>, E>,
    ) -> Result<Arena<R2>, E> {
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
            Phase::Sweep => CollectionPhase::Collecting,
            Phase::Sleep => CollectionPhase::Sleeping,
            Phase::Drop => unreachable!(),
        }
    }

    /// Run incremental garbage collection until the allocation debt is <= 0.0.
    ///
    /// There is no minimum unit of work enforced here, so it may be faster to only call this method
    /// when the allocation debt is above some threshold.
    ///
    /// This method will always return at least once when collection enters the `Sleeping` phase,
    /// i.e. it will never transition from the `Collecting` phase to the `Marking` phase without
    /// returning in-between.
    #[inline]
    pub fn collect_debt(&mut self) {
        unsafe {
            self.context.do_collection(&self.root, 0.0, false);
        }
    }

    /// Run only the *marking* part of incremental garbage collection until allocation debt is
    /// <= 0.0.
    ///
    /// This does *not* transition collection past the `Marked` phase. Does nothing if the
    /// collection phase is `Marked` or `Collecting`, otherwise acts like `Arena::collect_debt`.
    #[inline]
    pub fn mark_debt(&mut self) -> Option<MarkedArena<'_, R>> {
        if matches!(self.context.phase(), Phase::Mark | Phase::Sleep) {
            unsafe {
                self.context.do_collection(&self.root, 0.0, true);
            }
        }

        debug_assert!(self.context.phase() == Phase::Mark);
        if !self.context.gray_remaining() {
            Some(MarkedArena::new(self))
        } else {
            None
        }
    }

    /// Run the current garbage collection cycle to completion, stopping once garbage collection
    /// has restarted in the sleep phase. If the collector is currently in the sleep phase, this
    /// restarts the collection and performs a full collection before transitioning back to the
    /// sleep phase.
    #[inline]
    pub fn collect_all(&mut self) {
        unsafe {
            self.context
                .do_collection(&self.root, f64::NEG_INFINITY, false);
        }
    }

    /// Runs all of the remaining *marking* part of the current garbage collection cycle.
    ///
    /// Similarly to `Arena::mark_debt`, this does not transition collection  past the `Marked`
    /// phase, and does nothing if the collector is currently in the `Marked` phase or the
    /// `Collecting` phase.
    #[inline]
    pub fn mark_all(&mut self) -> Option<MarkedArena<'_, R>> {
        if matches!(self.context.phase(), Phase::Mark | Phase::Sleep) {
            unsafe {
                self.context
                    .do_collection(&self.root, f64::NEG_INFINITY, true);
            }
        }

        debug_assert!(self.context.phase() == Phase::Mark);
        if !self.context.gray_remaining() {
            Some(MarkedArena::new(self))
        } else {
            None
        }
    }
}

/// Proxy type that indicates that an arena is fully marked and allows you to examine its marked
/// state.
///
/// Callbacks get a `Finalization` context type which allows you to determine whether `GcWeak`
/// pointers are "dead" (aka, soon-to-be-dropped) and potentially resurrect them for this GC cycle.
///
/// The `Finalization` context type also allows unrestricted *mutation* (it derefs to `Mutation`),
/// which means that the arena is only known to be fully marked at the *beginning* of callbacks
/// provided here. Methods are also provided to examine / mutate the arena, then immediately
/// *re-mark* it to re-examine it.
///
/// Contains ephemeral state of type `S` which is of the `'gc` lifetime. The purpose of this state
/// is to keep track of temporary values only used during finalization, and it is *not* considered
/// owned by the arena "root". This allows keeping logical state during finalization that does not
/// itself incur the cost of object re-marking. This is safe to do, since a `MarkedArena` is never
/// in the `Collecting` phase, and thus cannot ever free pointers held by the ephemeral state.
pub struct MarkedArena<'a, R, S = ()>(&'a mut Arena<R>, Root<'static, S>)
where
    R: for<'b> Rootable<'b>,
    S: for<'b> Rootable<'b>;

impl<'a, R> MarkedArena<'a, R>
where
    R: for<'b> Rootable<'b>,
{
    fn new(arena: &'a mut Arena<R>) -> Self {
        debug_assert!(arena.context.phase() == Phase::Mark);
        debug_assert!(!arena.context.gray_remaining());
        Self(arena, ())
    }
}

impl<'a, R, S> MarkedArena<'a, R, S>
where
    R: for<'b> Rootable<'b>,
    S: for<'b> Rootable<'b>,
{
    /// Examine the state of the fully marked arena, potentially mutating it, then immediately fully
    /// re-mark the arena.
    #[inline]
    pub fn step(
        self,
        f: impl for<'gc> FnOnce(&'gc Finalization<'gc>, &'gc Root<'gc, R>, &mut Root<'gc, S>),
    ) -> MarkedArena<'a, R, S> {
        Self::try_step(self, move |fc, root, state| {
            Result::<_, ()>::Ok(f(fc, root, state))
        })
        .unwrap()
    }

    /// A version of [`MarkedArena::step`] with failure.
    #[inline]
    pub fn try_step<E>(
        self,
        f: impl for<'gc> FnOnce(
            &'gc Finalization<'gc>,
            &'gc Root<'gc, R>,
            &mut Root<'gc, S>,
        ) -> Result<(), E>,
    ) -> Result<MarkedArena<'a, R, S>, E> {
        self.try_map_state(move |fc, root, mut state| {
            f(fc, root, &mut state)?;
            Ok(state)
        })
    }

    /// A version of [`MarkedArena::step`] that changes the held state type.
    #[inline]
    pub fn map_state<S2: for<'b> Rootable<'b>>(
        self,
        f: impl for<'gc> FnOnce(
            &'gc Finalization<'gc>,
            &'gc Root<'gc, R>,
            Root<'gc, S>,
        ) -> Root<'gc, S2>,
    ) -> MarkedArena<'a, R, S2> {
        Self::try_map_state(self, move |fc, root, state| {
            Result::<_, ()>::Ok(f(fc, root, state))
        })
        .unwrap()
    }

    /// A version of [`MarkedArena::map_state`] with failure.
    #[inline]
    pub fn try_map_state<S2: for<'b> Rootable<'b>, E>(
        self,
        f: impl for<'gc> FnOnce(
            &'gc Finalization<'gc>,
            &'gc Root<'gc, R>,
            Root<'gc, S>,
        ) -> Result<Root<'gc, S2>, E>,
    ) -> Result<MarkedArena<'a, R, S2>, E> {
        unsafe {
            let mc: &'static Finalization<'_> =
                &*(self.0.context.finalization_context() as *const _);
            let root: &'static Root<'_, R> = &*(&self.0.root as *const _);
            let s2 = f(mc, root, self.1)?;
            let arena = self.0.mark_all().unwrap().0;
            Ok(MarkedArena(arena, s2))
        }
    }

    /// Examine the state of the fully marked arena, potentially mutating it.
    ///
    /// Consumes the `MarkedArena` as the arena is no longer left in a fully marked state.
    #[inline]
    pub fn finish<F, T>(self, f: F) -> T
    where
        F: for<'gc> FnOnce(&'gc Finalization<'gc>, &'gc Root<'gc, R>, Root<'gc, S>) -> T,
    {
        unsafe {
            let mc: &'static Finalization<'_> =
                &*(self.0.context.finalization_context() as *const _);
            let root: &'static Root<'_, R> = &*(&self.0.root as *const _);
            f(mc, root, self.1)
        }
    }
}

/// Create a temporary arena without a root object and perform the given operation on it. No garbage
/// collection will be done until the very end of the call, at which point all allocations will
/// be collected.
pub fn rootless_arena<F, R>(f: F) -> R
where
    F: for<'gc> FnOnce(&'gc Mutation<'gc>) -> R,
{
    unsafe {
        let context = Context::new();
        f(context.mutation_context())
    }
}
