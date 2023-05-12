use core::{f64, marker::PhantomData, mem, usize};

use crate::{
    context::{Context, MutationContext},
    Collect,
};

/// Tuning parameters for a given garbage collected [`Arena`].
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
    /// retained size> * `pause_multiplier` before beginning a new collection. Must be >= 0.0,
    /// setting this to 0.0 causes the collector to never sleep longer than `min_sleep` before
    /// beginning a new collection.
    pub fn set_pause_factor(mut self, pause_factor: f64) -> ArenaParameters {
        assert!(pause_factor >= 0.0);
        self.pause_factor = pause_factor;
        self
    }

    /// The garbage collector will try and finish a collection by the time <current heap size> *
    /// `timing_factor` additional bytes are allocated. For example, if the collection is started
    /// when the arena has 100KB live data, and the timing_multiplier is 1.0, the collector should
    /// finish its final phase of this collection after another 100KB has been allocated. Must be >=
    /// 0.0, setting this to 0.0 causes the collector to behave like a stop-the-world collector.
    pub fn set_timing_factor(mut self, timing_factor: f64) -> ArenaParameters {
        assert!(timing_factor >= 0.0);
        self.timing_factor = timing_factor;
        self
    }

    /// The minimum allocation amount during sleep before the arena starts collecting again. This is
    /// mostly useful when the heap is very small to prevent rapidly restarting collections.
    pub fn set_min_sleep(mut self, min_sleep: usize) -> ArenaParameters {
        self.min_sleep = min_sleep;
        self
    }
}

/// A trait that produces a [`Collect`]-able type for the given lifetime. This is used to produce
/// the root [`Collect`] instance in an [`Arena`].
///
/// In order to use an implementation of this trait in an [`Arena`], it must implement
/// `Rootable<'a>` for *any* possible `'a`. This is necessary so that the `Root` types can be
/// branded by the unique, invariant lifetimes that makes an `Arena` sound.
pub trait Rootable<'a>: 'static {
    type Root: Collect + 'a;
}

/// A marker type used by the `Rootable!` macro instead of a bare trait object.
///
/// Prevents having to include extra ?Sized bounds on every `for<'a> Rootable<'a>`.
#[doc(hidden)]
pub struct __DynRootable<T: ?Sized>(PhantomData<T>);

impl<'a, T: ?Sized + Rootable<'a>> Rootable<'a> for __DynRootable<T> {
    type Root = <T as Rootable<'a>>::Root;
}

/// A convenience macro for quickly creating type that implements of `Rootable`.
///
/// The macro takes a single argument, which should be a generic type that references a `'gc`
/// lifetime. When used as a root object, this `'gc` lifetime will be replaced with the branding
/// lifetime.
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
/// type MyArena = Arena<Rootable![MyRoot<'gc>]>;
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
/// type MyGenericArena<T> = Arena<Rootable![MyGenericRoot<'gc, T>]>;
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
        Rootable!['gc => $root]
    };
}

/// A helper type alias for a `Rootable::Root` for a specific lifetime.
pub type Root<'a, R> = <R as Rootable<'a>>::Root;

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
    // We rely on the implicit drop order here, `root` *must* be dropped before `context`!
    root: Root<'static, R>,
    context: Context,
}

impl<R: for<'a> Rootable<'a>> Arena<R> {
    /// Create a new arena with the given garbage collector tuning parameters. You must provide a
    /// closure that accepts a `MutationContext` and returns the appropriate root.
    pub fn new<F>(arena_parameters: ArenaParameters, f: F) -> Arena<R>
    where
        F: for<'gc> FnOnce(MutationContext<'gc, '_>) -> Root<'gc, R>,
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
                mem::transmute(context.mutation_context());
            let root: Root<'static, R> = f(mutation_context);
            Arena { context, root }
        }
    }

    /// Similar to `new`, but allows for constructor that can fail.
    pub fn try_new<F, E>(arena_parameters: ArenaParameters, f: F) -> Result<Arena<R>, E>
    where
        F: for<'gc> FnOnce(MutationContext<'gc, '_>) -> Result<Root<'gc, R>, E>,
    {
        unsafe {
            let context = Context::new(arena_parameters);
            let mutation_context: MutationContext<'static, '_> =
                mem::transmute(context.mutation_context());
            let root: Root<'static, R> = f(mutation_context)?;
            Ok(Arena { context, root })
        }
    }

    /// The primary means of interacting with a garbage collected arena. Accepts a callback which
    /// receives a `MutationContext` and a reference to the root, and can return any non garbage
    /// collected value. The callback may "mutate" any part of the object graph during this call,
    /// but no garbage collection will take place during this method.
    #[inline]
    pub fn mutate<F, T>(&self, f: F) -> T
    where
        F: for<'gc> FnOnce(MutationContext<'gc, '_>, &Root<'gc, R>) -> T,
    {
        // The user-provided callback may return a (non-GC'd) value borrowed from the arena;
        // this is safe as all objects in the graph live until the next collection, which
        // requires exclusive access to the arena.
        unsafe { f(self.context.mutation_context(), &self.root) }
    }

    /// An alternative version of [`Arena::mutate`] which allows mutating the root set, at the
    /// cost of an extra write barrier.
    #[inline]
    pub fn mutate_root<F, T>(&mut self, f: F) -> T
    where
        F: for<'gc> FnOnce(MutationContext<'gc, '_>, &mut Root<'gc, R>) -> T,
    {
        self.context.root_barrier();
        // The user-provided callback may return a (non-GC'd) value borrowed from the arena;
        // this is safe as all objects in the graph live until the next collection, which
        // requires exclusive access to the arena. Additionally, the write barrier ensures
        // that any changes to the root set are properly picked up.
        unsafe { f(self.context.mutation_context(), &mut self.root) }
    }

    #[inline]
    pub fn map_root<R2: for<'a> Rootable<'a>>(
        self,
        f: impl for<'gc> FnOnce(MutationContext<'gc, '_>, Root<'gc, R>) -> Root<'gc, R2>,
    ) -> Arena<R2> {
        self.context.root_barrier();
        let new_root: Root<'static, R2> = unsafe { f(self.context.mutation_context(), self.root) };
        Arena {
            context: self.context,
            root: new_root,
        }
    }

    #[inline]
    pub fn try_map_root<R2: for<'a> Rootable<'a>, E>(
        self,
        f: impl for<'gc> FnOnce(MutationContext<'gc, '_>, Root<'gc, R>) -> Result<Root<'gc, R2>, E>,
    ) -> Result<Arena<R2>, E> {
        self.context.root_barrier();
        let new_root: Root<'static, R2> = unsafe { f(self.context.mutation_context(), self.root)? };
        Ok(Arena {
            context: self.context,
            root: new_root,
        })
    }

    /// Return total currently used memory.
    #[inline]
    pub fn total_allocated(&self) -> usize {
        self.context.total_allocated()
    }

    #[inline]
    pub fn remembered_size(&self) -> usize {
        self.context.remembered_size()
    }

    /// When the garbage collector is not sleeping, all allocated objects cause the arena to
    /// accumulate "allocation debt". This debt is then be used to time incremental garbage
    /// collection based on the tuning parameters set in `ArenaParameters`. The allocation debt is
    /// measured in bytes, but will generally increase at a rate faster than that of allocation so
    /// that collection will always complete.
    #[inline]
    pub fn allocation_debt(&self) -> f64 {
        self.context.allocation_debt()
    }

    /// Run the incremental garbage collector until the allocation debt is <= 0.0. There is no
    /// minimum unit of work enforced here, so it may be faster to only call this method when the
    /// allocation debt is above some threshold.
    #[inline]
    pub fn collect_debt(&mut self) {
        unsafe {
            let debt = self.context.allocation_debt();
            if debt > 0.0 {
                self.context.do_collection(&self.root, debt);
            }
        }
    }

    /// Run the current garbage collection cycle to completion, stopping once the garbage collector
    /// has entered the sleeping phase. If the garbage collector is currently sleeping, starts a new
    /// cycle and runs that cycle to completion.
    pub fn collect_all(&mut self) {
        self.context.wake();
        unsafe {
            self.context.do_collection(&self.root, f64::INFINITY);
        }
    }
}

// SAFETY: The intention here is to make the Arena type Send if it contains no types which are
// !Send *other* than `Gc`. We know that we can't get in trouble with `Gc` specifically, because a
// `Gc` already cannot escape an `Arena` for the soundness of the garbage collector, so references
// to it can only be reachable from one thread at a time (you cannot, for example, store a `Gc`
// inside TLS without `unsafe`).
//
// We implement `Send` specifically for `Gc<'static, T>`, so that if there are no *other* !Send
// types, the entire root object (projected to 'static) will be Send.
//
// You could use the `'gc` lifetime in a different type, other than `Gc` or other `gc_arena` types,
// that implements Send iff 'gc == 'static, and in doing so, the projection of those types would be
// 'static and thus also Send, possibly incorrectly. However, it would be unsafe to create such
// a type and store it into the Arena in the first place, because it would require transmuting an
// unrelated lifetime to 'gc, which we know is generative and invariant.
unsafe impl<R: for<'a> Rootable<'a> + ?Sized> Send for Arena<R> where Root<'static, R>: Send {}

/// Create a temporary arena without a root object and perform the given operation on it. No garbage
/// collection will be done until the very end of the call, at which point all allocations will
/// be collected.
pub fn rootless_arena<F, R>(f: F) -> R
where
    F: for<'gc> FnOnce(MutationContext<'gc, '_>) -> R,
{
    unsafe {
        let context = Context::new(ArenaParameters::default());
        f(context.mutation_context())
    }
}
