use crate::context::Collection;

/// A trait for garbage collected objects that can be placed into `Gc` pointers. This trait is
/// unsafe, because `Gc` pointers inside an Arena are assumed never to be dangling, and in order to
/// ensure this certain rules must be followed:
///
///   1. `Collect::trace` *must* trace over *every* `Gc` pointer held inside this type, and cannot
///      fail.
///   2. Held `Gc` pointers must not be accessed inside `Drop::drop` since during drop any such
///      pointer may be dangling.
///   3. Internal mutability *must* not be used to adopt new `Gc` pointers without calling
///      `Gc::write` during the same arena mutation.
///
/// It is, however, possible to implement this trait safely by procedurally deriving it (see
/// [`gc_arena_derive::Collect`]), which requires that every field in the structure also implement
/// `Collect`, and ensures that `Drop` cannot safely be implemented. Internally mutable types like
/// `Cell` and `RefCell` do not implement `Collect` in such a way that it is possible to store
/// `Gc` pointers inside them, so the write barrier requirement cannot be broken when procedurally
/// deriving `Collect`. A safe way of providing internal mutability in this case is to use
/// [`crate::lock::Lock<T>`] and [`crate::lock::RefLock<T>`], which provides internal mutability
/// while ensuring that the write barrier is always executed.
pub unsafe trait Collect {
    /// As an optimization, if this type can never hold a `Gc` pointer and `trace` is unnecessary
    /// to call, you may set this to `false`. The default value is `true`, signaling that
    /// `Collect::trace` must be called.
    const NEEDS_TRACE: bool = true;

    /// *Must* call [`Collection::trace_gc`] (resp. [`Collection::trace_gc_weak`]) on all held
    /// [`Gc`] (resp. [`GcWeak`]) pointers. If this type holds inner types that implement
    /// `Collect`, a valid implementation would simply call `Collection::trace` on all the held
    /// values to ensure this.
    #[inline]
    #[allow(unused_variables, unused_mut)]
    fn trace(&self, mut cc: Collection<'_>) {}
}
