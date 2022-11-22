use crate::context::CollectionContext;

/// A trait for garbage collected objects that can be placed into `Gc` pointers.  This trait is
/// unsafe, because `Gc` pointers inside an Arena are assumed never to be dangling, and in order to
/// ensure this certain rules must be followed:
///
///   1. `Collect::trace` *must* trace over *every* `Gc` pointer held inside this type, and cannot
///      fail.
///   2. Held `Gc` pointers must not be accessed inside `Drop::drop` since during drop any such
///      pointer may be dangling.
///   3. Internal mutability *must* not be used to adopt new `Gc` pointers without calling
///      `Gc::write_barrier` during the same arena mutation.
///
/// It is, however, possible to implement this trait safely by procedurally deriving it, which
/// requires that every field in the structure also implement `Collect`, and implements a safe,
/// empty version of `Drop`.  Internally mutable types like `Cell` and `RefCell` do not implement
/// `Collect` in such a way that it is possible to store `Gc` pointers inside them, so the write
/// barrier requirement cannot be broken when procedurally deriving `Collect`.  A safe way of
/// providing internal mutability in this case is to use `GcCell`, which provides internal
/// mutability while ensuring that the write barrier is always executed.
///
/// ## Deriving `Collect`
///
/// To derive `Collect`, an additional attribute is required on the struct/enum called `collect`.
/// This has several optional arguments, but the only required argument is the derive strategy.
/// This can be one of
///
/// - `#[collect(require_static)]` - Adds a `'static` bound, which allows for a no-op trace implementation.
///   This is the ideal choice where possible.
/// - `#[collect(no_drop)]` - The typical safe tracing derive strategy which only has to add a requirement
///   that your struct/enum does not have a custom implementation of `Drop`.
/// - `#[collect(unsafe_drop)]` - The most versatile tracing derive strategy which allows a custom drop implementation.
///   However, this strategy can lead to unsoundness if care is not taken (see the above explanation of `Drop` interactions).
///
/// The `collect` attribute also accepts a number of optional configuration settings:
///
/// - `#[collect(bound = "<code>")]` - Replaces the default generated `where` clause with the given code.
///   This can be an empty string to add no `where` clause, or otherwise must start with `"where"`,
///   e.g., `#[collect(bound = "where T: Collect")]`.
///   Note that this option is ignored for `require_static` mode since the only bound it produces is `Self: 'static`.
///   Also note that providing an explicit bound in this way is safe, and only changes the trait bounds used
///   to enable the implementation of `Collect`.
///
/// Options may be passed to the `collect` attribute together, e.g., `#[collect(no_drop, bound = "")]`.
///
/// The `collect` attribute may also be used on any field of an enum or struct, however the only allowed usage
/// is to specify the strategy as `require_static` (no other strategies are allowed, and no optional settings can be specified).
/// This will add a `'static` bound to the type of the field (regardless of an explicit `bound` setting)
/// in exchange for not having to trace into the given field (the ideal choice where possible).
/// Note that if the entire struct/enum is marked with `require_static` then this is unnecessary.
pub unsafe trait Collect {
    /// As an optimization, if this type can never hold a `Gc` pointer and `trace` is unnecessary to
    /// call, you may implement this method and return false.  The default implementation returns
    /// true, signaling that `Collect::trace` must be called.
    #[inline]
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    /// *Must* call `Collect::trace` on all held `Gc` pointers.  If this type holds inner types that
    /// implement `Collect`, a valid implementation would simply call `Collect::trace` on all the
    /// held values to ensure this.
    #[inline]
    fn trace(&self, _cc: CollectionContext) {}
}
