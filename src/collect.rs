use crate::{Gc, GcWeak};

pub use gc_arena_derive::Collect;

/// A trait for garbage collected objects that can be placed into `Gc` pointers. This trait is
/// unsafe, because `Gc` pointers inside an Arena are assumed never to be dangling, and in order to
/// ensure this certain rules must be followed:
///
///   1. `Collect::trace` *must* trace over *every* `Gc` and `GcWeak` pointer held inside this type.
///   2. Held `Gc` and `GcWeak` pointers must not be accessed inside `Drop::drop` since during drop
///      any such pointer may be dangling.
///   3. Internal mutability *must* not be used to adopt new `Gc` or `GcWeak` pointers without
///      calling appropriate write barrier operations during the same arena mutation.
///
/// It is, however, possible to implement this trait safely by procedurally deriving it (see
/// [`gc_arena_derive::Collect`]), which requires that every field in the structure also implement
/// `Collect`, and ensures that `Drop` cannot safely be implemented. Internally mutable types like
/// `Cell` and `RefCell` do not implement `Collect` in such a way that it is possible to store
/// `Gc` pointers inside them, so write barrier requirements cannot be broken when procedurally
/// deriving `Collect`. A safe way of providing internal mutability in this case is to use
/// [`crate::lock::Lock<T>`] and [`crate::lock::RefLock<T>`], which provides internal mutability
/// while ensuring that write barriers are correctly executed.
pub unsafe trait Collect<'gc> {
    /// As an optimization, if this type can never hold a `Gc` pointer and `trace` is unnecessary
    /// to call, you may set this to `false`. The default value is `true`, signaling that
    /// `Collect::trace` must be called.
    const NEEDS_TRACE: bool = true;

    /// *Must* call [`Trace::trace_gc`] (resp. [`Trace::trace_gc_weak`]) on all directly owned
    /// [`Gc`] (resp. [`GcWeak`]) pointers. If this type holds inner types that implement `Collect`,
    /// a valid implementation would simply call [`Trace::trace`] on all the held values to ensure
    /// this.
    ///
    /// # Tracing pointers
    ///
    /// [`Gc`] and [`GcWeak`] have their own implementations of `Collect` which in turn call
    /// [`Trace::trace_gc`] and [`Trace::trace_gc_weak`] respectively. Because of this, it is not
    /// actually ever necessary to call [`Trace::trace_gc`] and [`Trace::trace_gc_weak`] directly,
    /// but be careful! It is important that owned pointers *themselves* are traced and NOT their
    /// contents (the content type will usually also implement `Collect`, so this is easy to
    /// accidentally do).
    ///
    /// It is always okay to use the [`Trace::trace_gc`] and [`Trace::trace_gc_weak`] directly as a
    /// potentially less risky alternative when manually implementing `Collect`.
    #[inline]
    #[allow(unused_variables)]
    fn trace<T: Trace<'gc>>(&self, cc: &mut T) {}
}

/// The trait that is passed to the [`Collect::trace`] method.
///
/// Though [`Collect::trace`] is primarily used during the marking phase of the mark and sweep
/// collector, this is a trait rather than a concrete type to facilitate other kinds of uses.
///
/// This trait is not itself unsafe, but implementers of [`Collect`] *must* uphold the safety
/// guarantees of [`Collect`] when using this trait.
pub trait Trace<'gc> {
    /// Trace a [`Gc`] pointer (of any real type).
    fn trace_gc(&mut self, gc: Gc<'gc, ()>);

    /// Trace a [`GcWeak`] pointer (of any real type).
    fn trace_gc_weak(&mut self, gc: GcWeak<'gc, ()>);

    /// This is a convenience method that calls [`Collect::trace`] but automatically adds a
    /// [`Collect::NEEDS_TRACE`] check around it.
    ///
    /// Manual implementations of [`Collect`] are encouraged to use this to ensure that there
    /// is a [`Collect::NEEDS_TRACE`] check without having to implement it manually. This can be
    /// important in cases where the [`Collect::trace`] method impl is not `#[inline]` or does not
    /// have its own early exit.
    ///
    /// There is generally no need for custom `Trace` implementations to override this method.
    #[inline]
    fn trace<C: Collect<'gc> + ?Sized>(&mut self, value: &C)
    where
        Self: Sized,
    {
        if C::NEEDS_TRACE {
            value.trace(self);
        }
    }
}

/// If a type is static, we know that it can never hold `Gc` pointers, so it is safe to provide a
/// simple empty `Collect` implementation.
///
/// This macro may be called in multiple ways:
///
/// ```ignore
/// static_collect!(MyTrait<'gc>);
/// static_collect!(<T> MyTrait<'gc, T> where T: Clone);
/// static_collect!(<T> MyTrait<'gc, Assoc = T>);
/// ```
///
/// Type parameters may be declared using `<A, B>` at the beginning of the macro, along with an
/// optional trailing where clause.
#[macro_export]
macro_rules! static_collect {
    (<$($params:tt),+ $(,)*> $type:ty $(where $($bounds:tt)+)?) => {
        unsafe impl<'gc, $($params),*> $crate::Collect<'gc> for $type
        where
            $type: 'static,
            $($($bounds)+)*
        {
            const NEEDS_TRACE: bool = false;
        }
    };
    ($type:ty) => {
        unsafe impl<'gc> $crate::Collect<'gc> for $type
        where
            $type: 'static,
        {
            const NEEDS_TRACE: bool = false;
        }
    }
}

/// An object safe version of the [`Collect`] trait.
///
/// This is automatically implemented for all types that implement the normal `Collect` trait.
///
/// The `dyn DynCollect` trait object implements `Collect` automatically, but trait objects for user
/// defined traits that have `DynCollect` as a parent trait can also be made to implement `Collect`
/// by using the [`dyn_collect`] macro.
pub unsafe trait DynCollect<'gc> {
    fn dyn_trace(&self, cc: &mut dyn Trace<'gc>);
}

unsafe impl<'gc> Collect<'gc> for dyn DynCollect<'gc> {
    fn trace<T: Trace<'gc>>(&self, cc: &mut T) {
        self.dyn_trace(cc)
    }
}

unsafe impl<'gc, T: Collect<'gc>> DynCollect<'gc> for T {
    fn dyn_trace(&self, cc: &mut dyn Trace<'gc>) {
        struct TraceWrap<'a, 'gc>(&'a mut dyn Trace<'gc>);

        impl<'a, 'gc> Trace<'gc> for TraceWrap<'a, 'gc> {
            fn trace_gc(&mut self, gc: Gc<'gc, ()>) {
                self.0.trace_gc(gc)
            }

            fn trace_gc_weak(&mut self, gc: GcWeak<'gc, ()>) {
                self.0.trace_gc_weak(gc)
            }
        }

        self.trace(&mut TraceWrap(cc))
    }
}

/// Implement [`Collect`] for custom trait objects.
///
/// The trait being implemented must have [`DynCollect`] as a supertrait.
///
/// This macro may be called in multiple ways:
///
/// ```ignore
/// dyn_collect!(MyTrait<'gc>);
/// dyn_collect!(<T> MyTrait<'gc, T> where T: Clone);
/// dyn_collect!(<T> MyTrait<'gc, Assoc = T>);
/// ```
///
/// The generated impl always has a single `'gc` lifetime parameter that is used as the `'gc`
/// parameter on the `Collect` trait. Additional type and lifetime parameters may be declared using
/// `<A, B>` at the beginning of the macro, along with an optional trailing where clause.
#[doc(hidden)]
#[macro_export]
macro_rules! __dyn_collect {
    (<$($params:tt),+ $(,)*> $trait:ty $(where $($bounds:tt)+)?) => {
        unsafe impl<'gc, $($params),*> $crate::Collect<'gc> for $trait
        where
            $($($bounds)+)*
        {
            fn trace<_T: $crate::collect::Trace<'gc>>(&self, cc: &mut _T) {
                $crate::collect::DynCollect::dyn_trace(self, cc);
            }
        }
    };
    ($trait:ty) => {
        unsafe impl<'gc> $crate::Collect<'gc> for $trait {
            fn trace<_T: $crate::collect::Trace<'gc>>(&self, cc: &mut _T) {
                $crate::collect::DynCollect::dyn_trace(self, cc);
            }
        }
    }
}

#[doc(inline)]
pub use crate::__dyn_collect as dyn_collect;
