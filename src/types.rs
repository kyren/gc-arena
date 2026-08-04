use core::{cell::Cell, marker::PhantomData};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub(crate) enum GcColor {
    /// An object that has not yet been reached by tracing (if we're in a tracing phase).
    ///
    /// During `Phase::Sweep`, we will free all white objects that existed *before* the start of the
    /// current `Phase::Sweep`. Objects allocated during `Phase::Sweep` will be white, but will not
    /// be freed.
    White,
    /// Like White, but for objects weakly reachable from a Black object.
    ///
    /// These objects may drop their contents during `Phase::Sweep`, but must stay allocated so that
    /// weak references can check the alive status.
    WhiteWeak,
    /// An object reachable from a Black object, but that has not yet been traced using
    /// `Collect::trace`. We also mark black objects as gray during `Phase::Mark` in response to
    /// a write barrier, so that we re-trace and find any objects newly reachable from the mutated
    /// object.
    Gray,
    /// An object that was reached during tracing. It will not be freed during `Phase::Sweep`. At
    /// the end of `Phase::Sweep`, all black objects will be reset to white.
    Black,
}

// Phantom type that holds a lifetime and ensures that it is invariant.
pub(crate) type Invariant<'a> = PhantomData<Cell<&'a ()>>;
