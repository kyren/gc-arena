use alloc::vec::Vec;
use core::{
    cell::{Cell, UnsafeCell},
    mem,
    ops::{ControlFlow, Deref, DerefMut},
};

use crate::{
    Gc, GcWeak,
    collect::{Collect, Trace},
    metrics::Metrics,
    types::{GcColor, GcPtr, Invariant},
};

/// Handle value given by arena callbacks during construction and mutation. Allows allocating new
/// `Gc` pointers and internally mutating values held by `Gc` pointers.
#[repr(transparent)]
pub struct Mutation<'gc> {
    context: Context,
    _invariant: Invariant<'gc>,
}

impl<'gc> Mutation<'gc> {
    #[inline]
    pub fn metrics(&self) -> &Metrics {
        self.context.metrics()
    }

    /// IF we are in the marking phase AND the `parent` pointer is colored black AND the `child` (if
    /// given) is colored white, then change the `parent` color to gray and enqueue it for tracing.
    ///
    /// This operation is known as a "backwards write barrier". Calling this method is one of the
    /// safe ways for the value in the `parent` pointer to use internal mutability to adopt the
    /// `child` pointer without invalidating the color invariant.
    ///
    /// If the `child` parameter is given, then calling this method ensures that the `parent`
    /// pointer may safely adopt the `child` pointer. If no `child` is given, then calling this
    /// method is more general, and it ensures that the `parent` pointer may adopt *any* child
    /// pointer(s) before collection is next triggered.
    #[inline]
    pub fn backward_barrier(&self, parent: Gc<'gc, ()>, child: Option<Gc<'gc, ()>>) {
        self.context
            .backward_barrier(parent.ptr.erase(), child.map(|p| p.ptr.erase()))
    }

    /// A version of [`Mutation::backward_barrier`] that allows adopting a [`GcWeak`] child.
    #[inline]
    pub fn backward_barrier_weak(&self, parent: Gc<'gc, ()>, child: GcWeak<'gc, ()>) {
        self.context
            .backward_barrier_weak(parent.ptr.erase(), child.inner.ptr.erase())
    }

    /// IF we are in the marking phase AND the `parent` pointer (if given) is colored black, AND
    /// the `child` is colored white, then immediately change the `child` to gray and enqueue it
    /// for tracing.
    ///
    /// This operation is known as a "forwards write barrier". Calling this method is one of the
    /// safe ways for the value in the `parent` pointer to use internal mutability to adopt the
    /// `child` pointer without invalidating the color invariant.
    ///
    /// If the `parent` parameter is given, then calling this method ensures that the `parent`
    /// pointer may safely adopt the `child` pointer. If no `parent` is given, then calling this
    /// method is more general, and it ensures that the `child` pointer may be adopted by *any*
    /// parent pointer(s) before collection is next triggered.
    #[inline]
    pub fn forward_barrier(&self, parent: Option<Gc<'gc, ()>>, child: Gc<'gc, ()>) {
        self.context
            .forward_barrier(parent.map(|p| p.ptr.erase()), child.ptr.erase())
    }

    /// A version of [`Mutation::forward_barrier`] that allows adopting a [`GcWeak`] child.
    #[inline]
    pub fn forward_barrier_weak(&self, parent: Option<Gc<'gc, ()>>, child: GcWeak<'gc, ()>) {
        self.context
            .forward_barrier_weak(parent.map(|p| p.ptr.erase()), child.inner.ptr.erase())
    }

    #[inline]
    pub(crate) fn allocate<T: Collect<'gc> + 'gc>(&self, t: T) -> GcPtr<T> {
        self.context.allocate(t)
    }

    #[inline]
    pub(crate) fn upgrade(&self, gc_ptr: GcPtr) -> bool {
        self.context.upgrade(gc_ptr)
    }
}

/// Handle value given to finalization callbacks in `MarkedArena`.
///
/// Derefs to `Mutation<'gc>` to allow for arbitrary mutation, but adds additional powers to examine
/// the state of the fully marked arena.
#[repr(transparent)]
pub struct Finalization<'gc> {
    context: Context,
    _invariant: Invariant<'gc>,
}

impl<'gc> Deref for Finalization<'gc> {
    type Target = Mutation<'gc>;

    fn deref(&self) -> &Self::Target {
        // SAFETY: Finalization and Mutation are #[repr(transparent)]
        unsafe { mem::transmute::<&Self, &Mutation>(&self) }
    }
}

impl<'gc> Finalization<'gc> {
    #[inline]
    pub(crate) fn resurrect(&self, gc_ptr: GcPtr) {
        self.context.resurrect(gc_ptr)
    }
}

impl<'gc> Trace<'gc> for Context {
    fn trace_gc(&mut self, gc: Gc<'gc, ()>) {
        Context::trace(self, gc.ptr)
    }

    fn trace_gc_weak(&mut self, gc: GcWeak<'gc, ()>) {
        Context::trace_weak(self, gc.inner.ptr)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum Phase {
    Mark,
    Sweep,
    Sleep,
    Drop,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum RunUntil {
    // Run collection until we reach the stop condition *or* debt is zero.
    PayDebt,
    // Run collection until we reach our stop condition.
    Stop,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub(crate) enum Stop {
    // Don't proceed past the end of marking, *just* before the sweep phase
    FullyMarked,
    // Don't proceed past the very beginning of the sweep phase
    AtSweep,
    // Stop once we reach the end of the current cycle and are in `Phase::Sleep`.
    FinishCycle,
    // Stop once we have done an entire cycle as a single atomic unit. This is the maximum amount
    // of work that a call to `Context::do_collection` will do, since a full collection as a single
    // atomic unit means that all unreachable values *must* already be freed.
    Full,
}

pub(crate) struct Context {
    metrics: Metrics,
    phase: Phase,
    #[cfg(feature = "tracing")]
    phase_span: tracing::Span,

    // A linked list of all allocated `GcPtr`s.
    all: Cell<Option<GcPtr>>,

    // A copy of the head of `all` at the end of `Phase::Mark`.
    // During `Phase::Sweep`, we free all white allocations on this list.
    // Any allocations created *during* `Phase::Sweep` will be added to `all`,
    // but `sweep` will *not* be updated. This ensures that we keep allocations
    // alive until we've had a chance to trace them.
    sweep: Option<GcPtr>,

    // The most recent black object that we encountered during `Phase::Sweep`.
    // When we free objects, we update this `GcPtr.next` to remove them from
    // the linked list.
    sweep_prev: Cell<Option<GcPtr>>,

    /// Does the root needs to be traced?
    /// This should be `true` at the beginning of `Phase::Mark`.
    root_needs_trace: bool,

    /// A queue of gray objects, used during `Phase::Mark`.
    /// This holds traceable objects that have yet to be traced.
    gray: Queue<GcPtr>,

    // A queue of gray objects that became gray as a result
    // of a write barrier.
    gray_again: Queue<GcPtr>,
}

impl Drop for Context {
    fn drop(&mut self) {
        struct DropAll<'a>(&'a Metrics, Option<GcPtr>);

        impl<'a> Drop for DropAll<'a> {
            fn drop(&mut self) {
                if let Some(gc_ptr) = self.1.take() {
                    let mut drop_resume = DropAll(self.0, Some(gc_ptr));
                    while let Some(mut gc_ptr) = drop_resume.1.take() {
                        let header = gc_ptr.header();
                        drop_resume.1 = header.next();
                        // SAFETY: the context owns its GC'd objects
                        unsafe {
                            if header.is_live() {
                                gc_ptr.drop_in_place();
                                self.0.mark_gc_dropped(1);
                            }
                            gc_ptr.dealloc();
                            self.0.mark_gc_freed(1);
                        }
                    }
                }
            }
        }

        let cx = PhaseGuard::enter(self, Some(Phase::Drop));
        DropAll(&cx.metrics, cx.all.get());
    }
}

impl Context {
    pub(crate) unsafe fn new() -> Context {
        let metrics = Metrics::new();
        Context {
            phase: Phase::Sleep,
            #[cfg(feature = "tracing")]
            phase_span: PhaseGuard::span_for(&metrics, Phase::Sleep),
            metrics: metrics.clone(),
            all: Cell::new(None),
            sweep: None,
            sweep_prev: Cell::new(None),
            root_needs_trace: true,
            gray: Queue::new(),
            gray_again: Queue::new(),
        }
    }

    #[inline]
    pub(crate) unsafe fn mutation_context<'gc>(&self) -> &Mutation<'gc> {
        unsafe { mem::transmute::<&Self, &Mutation>(&self) }
    }

    #[inline]
    pub(crate) unsafe fn finalization_context<'gc>(&self) -> &Finalization<'gc> {
        unsafe { mem::transmute::<&Self, &Finalization>(&self) }
    }

    #[inline]
    pub(crate) fn metrics(&self) -> &Metrics {
        &self.metrics
    }

    #[inline]
    pub(crate) fn root_barrier(&mut self) {
        if self.phase == Phase::Mark {
            self.root_needs_trace = true;
        }
    }

    #[inline]
    pub(crate) fn phase(&self) -> Phase {
        self.phase
    }

    #[inline]
    pub(crate) fn gray_remaining(&self) -> bool {
        !self.gray.is_empty() || !self.gray_again.is_empty() || self.root_needs_trace
    }

    // Do some collection work until either we have achieved our `target` (paying off debt or
    // finishing a full collection) or we have reached the `stop` condition.
    //
    // In order for this to be safe, at the time of call no `Gc` pointers can be live that are not
    // reachable from the given root object.
    //
    // If we are currently in `Phase::Sleep` and have positive debt, this will immediately
    // transition the collector to `Phase::Mark`.
    #[deny(unsafe_op_in_unsafe_fn)]
    pub(crate) unsafe fn do_collection<'gc, R: Collect<'gc> + ?Sized>(
        &mut self,
        root: &R,
        run_until: RunUntil,
        stop: Stop,
    ) {
        let mut cx = PhaseGuard::enter(self, None);

        if run_until == RunUntil::PayDebt && !(cx.metrics.allocation_debt() > 0.0) {
            cx.log_progress("GC: paused");
            return;
        }

        let mut has_slept = false;

        loop {
            match cx.phase {
                Phase::Sleep => {
                    has_slept = true;
                    // Immediately enter the mark phase
                    cx.switch(Phase::Mark);
                }
                Phase::Mark => {
                    if cx.mark_one(root).is_break() {
                        if stop <= Stop::FullyMarked {
                            break;
                        } else {
                            // If we have no gray objects left, we enter the sweep phase.
                            cx.switch(Phase::Sweep);

                            // Set `sweep to the current head of our `all` linked list. Any new
                            // allocations during the newly-entered `Phase:Sweep` will update `all`,
                            // but will *not* be reachable from `this.sweep`.
                            cx.sweep = cx.all.get();
                        }
                    }
                }
                Phase::Sweep => {
                    if stop <= Stop::AtSweep {
                        break;
                    } else if cx.sweep_one().is_break() {
                        // Begin a new cycle.
                        //
                        // We reset our debt if we have done an entire collection cycle (marking and
                        // sweeping) as a single atomic unit. This keeps inherited debt from growing
                        // without bound.
                        cx.metrics.finish_cycle(has_slept);
                        cx.root_needs_trace = true;
                        cx.switch(Phase::Sleep);

                        // We treat a stop condition of `Stop::Finish` as special for the purposes
                        // of logging, and log that we finished a cycle.
                        if stop == Stop::FinishCycle {
                            cx.log_progress("GC: finished");
                            return;
                        }

                        // Otherwise we always break if we have performed a full cycle as a single
                        // atomic unit, because there cannot be any more work to do in this case.
                        if has_slept {
                            // We shouldn't be stopping here if the stop condition is something like
                            // `Stop::AtSweep`, but this should be impossible since the only way to
                            // get here is to have gone through the entire cycle.
                            assert!(stop == Stop::Full);
                            break;
                        }
                    }
                }
                Phase::Drop => unreachable!(),
            }

            if run_until == RunUntil::PayDebt && !(cx.metrics.allocation_debt() > 0.0) {
                break;
            }
        }

        cx.log_progress("GC: yielding...");
    }

    #[inline]
    fn allocate<'gc, T: Collect<'gc>>(&self, t: T) -> GcPtr<T> {
        let gc_ptr = GcPtr::alloc(t);

        let header = gc_ptr.header();
        header.set_next(self.all.get());
        header.set_live(true);
        header.set_needs_trace(T::NEEDS_TRACE);

        self.all.set(Some(gc_ptr.erase()));
        if self.phase == Phase::Sweep && self.sweep_prev.get().is_none() {
            self.sweep_prev.set(self.all.get());
        }

        self.metrics.mark_gc_allocated(1);

        gc_ptr
    }

    #[inline]
    fn backward_barrier(&self, parent: GcPtr, child: Option<GcPtr>) {
        // During the marking phase, if we are mutating a black object, we may add a white object to
        // it and invalidate the invariant that black objects may not point to white objects. Turn
        // the black parent object gray to prevent this.
        //
        // NOTE: This also adds the pointer to the gray_again queue even if `header.needs_trace()`
        // is false, but this is not harmful (just wasteful). There's no reason to call a barrier on
        // a pointer that can't adopt other pointers, so we skip the check.
        if self.phase == Phase::Mark
            && parent.header().color() == GcColor::Black
            && child
                .map(|c| matches!(c.header().color(), GcColor::White | GcColor::WhiteWeak))
                .unwrap_or(true)
        {
            // Outline the actual barrier code (which is somewhat expensive and won't be executed
            // often) to promote the inlining of the write barrier.
            #[cold]
            fn barrier(this: &Context, parent: GcPtr) {
                this.make_gray_again(parent);
            }
            barrier(&self, parent);
        }
    }

    #[inline]
    fn backward_barrier_weak(&self, parent: GcPtr, child: GcPtr) {
        if self.phase == Phase::Mark
            && parent.header().color() == GcColor::Black
            && child.header().color() == GcColor::White
        {
            // Outline the actual barrier code (which is somewhat expensive and won't be executed
            // often) to promote the inlining of the write barrier.
            #[cold]
            fn barrier(this: &Context, parent: GcPtr) {
                this.make_gray_again(parent);
            }
            barrier(&self, parent);
        }
    }

    #[inline]
    fn forward_barrier(&self, parent: Option<GcPtr>, child: GcPtr) {
        // During the marking phase, if we are mutating a black object, we may add a white object
        // to it and invalidate the invariant that black objects may not point to white objects.
        // Immediately trace the child white object to turn it gray (or black) to prevent this.
        if self.phase == Phase::Mark
            && parent
                .map(|p| p.header().color() == GcColor::Black)
                .unwrap_or(true)
        {
            // Outline the actual barrier code (which is somewhat expensive and won't be executed
            // often) to promote the inlining of the write barrier.
            #[cold]
            fn barrier(this: &Context, child: GcPtr) {
                this.trace(child);
            }
            barrier(&self, child);
        }
    }

    #[inline]
    fn forward_barrier_weak(&self, parent: Option<GcPtr>, child: GcPtr) {
        // During the marking phase, if we are mutating a black object, we may add a white object
        // to it and invalidate the invariant that black objects may not point to white objects.
        // Immediately trace the child white object to turn it gray (or black) to prevent this.
        if self.phase == Phase::Mark
            && parent
                .map(|p| p.header().color() == GcColor::Black)
                .unwrap_or(true)
        {
            // Outline the actual barrier code (which is somewhat expensive and won't be executed
            // often) to promote the inlining of the write barrier.
            #[cold]
            fn barrier(this: &Context, child: GcPtr) {
                this.trace_weak(child);
            }
            barrier(&self, child);
        }
    }

    #[inline]
    fn trace(&self, gc_ptr: GcPtr) {
        let header = gc_ptr.header();
        let color = header.color();
        match color {
            GcColor::Black | GcColor::Gray => {}
            GcColor::White | GcColor::WhiteWeak => {
                if header.needs_trace() {
                    // A white traceable object is not in the gray queue, becomes gray and enters
                    // the normal gray queue.
                    header.set_color(GcColor::Gray);
                    debug_assert!(header.is_live());
                    self.gray.push(gc_ptr);
                } else {
                    // A white object that doesn't need tracing simply becomes black.
                    header.set_color(GcColor::Black);
                }

                // Only marking the *first* time counts as a mark metric.
                if color == GcColor::White {
                    self.metrics.mark_gc_marked(1);
                }
            }
        }
    }

    #[inline]
    fn trace_weak(&self, gc_ptr: GcPtr) {
        let header = gc_ptr.header();
        if header.color() == GcColor::White {
            header.set_color(GcColor::WhiteWeak);
            self.metrics.mark_gc_marked(1);
        }
    }

    /// Determines whether or not a Gc pointer is safe to be upgraded.
    /// This is used by weak pointers to determine if it can safely upgrade to a strong pointer.
    #[inline]
    fn upgrade(&self, gc_ptr: GcPtr) -> bool {
        let header = gc_ptr.header();

        // This object has already been freed, definitely not safe to upgrade.
        if !header.is_live() {
            return false;
        }

        // Consider the different possible phases of the GC:
        // * In `Phase::Sleep`, the GC is not running, so we can upgrade.
        //   If the newly-created `Gc` or `GcCell` survives the current `arena.mutate`
        //   call, then the situation is equivalent to having copied an existing `Gc`/`GcCell`,
        //   or having created a new allocation.
        //
        // * In `Phase::Mark`:
        //   If the newly-created `Gc` or `GcCell` survives the current `arena.mutate`
        //   call, then it must have been stored somewhere, triggering a write barrier.
        //   This will ensure that the new `Gc`/`GcCell` gets traced (if it's now reachable)
        //   before we transition to `Phase::Sweep`.
        //
        // * In `Phase::Sweep`:
        //   If the allocation is `WhiteWeak`, then it's impossible for it to have been freshly-
        //   created during this `Phase::Sweep`. `WhiteWeak` is only  set when a white `GcWeak/
        //   GcWeakCell` is traced. A `GcWeak/GcWeakCell` must be created from an existing `Gc/
        //   GcCell` via `downgrade()`, so `WhiteWeak` means that a `GcWeak` / `GcWeakCell` existed
        //   during the last `Phase::Mark.`
        //
        //   Therefore, a `WhiteWeak` object is guaranteed to be deallocated during this
        //   `Phase::Sweep`, and we must not upgrade it.
        //
        //   Conversely, it's always safe to upgrade a white object that is not `WhiteWeak`.
        //   In order to call `upgrade`, you must have a `GcWeak/GcWeakCell`. Since it is
        //   not `WhiteWeak` there cannot have been any `GcWeak/GcWeakCell`s during the
        //   last `Phase::Mark`, so the weak pointer must have been created during this
        //   `Phase::Sweep`. This is only possible if the underlying allocation was freshly-created
        //   - if the allocation existed during `Phase::Mark` but was not traced, then it
        //   must have been unreachable, which means that the user wouldn't have been able to call
        //   `downgrade`. Therefore, we can safely upgrade, knowing that the object will not be
        //   freed during this phase, despite being white.
        if self.phase == Phase::Sweep && header.color() == GcColor::WhiteWeak {
            return false;
        }
        true
    }

    #[inline]
    fn resurrect(&self, gc_ptr: GcPtr) {
        let header = gc_ptr.header();
        debug_assert_eq!(self.phase, Phase::Mark);
        debug_assert!(header.is_live());
        let color = header.color();
        if matches!(header.color(), GcColor::White | GcColor::WhiteWeak) {
            header.set_color(GcColor::Gray);
            self.gray.push(gc_ptr);
            // Only marking the *first* time counts as a mark metric.
            if color == GcColor::White {
                self.metrics.mark_gc_marked(1);
            }
        }
    }

    fn mark_one<'gc, R: Collect<'gc> + ?Sized>(&mut self, root: &R) -> ControlFlow<()> {
        // We look for an object first in the normal gray queue, then the "gray again" queue.
        // Processing "gray again" objects later gives them more time to be mutated again without
        // triggering another write barrier.
        let next_gray = if let Some(gc_ptr) = self.gray.pop() {
            Some(gc_ptr)
        } else if let Some(gc_ptr) = self.gray_again.pop() {
            Some(gc_ptr)
        } else {
            None
        };

        if let Some(gc_ptr) = next_gray {
            // We always mark work for objects processed from both the gray and "gray again" queue.
            // When objects are placed into the "gray again" queue due to a write barrier, the
            // original work is *undone*, so we do it again here.
            self.metrics.mark_gc_traced(1);
            gc_ptr.header().set_color(GcColor::Black);

            // If we have an object in the gray queue, take one, trace it, and turn it black.

            // Our `Collect::trace` call may panic, and if it does the object will be lost from
            // the gray queue but potentially incompletely traced. By catching a panic during
            // `Arena::collect()`, this could lead to memory unsafety.
            //
            // So, if the `Collect::trace` call panics, we need to add the popped object back to the
            // `gray_again` queue. If the panic is caught, this will maybe give some time for its
            // trace method to not panic before attempting to collect it again.
            struct DropGuard<'a> {
                context: &'a mut Context,
                gc_ptr: GcPtr,
            }

            impl<'a> Drop for DropGuard<'a> {
                fn drop(&mut self) {
                    self.context.make_gray_again(self.gc_ptr);
                }
            }

            let guard = DropGuard {
                context: self,
                gc_ptr,
            };
            debug_assert!(gc_ptr.header().is_live());
            unsafe { gc_ptr.trace_value(guard.context) }
            mem::forget(guard);

            ControlFlow::Continue(())
        } else if self.root_needs_trace {
            // We treat the root object as gray if `root_needs_trace` is set, and we process it at
            // the end of the gray queue for the same reason as the "gray again" objects.
            root.trace(self);
            self.root_needs_trace = false;
            ControlFlow::Continue(())
        } else {
            ControlFlow::Break(())
        }
    }

    fn sweep_one(&mut self) -> ControlFlow<()> {
        let Some(mut sweep) = self.sweep else {
            self.sweep_prev.set(None);
            return ControlFlow::Break(());
        };

        let sweep_header = sweep.header();

        let next_ptr = sweep_header.next();
        self.sweep = next_ptr;

        match sweep_header.color() {
            // If the next object in the sweep portion of the main list is white, we
            // need to remove it from the main object list and destruct it.
            GcColor::White => {
                if let Some(sweep_prev) = self.sweep_prev.get() {
                    sweep_prev.header().set_next(next_ptr);
                } else {
                    // If `sweep_prev` is None, then the sweep pointer is also the
                    // beginning of the main object list, so we need to adjust it.
                    debug_assert!(self.all.get().is_some_and(|f| f.addr_eq(sweep)));
                    self.all.set(next_ptr);
                }

                // SAFETY: this object is white, and wasn't traced by a `GcWeak` during this cycle,
                // meaning it cannot have either strong or weak pointers, so we can drop the whole
                // object.
                unsafe {
                    if sweep_header.is_live() {
                        // If the alive flag is set, that means we haven't dropped the inner value
                        // of this object,
                        sweep.drop_in_place();
                        self.metrics.mark_gc_dropped(1);
                    }
                    sweep.dealloc();
                    self.metrics.mark_gc_freed(1);
                }
            }
            // Keep the `GcPtr` as part of the linked list if we traced a weak pointer to it. The
            // weak pointer still needs access to the `GcPtr` to be able to check if the object
            // is still alive. We can only deallocate the `GcPtr`, once there are no weak pointers
            // left.
            GcColor::WhiteWeak => {
                self.sweep_prev.set(Some(sweep));
                sweep_header.set_color(GcColor::White);
                if sweep_header.is_live() {
                    sweep_header.set_live(false);
                    // SAFETY: Since this object is white, that means there are no more strong
                    // pointers to this object, only weak pointers, so we can safely drop its
                    // contents.
                    unsafe { sweep.drop_in_place() }
                    self.metrics.mark_gc_dropped(1);
                }
                self.metrics.mark_gc_remembered(1);
            }
            // If the next object in the sweep portion of the main list is black, we
            // need to keep it but turn it back white.
            GcColor::Black => {
                self.sweep_prev.set(Some(sweep));
                sweep_header.set_color(GcColor::White);
                self.metrics.mark_gc_remembered(1);
            }
            // No gray objects should be in this part of the main list, they should
            // be added to the beginning of the list before the sweep pointer, so it
            // should not be possible for us to encounter them here.
            GcColor::Gray => {
                debug_assert!(false, "unexpected gray object in sweep list")
            }
        }

        ControlFlow::Continue(())
    }

    // Take a black pointer and turn it gray and put it in the `gray_again` queue.
    fn make_gray_again(&self, gc_ptr: GcPtr) {
        let header = gc_ptr.header();
        debug_assert_eq!(header.color(), GcColor::Black);
        header.set_color(GcColor::Gray);
        self.gray_again.push(gc_ptr);
        self.metrics.mark_gc_untraced(1);
    }
}

/// Helper type for managing phase transitions.
struct PhaseGuard<'a> {
    cx: &'a mut Context,
    #[cfg(feature = "tracing")]
    span: tracing::span::EnteredSpan,
}

impl<'a> Drop for PhaseGuard<'a> {
    fn drop(&mut self) {
        #[cfg(feature = "tracing")]
        {
            let span = mem::replace(&mut self.span, tracing::Span::none().entered());
            self.cx.phase_span = span.exit();
        }
    }
}

impl<'a> Deref for PhaseGuard<'a> {
    type Target = Context;

    #[inline(always)]
    fn deref(&self) -> &Context {
        &self.cx
    }
}

impl<'a> DerefMut for PhaseGuard<'a> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Context {
        &mut self.cx
    }
}

impl<'a> PhaseGuard<'a> {
    fn enter(cx: &'a mut Context, phase: Option<Phase>) -> Self {
        if let Some(phase) = phase {
            cx.phase = phase;
        }

        Self {
            #[cfg(feature = "tracing")]
            span: {
                let mut span = mem::replace(&mut cx.phase_span, tracing::Span::none());
                if let Some(phase) = phase {
                    span = Self::span_for(&cx.metrics, phase);
                }
                span.entered()
            },
            cx,
        }
    }

    fn switch(&mut self, phase: Phase) {
        self.cx.phase = phase;

        #[cfg(feature = "tracing")]
        {
            let _ = mem::replace(&mut self.span, tracing::Span::none().entered());
            self.span = Self::span_for(&self.cx.metrics, phase).entered();
        }
    }

    fn log_progress(&mut self, #[allow(unused)] message: &str) {
        // TODO: add more infos here
        #[cfg(feature = "tracing")]
        tracing::debug!(
            target: "gc_arena",
            parent: &self.span,
            message,
            phase = tracing::field::debug(self.cx.phase),
            allocated_gcs = self.cx.metrics.total_gc_count(),
        );
    }

    #[cfg(feature = "tracing")]
    fn span_for(metrics: &Metrics, phase: Phase) -> tracing::Span {
        tracing::debug_span!(
            target: "gc_arena",
            "gc_arena",
            id = metrics.arena_id(),
            ?phase,
        )
    }
}

// A shared, internally mutable `Vec<T>` that avoids the overhead of `RefCell`. Used for the "gray"
// and "gray again" queues.
//
// SAFETY: We do not return any references at all to the contents of the internal `UnsafeCell`, nor
// do we provide any methods with callbacks. Since this type is `!Sync`, only one reference to the
// `UnsafeCell` contents can be alive at any given time, thus we cannot violate aliasing rules.
#[derive(Default)]
struct Queue<T> {
    vec: UnsafeCell<Vec<T>>,
}

impl<T> Queue<T> {
    fn new() -> Self {
        Self {
            vec: UnsafeCell::new(Vec::new()),
        }
    }

    fn is_empty(&self) -> bool {
        unsafe { (*self.vec.get().cast_const()).is_empty() }
    }

    fn push(&self, val: T) {
        unsafe {
            (*self.vec.get()).push(val);
        }
    }

    fn pop(&self) -> Option<T> {
        unsafe { (*self.vec.get()).pop() }
    }
}
