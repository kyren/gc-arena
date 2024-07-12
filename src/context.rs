use alloc::{boxed::Box, vec::Vec};
use core::{
    cell::{Cell, RefCell},
    mem,
    ops::Deref,
    ptr::NonNull,
};

use crate::{
    collect::Collect,
    metrics::Metrics,
    types::{GcBox, GcBoxHeader, GcBoxInner, GcColor, Invariant},
    Gc,
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
        self.context.backward_barrier(
            unsafe { GcBox::erase(parent.ptr) },
            child.map(|p| unsafe { GcBox::erase(p.ptr) }),
        )
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
            .forward_barrier(parent.map(|p| unsafe { GcBox::erase(p.ptr) }), unsafe {
                GcBox::erase(child.ptr)
            })
    }

    #[inline]
    pub(crate) fn allocate<T: Collect + 'gc>(&self, t: T) -> NonNull<GcBoxInner<T>> {
        self.context.allocate(t)
    }

    #[inline]
    pub(crate) fn upgrade(&self, gc_box: GcBox) -> bool {
        self.context.upgrade(gc_box)
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
    pub(crate) fn resurrect(&self, gc_box: GcBox) {
        self.context.resurrect(gc_box)
    }
}

/// Handle value given by arena callbacks during garbage collection, which must be passed through
/// `Collect::trace` implementations.
#[repr(transparent)]
pub struct Collection {
    context: Context,
}

impl Collection {
    #[inline]
    pub fn metrics(&self) -> &Metrics {
        self.context.metrics()
    }

    #[inline]
    pub(crate) fn trace(&self, gc_box: GcBox) {
        self.context.trace(gc_box)
    }

    #[inline]
    pub(crate) fn trace_weak(&self, gc_box: GcBox) {
        self.context.trace_weak(gc_box)
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
pub(crate) enum EarlyStop {
    BeforeSweep,
    AfterSweep,
}

pub(crate) struct Context {
    metrics: Metrics,
    phase: Cell<Phase>,
    #[cfg(feature = "tracing")]
    phase_span: Cell<tracing::Span>,

    root_needs_trace: Cell<bool>,

    // A linked list of all allocated `GcBox`es.
    all: Cell<Option<GcBox>>,

    // A copy of the head of `all` at the end of `Phase::Mark`.
    // During `Phase::Sweep`, we free all white allocations on this list.
    // Any allocations created *during* `Phase::Sweep` will be added to `all`,
    // but `sweep` will *not* be updated. This ensures that we keep allocations
    // alive until we've had a chance to trace them.
    sweep: Cell<Option<GcBox>>,

    // The most recent black object that we encountered during `Phase::Sweep`.
    // When we free objects, we update this `GcBox.next` to remove them from
    // the linked list.
    sweep_prev: Cell<Option<GcBox>>,

    /// A queue of gray objects, used during `Phase::Mark`.
    /// This holds traceable objects that have yet to be traced.
    /// When we enter `Phase::Mark`, we push `root` to this queue.
    gray: RefCell<Vec<GcBox>>,

    // A queue of gray objects that became gray as a result
    // of a write barrier.
    gray_again: RefCell<Vec<GcBox>>,
}

impl Drop for Context {
    fn drop(&mut self) {
        struct DropAll<'a>(&'a Metrics, Option<GcBox>);

        impl<'a> Drop for DropAll<'a> {
            fn drop(&mut self) {
                if let Some(gc_box) = self.1.take() {
                    let mut drop_resume = DropAll(self.0, Some(gc_box));
                    while let Some(gc_box) = drop_resume.1.take() {
                        let header = gc_box.header();
                        drop_resume.1 = header.next();
                        self.0.mark_gc_deallocated(header.size_of_box());
                        // SAFETY: the context owns its GC'd objects
                        unsafe { free_gc_box(gc_box) }
                    }
                }
            }
        }

        let _guard = PhaseGuard::enter(&self, Some(Phase::Drop));
        DropAll(&self.metrics, self.all.get());
    }
}

impl Context {
    pub(crate) unsafe fn new() -> Context {
        let metrics = Metrics::new();
        Context {
            phase: Cell::new(Phase::Sleep),
            #[cfg(feature = "tracing")]
            phase_span: Cell::new(PhaseGuard::span_for(&metrics, Phase::Sleep)),
            metrics,
            root_needs_trace: Cell::new(true),
            all: Cell::new(None),
            sweep: Cell::new(None),
            sweep_prev: Cell::new(None),
            gray: RefCell::new(Vec::new()),
            gray_again: RefCell::new(Vec::new()),
        }
    }

    #[inline]
    pub(crate) unsafe fn mutation_context<'gc>(&self) -> &Mutation<'gc> {
        mem::transmute::<&Self, &Mutation>(&self)
    }

    #[inline]
    fn collection_context(&self) -> &Collection {
        // SAFETY: `Collection` is `repr(transparent)`
        unsafe { mem::transmute::<&Self, &Collection>(self) }
    }

    #[inline]
    pub(crate) unsafe fn finalization_context<'gc>(&self) -> &Finalization<'gc> {
        mem::transmute::<&Self, &Finalization>(&self)
    }

    #[inline]
    pub(crate) fn metrics(&self) -> &Metrics {
        &self.metrics
    }

    #[inline]
    pub(crate) fn root_barrier(&self) {
        if self.phase.get() == Phase::Mark {
            self.root_needs_trace.set(true);
        }
    }

    #[inline]
    pub(crate) fn phase(&self) -> Phase {
        self.phase.get()
    }

    #[inline]
    pub(crate) fn gray_remaining(&self) -> bool {
        !self.gray.borrow().is_empty()
            || !self.gray_again.borrow().is_empty()
            || self.root_needs_trace.get()
    }

    // Do some collection work until either the debt goes down below the target amount or we have
    // finished the gc sweep phase. The unit of "work" here is a byte count of objects either turned
    // black or freed, so to completely collect a heap with 1000 bytes of objects should take 1000
    // units of work, whatever percentage of them are live or not.
    //
    // In order for this to be safe, at the time of call no `Gc` pointers can be live that are not
    // reachable from the given root object.
    //
    // If we are currently in `Phase::Sleep`, this will transition the collector to `Phase::Mark`.
    pub(crate) unsafe fn do_collection<R: Collect + ?Sized>(
        &self,
        root: &R,
        target_debt: f64,
        early_stop: Option<EarlyStop>,
    ) {
        self.do_collection_inner(root, target_debt, early_stop)
    }

    fn do_collection_inner<R: Collect + ?Sized>(
        &self,
        root: &R,
        mut target_debt: f64,
        early_stop: Option<EarlyStop>,
    ) {
        let mut entered = PhaseGuard::enter(self, None);

        if !(self.metrics.allocation_debt() > target_debt) {
            entered.log_progress("GC: paused");
            return;
        }

        loop {
            match self.phase.get() {
                Phase::Sleep => {
                    // Immediately enter the mark phase; no need to update metrics here.
                    entered.switch(Phase::Mark);
                    continue;
                }
                Phase::Mark => {
                    // We look for an object first in the normal gray queue, then the "gray again"
                    // queue. Objects from the normal gray queue count as regular work, but objects
                    // which are gray a second time have already been counted as work, so we don't
                    // double count them. Processing "gray again" objects later also gives them more
                    // time to be mutated again without triggering another write barrier.
                    let next_gray = if let Some(gc_box) = self.gray.borrow_mut().pop() {
                        self.metrics.mark_gc_traced(gc_box.header().size_of_box());
                        Some(gc_box)
                    } else if let Some(gc_box) = self.gray_again.borrow_mut().pop() {
                        Some(gc_box)
                    } else {
                        None
                    };

                    if let Some(gc_box) = next_gray {
                        // If we have an object in the gray queue, take one, trace it, and turn it
                        // black.

                        // Our `Collect::trace` call may panic, and if it does the object will be
                        // lost from the gray queue but potentially incompletely traced. By catching
                        // a panic during `Arena::collect()`, this could lead to memory unsafety.
                        //
                        // So, if the `Collect::trace` call panics, we need to add the popped object
                        // back to the `gray_again` queue. If the panic is caught, this will maybe
                        // give it some time to not panic before attempting to collect it again, and
                        // also this doesn't invalidate the collection debt math.
                        struct DropGuard<'a> {
                            cx: &'a Context,
                            gc_box: GcBox,
                        }

                        impl<'a> Drop for DropGuard<'a> {
                            fn drop(&mut self) {
                                self.cx.gray_again.borrow_mut().push(self.gc_box);
                            }
                        }

                        let guard = DropGuard { cx: self, gc_box };
                        debug_assert!(gc_box.header().is_live());
                        unsafe { gc_box.trace_value(self.collection_context()) }
                        gc_box.header().set_color(GcColor::Black);
                        mem::forget(guard);
                    } else if self.root_needs_trace.get() {
                        // We treat the root object as gray if `root_needs_trace` is set, and we
                        // process it at the end of the gray queue for the same reason as the "gray
                        // again" objects.
                        root.trace(self.collection_context());
                        self.root_needs_trace.set(false);
                    } else {
                        if early_stop == Some(EarlyStop::BeforeSweep) {
                            target_debt = f64::INFINITY;
                        } else {
                            // If we have no gray objects left, we enter the sweep phase.
                            entered.switch(Phase::Sweep);

                            // Set `sweep to the current head of our `all` linked list. Any new
                            // allocations during the newly-entered `Phase:Sweep` will update `all`,
                            // but will *not* be reachable from `this.sweep`.
                            self.sweep.set(self.all.get());

                            // No need to update metrics here.
                            continue;
                        }
                    }
                }
                Phase::Sweep => {
                    if early_stop == Some(EarlyStop::AfterSweep) {
                        target_debt = f64::INFINITY;
                    } else if let Some(mut sweep) = self.sweep.get() {
                        let sweep_header = sweep.header();

                        let next_box = sweep_header.next();
                        self.sweep.set(next_box);

                        match sweep_header.color() {
                            // If the next object in the sweep portion of the main list is white, we
                            // need to remove it from the main object list and destruct it.
                            GcColor::White => {
                                if let Some(sweep_prev) = self.sweep_prev.get() {
                                    sweep_prev.header().set_next(next_box);
                                } else {
                                    // If `sweep_prev` is None, then the sweep pointer is also the
                                    // beginning of the main object list, so we need to adjust it.
                                    debug_assert_eq!(self.all.get(), Some(sweep));
                                    self.all.set(next_box);
                                }
                                self.metrics.mark_gc_deallocated(sweep_header.size_of_box());

                                // SAFETY: this object is white, and wasn't traced by a `GcWeak`
                                // during this cycle, meaning it cannot have either strong or weak
                                // pointers, so we can drop the whole object.
                                unsafe { free_gc_box(sweep) }
                            }
                            // Keep the `GcBox` as part of the linked list if we traced a weak
                            // pointer to it. The weak pointer still needs access to the `GcBox` to
                            // be able to check if the object is still alive. We can only deallocate
                            // the `GcBox`, once there are no weak pointers left.
                            GcColor::WhiteWeak => {
                                self.sweep_prev.set(Some(sweep));
                                sweep_header.set_color(GcColor::White);
                                if sweep_header.is_live() {
                                    sweep_header.set_live(false);
                                    // SAFETY: Since this object is white, that means there are no
                                    // more strong pointers to this object, only weak pointers, so
                                    // we can safely drop its contents.
                                    unsafe { sweep.drop_in_place() }
                                }
                            }
                            // If the next object in the sweep portion of the main list is black, we
                            // need to keep it but turn it back white.
                            GcColor::Black => {
                                self.sweep_prev.set(Some(sweep));
                                self.metrics.mark_gc_remembered(sweep_header.size_of_box());
                                sweep_header.set_color(GcColor::White);
                            }
                            // No gray objects should be in this part of the main list, they should
                            // be added to the beginning of the list before the sweep pointer, so it
                            // should not be possible for us to encounter them here.
                            GcColor::Gray => {
                                debug_assert!(false, "unexpected gray object in sweep list")
                            }
                        }
                    } else {
                        self.sweep_prev.set(None);
                        self.root_needs_trace.set(true);
                        entered.switch(Phase::Sleep);
                        self.metrics.start_cycle();
                        // Collection is done, forcibly exit the loop.
                        target_debt = f64::INFINITY;
                    }
                }
                Phase::Drop => unreachable!(),
            }

            if !(self.metrics.allocation_debt() > target_debt) {
                entered.log_progress("GC: yielding...");
                return;
            }
        }
    }

    fn allocate<T: Collect>(&self, t: T) -> NonNull<GcBoxInner<T>> {
        let header = GcBoxHeader::new::<T>();
        header.set_next(self.all.get());
        header.set_live(true);
        header.set_needs_trace(T::needs_trace());

        let alloc_size = header.size_of_box();
        self.metrics.mark_gc_allocated(alloc_size);

        // Make the generated code easier to optimize into `T` being constructed in place or at the
        // very least only memcpy'd once.
        // For more information, see: https://github.com/kyren/gc-arena/pull/14
        let (gc_box, ptr) = unsafe {
            let mut uninitialized = Box::new(mem::MaybeUninit::<GcBoxInner<T>>::uninit());
            core::ptr::write(uninitialized.as_mut_ptr(), GcBoxInner::new(header, t));
            let ptr = NonNull::new_unchecked(Box::into_raw(uninitialized) as *mut GcBoxInner<T>);
            (GcBox::erase(ptr), ptr)
        };

        self.all.set(Some(gc_box));
        if self.phase.get() == Phase::Sweep && self.sweep_prev.get().is_none() {
            self.sweep_prev.set(self.all.get());
        }

        ptr
    }

    #[inline]
    fn backward_barrier(&self, parent: GcBox, child: Option<GcBox>) {
        // During the marking phase, if we are mutating a black object, we may add a white object to
        // it and invalidate the invariant that black objects may not point to white objects. Turn
        // the black parent object gray to prevent this.
        //
        // NOTE: This also adds the pointer to the gray_again queue even if `header.needs_trace()`
        // is false, but this is not harmful (just wasteful). There's no reason to call a barrier on
        // a pointer that can't adopt other pointers, so we skip the check.
        if self.phase.get() == Phase::Mark
            && parent.header().color() == GcColor::Black
            && child
                .map(|c| matches!(c.header().color(), GcColor::White | GcColor::WhiteWeak))
                .unwrap_or(true)
        {
            // Outline the actual barrier code (which is somewhat expensive and won't be executed
            // often) to promote the inlining of the write barrier.
            #[cold]
            fn barrier(this: &Context, parent: GcBox) {
                parent.header().set_color(GcColor::Gray);
                this.gray_again.borrow_mut().push(parent);
            }
            barrier(&self, parent);
        }
    }

    #[inline]
    fn forward_barrier(&self, parent: Option<GcBox>, child: GcBox) {
        // During the marking phase, if we are mutating a black object, we may add a white object
        // to it and invalidate the invariant that black objects may not point to white objects.
        // Immediately trace the child white object to turn it gray (or black) to prevent this.
        if self.phase.get() == Phase::Mark
            && parent
                .map(|p| p.header().color() == GcColor::Black)
                .unwrap_or(true)
        {
            self.trace(child);
        }
    }

    #[inline]
    fn trace(&self, gc_box: GcBox) {
        let header = gc_box.header();
        match header.color() {
            GcColor::Black | GcColor::Gray => {}
            GcColor::White | GcColor::WhiteWeak => {
                if header.needs_trace() {
                    // A white traceable object is not in the gray queue, becomes gray and enters
                    // the normal gray queue.
                    header.set_color(GcColor::Gray);
                    debug_assert!(header.is_live());
                    self.gray.borrow_mut().push(gc_box);
                } else {
                    // A white object that doesn't need tracing simply becomes black.
                    header.set_color(GcColor::Black);
                    self.metrics.mark_gc_traced(header.size_of_box());
                }
            }
        }
    }

    #[inline]
    fn trace_weak(&self, gc_box: GcBox) {
        let header = gc_box.header();
        if header.color() == GcColor::White {
            header.set_color(GcColor::WhiteWeak);
        }
    }

    /// Determines whether or not a Gc pointer is safe to be upgraded.
    /// This is used by weak pointers to determine if it can safely upgrade to a strong pointer.
    #[inline]
    fn upgrade(&self, gc_box: GcBox) -> bool {
        let header = gc_box.header();

        // This object has already been freed, definitely not safe to upgrade.
        if !header.is_live() {
            return false;
        }

        // Consider the different possible phases of the GC:
        // * In `Phase::Sleep`, the GC is not running, so we can upgrade.
        //   If the newly-created `Gc` or `GcCell` survives the current `arena.mutate`
        //   call, then the situtation is equivalent to having copied an existing `Gc`/`GcCell`,
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
        if self.phase.get() == Phase::Sweep && header.color() == GcColor::WhiteWeak {
            return false;
        }
        true
    }

    #[inline]
    fn resurrect(&self, gc_box: GcBox) {
        let header = gc_box.header();
        debug_assert_eq!(self.phase.get(), Phase::Mark);
        debug_assert!(header.is_live());
        if matches!(header.color(), GcColor::White | GcColor::WhiteWeak) {
            header.set_color(GcColor::Gray);
            self.gray.borrow_mut().push(gc_box);
        }
    }
}

// SAFETY: the gc_box must never be accessed after calling this function.
unsafe fn free_gc_box<'gc>(mut gc_box: GcBox) {
    if gc_box.header().is_live() {
        // If the alive flag is set, that means we haven't dropped the inner value of this object,
        gc_box.drop_in_place();
    }
    gc_box.dealloc();
}

/// Helper type for managing phase transitions.
struct PhaseGuard<'a> {
    cx: &'a Context,
    #[cfg(feature = "tracing")]
    span: tracing::span::EnteredSpan,
}

impl<'a> Drop for PhaseGuard<'a> {
    fn drop(&mut self) {
        #[cfg(feature = "tracing")]
        {
            let span = mem::replace(&mut self.span, tracing::Span::none().entered());
            self.cx.phase_span.set(span.exit());
        }
    }
}

impl<'a> PhaseGuard<'a> {
    fn enter(cx: &'a Context, phase: Option<Phase>) -> Self {
        if let Some(phase) = phase {
            cx.phase.set(phase);
        }

        Self {
            cx,
            #[cfg(feature = "tracing")]
            span: {
                let mut span = cx.phase_span.replace(tracing::Span::none());
                if let Some(phase) = phase {
                    span = Self::span_for(&cx.metrics, phase);
                }
                span.entered()
            },
        }
    }

    fn switch(&mut self, phase: Phase) {
        self.cx.phase.set(phase);

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
            phase = tracing::field::debug(self.cx.phase.get()),
            allocated = self.cx.metrics.total_allocation(),
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
