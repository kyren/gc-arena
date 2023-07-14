use alloc::{boxed::Box, vec::Vec};
use core::{
    cell::{Cell, RefCell},
    mem,
    ptr::NonNull,
};

use crate::{
    collect::Collect,
    metrics::Metrics,
    types::{GcBox, GcBoxHeader, GcBoxInner, GcColor, Invariant},
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

    #[inline]
    pub(crate) fn allocate<T: 'gc + Collect>(&self, t: T) -> NonNull<GcBoxInner<T>> {
        self.context.allocate(t)
    }

    #[inline]
    pub(crate) fn write_barrier(&self, gc_box: GcBox) {
        self.context.write_barrier(gc_box)
    }

    #[inline]
    pub(crate) fn upgrade(&self, gc_box: GcBox) -> bool {
        self.context.upgrade(gc_box)
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
    // of a `write_barrier` call.
    gray_again: RefCell<Vec<GcBox>>,
}

impl Drop for Context {
    fn drop(&mut self) {
        struct DropAll(Option<GcBox>);

        impl Drop for DropAll {
            fn drop(&mut self) {
                if let Some(gc_box) = self.0.take() {
                    let mut drop_resume = DropAll(Some(gc_box));
                    while let Some(gc_box) = drop_resume.0.take() {
                        drop_resume.0 = gc_box.header().next();
                        // SAFETY: the context owns its GC'd objects
                        unsafe { free_gc_box(gc_box) }
                    }
                }
            }
        }

        let _guard = PhaseGuard::enter(&self, Some(Phase::Drop));
        DropAll(self.all.get());
    }
}

impl Context {
    pub(crate) unsafe fn new() -> Context {
        Context {
            metrics: Metrics::new(),
            phase: Cell::new(Phase::Sleep),
            #[cfg(feature = "tracing")]
            phase_span: Cell::new(tracing::Span::none()),
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
    pub(crate) fn metrics(&self) -> &Metrics {
        &self.metrics
    }

    #[inline]
    pub(crate) fn root_barrier(&self) {
        if self.phase.get() == Phase::Mark {
            self.root_needs_trace.set(true);
        }
    }

    // Do some collection work until we have either reached the target amount of work or have
    // finished the gc sweep phase. The unit of "work" here is a byte count of objects either
    // turned black or freed, so to completely collect a heap with 1000 bytes of objects should take
    // 1000 units of work, whatever percentage of them are live or not.
    //
    // In order for this to be safe, at the time of call no `Gc` pointers can be live that are not
    // reachable from the given root object.
    //
    // If we are currently in `Phase::Sleep`, this will transition the collector to
    // `Phase::Mark`.
    pub(crate) unsafe fn do_collection<R: Collect>(&self, root: &R, work: f64) {
        self.do_collection_inner(root, work)
    }

    fn do_collection_inner<R: Collect>(&self, root: &R, work: f64) {
        let mut debt = self.metrics.allocation_debt();
        let target_debt = debt - work;

        let mut entered = PhaseGuard::enter(
            self,
            // Calling `Context::do_collection` always transitions away from `Phase::Sleep` to
            // `Phase::Mark`.
            (self.phase.get() == Phase::Sleep).then(|| Phase::Mark),
        );

        entered.log_progress("GC: running...");

        while debt > target_debt {
            match self.phase.get() {
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
                        // If we have no gray objects left, we enter the sweep phase.
                        entered.switch(Phase::Sweep);

                        // Set `sweep to the current head of our `all` linked list. Any new
                        // allocations during the newly-entered `Phase:Sweep` will update `all`, but
                        // will *not* be reachable from `this.sweep`.
                        self.sweep.set(self.all.get());
                    }
                }
                Phase::Sweep => {
                    if let Some(mut sweep) = self.sweep.get() {
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
                    }
                }
                Phase::Sleep => break,
                Phase::Drop => unreachable!(),
            }

            debt = self.metrics.allocation_debt();
        }

        entered.log_progress("GC: yielding...");
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
    fn write_barrier(&self, gc_box: GcBox) {
        // During the propagating phase, if we are mutating a black object, we may add a white
        // object to it and invalidate the invariant that black objects may not point to white
        // objects. Turn black obejcts to gray to prevent this.
        let header = gc_box.header();
        if self.phase.get() == Phase::Mark && header.color() == GcColor::Black {
            header.set_color(GcColor::Gray);

            // Outline the actual enqueueing code (which is somewhat expensive and won't be
            // executed often) to promote the inlining of the write barrier.
            #[cold]
            fn enqueue(this: &Context, gc_box: GcBox) {
                this.gray_again.borrow_mut().push(gc_box);
            }

            enqueue(&self, gc_box);
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
                    self.gray.borrow_mut().push(gc_box);
                } else {
                    // A white object that doesn't need tracing simply becomes black.
                    header.set_color(GcColor::Black);
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
        //   If the allocation is `WhiteWeak`, then it's impossile for it to have been freshly-
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
}

// SAFETY: the gc_box must never be accessed after calling this function.
unsafe fn free_gc_box<'gc>(mut gc_box: GcBox) {
    if gc_box.header().is_live() {
        // If the alive flag is set, that means we haven't dropped the inner value of this object,
        gc_box.drop_in_place();
    }
    gc_box.dealloc();
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Phase {
    Mark,
    Sweep,
    Sleep,
    Drop,
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
        self.cx.phase_span.set(self.exit());
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
                    span = Self::span_for(cx, phase);
                }
                span.entered()
            },
        }
    }

    fn switch(&mut self, phase: Phase) {
        self.cx.phase.set(phase);

        #[cfg(feature = "tracing")]
        {
            self.exit();
            self.span = Self::span_for(self.cx, phase).entered();
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
}

#[cfg(feature = "tracing")]
impl<'a> PhaseGuard<'a> {
    fn exit(&mut self) -> tracing::Span {
        mem::replace(&mut self.span, tracing::Span::none().entered()).exit()
    }

    fn span_for(cx: &Context, phase: Phase) -> tracing::Span {
        // The sleep phase doesn't have an explicit span.
        if phase == Phase::Sleep {
            return tracing::Span::none();
        }
        tracing::debug_span!(
            target: "gc_arena",
            "gc_arena",
            id = cx.metrics.arena_id(),
            ?phase,
        )
    }
}
