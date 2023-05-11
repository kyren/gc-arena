use alloc::boxed::Box;
use alloc::vec::Vec;
use core::cell::{Cell, RefCell};
use core::marker::PhantomData;
use core::mem;
use core::ptr::NonNull;

use crate::arena::ArenaParameters;
use crate::collect::Collect;
use crate::types::{GcBox, GcBoxHeader, GcBoxInner, GcColor, Invariant};

/// Handle value given by arena callbacks during construction and mutation. Allows allocating new
/// `Gc` pointers and internally mutating values held by `Gc` pointers.
#[derive(Copy, Clone)]
pub struct MutationContext<'gc, 'context> {
    _invariant: Invariant<'gc>,
    context: &'context Context,
}

impl<'gc, 'context> MutationContext<'gc, 'context> {
    #[inline]
    pub(crate) fn allocate<T: 'gc + Collect>(self, t: T) -> NonNull<GcBoxInner<T>> {
        self.context.allocate(t)
    }

    #[inline]
    pub(crate) fn write_barrier(self, gc_box: GcBox) {
        self.context.write_barrier(gc_box)
    }

    #[inline]
    pub(crate) fn upgrade(self, gc_box: GcBox) -> bool {
        self.context.upgrade(gc_box)
    }
}

/// Handle value given by arena callbacks during garbage collection, which must be passed through
/// `Collect::trace` implementations.
#[derive(Copy, Clone)]
pub struct CollectionContext<'context> {
    context: &'context Context,
}

impl<'context> CollectionContext<'context> {
    #[inline]
    pub(crate) fn trace(self, gc_box: GcBox) {
        self.context.trace(gc_box)
    }

    #[inline]
    pub(crate) fn trace_weak(&self, gc_box: GcBox) {
        self.context.trace_weak(gc_box)
    }
}

pub(crate) struct Context {
    parameters: ArenaParameters,

    phase: Cell<Phase>,
    root_needs_trace: Cell<bool>,
    total_allocated: Cell<usize>,
    remembered_size: Cell<usize>,
    wakeup_total: Cell<usize>,
    allocation_debt: Cell<f64>,

    // A linked list of all allocated `GcBox`es.
    all: Cell<Option<GcBox>>,

    // A copy of the head of `all` at the end of `Phase::Propagate`.
    // During `Phase::Sweep`, we free all white allocations on this list.
    // Any allocations created *during* `Phase::Sweep` will be added to `all`,
    // but `sweep` will *not* be updated. This ensures that we keep allocations
    // alive until we've had a chance to trace them.
    sweep: Cell<Option<GcBox>>,

    // The most recent black object that we encountered during `Phase::Sweep`.
    // When we free objects, we update this `GcBox.next` to remove them from
    // the linked list.
    sweep_prev: Cell<Option<GcBox>>,

    /// A queue of gray objects, used during `Phase::Propagate`.
    /// This holds traceable objects that have yet to be traced.
    /// When we enter `Phase::Propagate`, we push `root` to this queue.
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

        DropAll(self.all.get());
    }
}

impl Context {
    pub(crate) unsafe fn new(parameters: ArenaParameters) -> Context {
        Context {
            parameters,
            phase: Cell::new(Phase::Propagate),
            root_needs_trace: Cell::new(true),
            total_allocated: Cell::new(0),
            remembered_size: Cell::new(0),
            wakeup_total: Cell::new(0),
            allocation_debt: Cell::new(0.0),
            all: Cell::new(None),
            sweep: Cell::new(None),
            sweep_prev: Cell::new(None),
            gray: RefCell::new(Vec::new()),
            gray_again: RefCell::new(Vec::new()),
        }
    }

    // Creates a MutationContext with an unbounded 'gc lifetime.
    #[inline]
    pub(crate) unsafe fn mutation_context<'gc, 'context>(
        &'context self,
    ) -> MutationContext<'gc, 'context> {
        MutationContext {
            _invariant: PhantomData,
            context: self,
        }
    }

    #[inline]
    pub(crate) fn allocation_debt(&self) -> f64 {
        self.allocation_debt.get()
    }

    #[inline]
    pub(crate) fn total_allocated(&self) -> usize {
        self.total_allocated.get()
    }

    #[inline]
    pub(crate) fn remembered_size(&self) -> usize {
        self.remembered_size.get()
    }

    // If the garbage collector is currently in the sleep phase,
    // add the root to the gray queue and transition to the `Propagate` phase.
    #[inline]
    pub(crate) fn wake(&self) {
        if self.phase.get() == Phase::Sleep {
            self.phase.set(Phase::Propagate);
            self.root_needs_trace.set(true);
        }
    }

    #[inline]
    pub(crate) fn root_barrier(&self) {
        if self.phase.get() == Phase::Propagate {
            self.root_needs_trace.set(true);
        }
    }

    // Do some collection work until we have either reached the target amount of work or are in the
    // sleeping gc phase. The unit of "work" here is a byte count of objects either turned black
    // or freed, so to completely collect a heap with 1000 bytes of objects should take 1000 units
    // of work, whatever percentage of them are live or not. Returns the amount of work actually
    // performed, which may be less if we are entering the sleep phase.
    //
    // In order for this to be safe, at the time of call no `Gc` pointers can be live that are not
    // reachable from the given root object.
    pub(crate) unsafe fn do_collection<R: Collect>(&self, root: &R, work: f64) -> f64 {
        self.do_collection_inner(root, work)
    }

    fn do_collection_inner<R: Collect>(&self, root: &R, work: f64) -> f64 {
        let mut work_done = 0.0;
        let cc = CollectionContext { context: self };

        while work > work_done {
            match self.phase.get() {
                Phase::Propagate => {
                    // We look for an object first in the normal gray queue, then the "gray again"
                    // queue. Objects from the normal gray queue count as regular work, but objects
                    // which are gray a second time have already been counted as work, so we don't
                    // double count them. Processing "gray again" objects later also gives them more
                    // time to be mutated again without triggering another write barrier.
                    let next_gray = if let Some(gc_box) = self.gray.borrow_mut().pop() {
                        let gray_size = gc_box.header().size_of_box() as f64;
                        work_done += gray_size;
                        self.allocation_debt
                            .set((self.allocation_debt.get() - gray_size).max(0.0));
                        Some(gc_box)
                    } else if let Some(gc_box) = self.gray_again.borrow_mut().pop() {
                        Some(gc_box)
                    } else {
                        None
                    };

                    if let Some(gc_box) = next_gray {
                        // If we have an object in the gray queue, take one, trace it, and turn it
                        // black.
                        // SAFETY: we know gray objects are always live.
                        unsafe { gc_box.trace_value(cc) }
                        gc_box.header().set_color(GcColor::Black);
                    } else if self.root_needs_trace.get() {
                        // We treat the root object as gray if `root_needs_trace` is set, and we
                        // process it at the end of the gray queue for the same reason as the "gray
                        // again" objects.
                        root.trace(cc);
                        self.root_needs_trace.set(false);
                    } else {
                        // If we have no gray objects left, we enter the sweep phase.
                        self.phase.set(Phase::Sweep);

                        // Set `sweep to the current head of our `all` linked list. Any new allocations
                        // during the newly-entered `Phase:Sweep` will update `all`, but will *not*
                        // be reachable from `this.sweep`.
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
                                let sweep_size = sweep_header.size_of_box();
                                self.total_allocated
                                    .set(self.total_allocated.get() - sweep_size);
                                work_done += sweep_size as f64;
                                self.allocation_debt
                                    .set((self.allocation_debt.get() - sweep_size as f64).max(0.0));

                                // SAFETY: this object is white, and wasn't traced by a `GcWeak` during this cycle,
                                // meaning it cannot have either strong or weak pointers, so we can drop the whole object.
                                unsafe { free_gc_box(sweep) }
                            }
                            // Keep the `GcBox` as part of the linked list if we traced a weak pointer
                            // to it. The weak pointer still needs access to the `GcBox` to be able to
                            // check if the object is still alive. We can only deallocate the `GcBox`,
                            // once there are no weak pointers left.
                            GcColor::WhiteWeak => {
                                self.sweep_prev.set(Some(sweep));
                                sweep_header.set_color(GcColor::White);
                                if sweep_header.is_live() {
                                    sweep_header.set_live(false);
                                    // SAFETY: Since this object is white, that means there are no more strong pointers
                                    // to this object, only weak pointers, so we can safely drop its contents.
                                    unsafe { sweep.drop_in_place() }
                                }
                            }
                            // If the next object in the sweep portion of the main list is black, we
                            // need to keep it but turn it back white.
                            GcColor::Black => {
                                self.sweep_prev.set(Some(sweep));
                                self.remembered_size
                                    .set(self.remembered_size.get() + sweep_header.size_of_box());
                                sweep_header.set_color(GcColor::White);
                            }
                            // No gray objects should be in this part of the main list, they should be
                            // added to the beginning of the list before the sweep pointer, so it should
                            // not be possible for us to encounter them here.
                            GcColor::Gray => {
                                debug_assert!(false, "unexpected gray object in sweep list")
                            }
                        }
                    } else {
                        // We are done sweeping, so enter the sleeping phase.
                        self.sweep_prev.set(None);
                        self.phase.set(Phase::Sleep);

                        // Do not let debt or remembered size accumulate across cycles.
                        // When we enter sleep, zero them out.
                        self.allocation_debt.set(0.0);
                        let remembered_size = self.remembered_size.replace(0);

                        let sleep =
                            f64_to_usize(remembered_size as f64 * self.parameters.pause_factor)
                                .min(self.parameters.min_sleep);

                        self.wakeup_total.set(self.total_allocated.get() + sleep);
                    }
                }
                Phase::Sleep => break,
            }
        }

        work_done
    }

    fn allocate<T: Collect>(&self, t: T) -> NonNull<GcBoxInner<T>> {
        let header = GcBoxHeader::new::<T>();
        header.set_next(self.all.get());
        header.set_live(true);
        header.set_needs_trace(T::needs_trace());

        let alloc_size = header.size_of_box();
        self.total_allocated
            .set(self.total_allocated.get() + alloc_size);
        if self.phase.get() == Phase::Sleep && self.total_allocated.get() > self.wakeup_total.get()
        {
            self.wake();
        }

        if self.phase.get() != Phase::Sleep {
            self.allocation_debt.set(
                self.allocation_debt.get()
                    + alloc_size as f64
                    + alloc_size as f64 / self.parameters.timing_factor,
            );
        }

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
        if self.phase.get() == Phase::Propagate && header.color() == GcColor::Black {
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
        // * In `Phase::Propagate`:
        //   If the newly-created `Gc` or `GcCell` survives the current `arena.mutate`
        //   call, then it must have been stored somewhere, triggering a write barrier.
        //   This will ensure that the new `Gc`/`GcCell` gets traced (if it's now reachable)
        //   before we transition to `Phase::Sweep`.
        //
        // * In `Phase::Sweep`:
        //   If the allocation is `WhiteWeak`, then it's impossile for it to have been freshly-created
        //   during this `Phase::Sweep`. `WhiteWeak` is only  set when a white `GcWeak/GcWeakCell` is traced.
        //   A `GcWeak/GcWeakCell` must be created from an existing `Gc/GcCell` via `downgrade()`, so
        //   `WhiteWeak` means that a `GcWeak` / `GcWeakCell` existed during the last `Phase::Propagate.`
        //
        //   Therefore, a `WhiteWeak` object is guaranteed to be deallocated during this `Phase::Sweep`,
        //   and we must not upgrade it.
        //
        //   Conversely, it's always safe to upgrade a white object that is not `WhiteWeak`.
        //   In order to call `upgrade`, you must have a `GcWeak/GcWeakCell`. Since it is not `WhiteWeak`
        //   there cannot have been any `GcWeak/GcWeakCell`s during the last `Phase::Propagate`, so
        //   the weak pointer must have been created during this `Phase::Sweep`.
        //   This is only possible if the underlying allocation was freshly-created - if the allocation existed during
        //   `Phase::Propagate` but was not traced, then it must have been unreachable,
        //   which means that the user wouldn't have been able to call `downgrade`.
        //   Therefore, we can safely upgrade, knowing that the object will not be freed
        //   during this phase, despite being white.
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
    Propagate,
    Sweep,
    Sleep,
}

/// Rounds a floating point number to an unsigned integer.
///
/// If the floating point number is outside the bounds of the unsigned
/// integer, the number is clamped.
///
/// This methods works in no_std environments too.
#[inline]
fn f64_to_usize(input: f64) -> usize {
    // As per the Rustonomicon, the cast to usize is truncating.
    // TODO: Use f64::round when that is available in no_std. See:
    // https://github.com/rust-lang/rust/issues/50145
    (input + 0.5) as usize
}

#[cfg(test)]
mod test {
    use super::f64_to_usize;

    #[test]
    fn test_clamp_f64() {
        assert_eq!(f64_to_usize(f64::MIN), 0);
        assert_eq!(f64_to_usize(-100.0), 0);
        assert_eq!(f64_to_usize(-1.0), 0);
        assert_eq!(f64_to_usize(-0.6), 0);
        assert_eq!(f64_to_usize(0.0), 0);
        assert_eq!(f64_to_usize(0.4), 0);
        assert_eq!(f64_to_usize(0.5 - f64::EPSILON), 0);
        assert_eq!(f64_to_usize(0.5), 1);
        assert_eq!(f64_to_usize(0.6), 1);
        assert_eq!(f64_to_usize(1.0), 1);
        assert_eq!(f64_to_usize(100.0), 100);
        assert_eq!(f64_to_usize(usize::MAX as f64), usize::MAX);
        assert_eq!(f64_to_usize(f64::MAX), usize::MAX);
    }

    // This tests loss of precision, which only happens when `usize::MAX`
    // is large enough. when `usize` is `u32`, it's small enough that we
    // don't lose precision, so only run this test on 64-bit platforms.
    #[cfg(all(feature = "std", target_pointer_width = "64"))]
    #[test]
    fn test_clamp_f64_precision() {
        fn std_impl(input: f64) -> usize {
            input.round().min(usize::MAX as f64) as usize
        }

        // Precision is lost both using the no_std impl and the std impl
        assert_eq!(std_impl((usize::MAX - 1) as f64) as usize, usize::MAX);
        assert_eq!(f64_to_usize((usize::MAX - 1) as f64), usize::MAX);
    }
}
