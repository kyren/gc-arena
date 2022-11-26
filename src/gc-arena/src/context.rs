use alloc::boxed::Box;
use alloc::vec::Vec;
use core::cell::{Cell, RefCell, UnsafeCell};
use core::marker::PhantomData;
use core::mem::{self, ManuallyDrop};
use core::ptr::NonNull;

use crate::arena::ArenaParameters;
use crate::collect::Collect;
use crate::types::{GcBox, GcColor, GcFlags, Invariant};

/// Handle value given by arena callbacks during construction and mutation.  Allows allocating new
/// `Gc` pointers and internally mutating values held by `Gc` pointers.
#[derive(Copy, Clone)]
pub struct MutationContext<'gc, 'context> {
    _invariant: Invariant<'gc>,
    context: &'context Context,
}

impl<'gc, 'context> MutationContext<'gc, 'context> {
    pub(crate) unsafe fn allocate<T: 'gc + Collect>(self, t: T) -> NonNull<GcBox<T>> {
        self.context.allocate(t)
    }

    pub(crate) unsafe fn write_barrier(self, ptr: NonNull<GcBox<dyn Collect + 'gc>>) {
        self.context.write_barrier(ptr)
    }

    pub(crate) unsafe fn upgrade<T: 'gc + Collect>(self, ptr: NonNull<GcBox<T>>) -> bool {
        self.context.upgrade(ptr)
    }
}

/// Handle value given by arena callbacks during garbage collection, which must be passed through
/// `Collect::trace` implementations.
#[derive(Copy, Clone)]
pub struct CollectionContext<'context> {
    context: &'context Context,
}

impl<'context> CollectionContext<'context> {
    pub(crate) unsafe fn trace<T: Collect>(self, ptr: NonNull<GcBox<T>>) {
        self.context.trace(ptr)
    }
}

pub(crate) struct Context {
    parameters: ArenaParameters,

    phase: Cell<Phase>,
    total_allocated: Cell<usize>,
    remembered_size: Cell<usize>,
    wakeup_total: Cell<usize>,
    allocation_debt: Cell<f64>,

    // A linked list of all allocated `GcBox`es.
    all: Cell<Option<NonNull<GcBox<dyn Collect>>>>,

    // A copy of the head of `all` at the end of `Phase::Propagate`.
    // During `Phase::Sweep`, we free all white allocations on this list.
    // Any allocations created *during* `Phase::Sweep` will be added to `all`,
    // but `sweep` will *not* be updated. This ensures that we keep allocations
    // alive until we've had a chance to trace them.
    sweep: Cell<Option<NonNull<GcBox<dyn Collect>>>>,

    // The most recent black object that we encountered during `Phase::Sweep`.
    // When we free objects, we update this `GcBox.next` to remove them from
    // the linked list.
    sweep_prev: Cell<Option<NonNull<GcBox<dyn Collect>>>>,

    /// A queue of gray objects, used during `Phase::Propagate`.
    /// This holds traceable objects that have yet to be traced.
    /// When we enter `Phase::Propagate`, we push `root` to this queue.
    gray: RefCell<Vec<NonNull<GcBox<dyn Collect>>>>,

    // A queue of gray objects that became gray as a result
    // of a `write_barrier` call.
    gray_again: RefCell<Vec<NonNull<GcBox<dyn Collect>>>>,

    // The root object of the arena. This is only `None` during initialization,
    // while we're invoking the callback passed to `Arena::new` or `Arena::try_new`.
    // Afterwards, it should always be `Some`
    root: Option<NonNull<GcBox<dyn Collect>>>,
}

impl Drop for Context {
    fn drop(&mut self) {
        struct DropAll(Option<NonNull<GcBox<dyn Collect>>>);

        impl Drop for DropAll {
            fn drop(&mut self) {
                unsafe {
                    if let Some(ptr) = self.0.take() {
                        let mut drop_resume = DropAll(Some(ptr));
                        while let Some(ptr) = drop_resume.0.take() {
                            let gc_box = ptr.as_ref();
                            drop_resume.0 = gc_box.next.get();
                            free_gc_box(ptr);
                        }
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
            total_allocated: Cell::new(0),
            remembered_size: Cell::new(0),
            wakeup_total: Cell::new(0),
            allocation_debt: Cell::new(0.0),
            all: Cell::new(None),
            sweep: Cell::new(None),
            sweep_prev: Cell::new(None),
            gray: RefCell::new(Vec::new()),
            gray_again: RefCell::new(Vec::new()),
            root: None,
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

    // Must be called exactly once, after invoking the callback passed to `Arena::new` or
    // `Arena::try_new`. This sets the root object, and pushes it to the gray queue.
    //
    // # Safety: Must be called with the root object of the parent `Arena`.
    pub(crate) unsafe fn initialize<R: Collect>(&mut self, root: R) -> NonNull<GcBox<R>> {
        let root = self.allocate(root);
        let static_root = static_gc_box(root);
        self.root = Some(static_root);
        self.gray.borrow_mut().push(static_root);
        root
    }

    #[inline]
    pub(crate) fn allocation_debt(&self) -> f64 {
        self.allocation_debt.get()
    }

    #[inline]
    pub(crate) fn total_allocated(&self) -> usize {
        self.total_allocated.get()
    }

    // If the garbage collector is currently in the sleep phase,
    // add the root to the gray queue and transition to the `Propagate` phase.
    pub(crate) fn wake(&self) {
        if self.phase.get() == Phase::Sleep {
            self.phase.set(Phase::Propagate);
            self.gray.borrow_mut().push(self.root.unwrap());
        }
    }

    // Do some collection work until we have either reached the target amount of work or are in the
    // sleeping gc phase.  The unit of "work" here is a byte count of objects either turned black or
    // freed, so to completely collect a heap with 1000 bytes of objects should take 1000 units of
    // work, whatever percentage of them are live or not.  Returns the amount of work actually
    // performed, which may be less if we are entering the sleep phase.
    //
    // In order for this to be safe, at the time of call no `Gc` pointers can be live that are not
    // reachable from the given root object.
    pub(crate) unsafe fn do_collection(&mut self, work: f64) -> f64 {
        let mut work_done = 0.0;
        let cc = CollectionContext { context: self };

        while work > work_done {
            match self.phase.get() {
                Phase::Propagate => {
                    // We look for an object first in the normal gray queue, then the "gray again"
                    // queue.  Objects from the normal gray queue count as regular work, but objects
                    // which are gray a second time have already been counted as work, so we don't
                    // double count them.  Processing "gray again" objects later also gives them
                    // more time to be mutated again without triggering another write barrier.
                    let next_gray = if let Some(ptr) = self.gray.borrow_mut().pop() {
                        let gray_size = mem::size_of_val(ptr.as_ref()) as f64;
                        work_done += gray_size;
                        self.allocation_debt
                            .set((self.allocation_debt.get() - gray_size).max(0.0));
                        Some(ptr)
                    } else if let Some(ptr) = self.gray_again.borrow_mut().pop() {
                        Some(ptr)
                    } else {
                        None
                    };

                    if let Some(ptr) = next_gray {
                        // If we have an object in the gray queue, take one, trace it, and turn it
                        // black.
                        let gc_box = ptr.as_ref();
                        (*gc_box.value.get()).trace(cc);
                        gc_box.flags.set_color(GcColor::Black);
                    } else {
                        // If we have no objects left in the normal gray queue, we enter the sweep
                        // phase.
                        self.phase.set(Phase::Sweep);

                        // Set `sweep to the current head of our `all` linked list. Any new allocations
                        // during the newly-entered `Phase:Sweep` will update `all`, but will *not*
                        // be reachable from `this.sweep`.
                        self.sweep.set(self.all.get());
                    }
                }
                Phase::Sweep => {
                    if let Some(mut sweep_ptr) = self.sweep.get() {
                        let sweep = sweep_ptr.as_mut();
                        let sweep_size = mem::size_of_val(sweep);

                        let next_ptr = sweep.next.get();
                        self.sweep.set(next_ptr);

                        // If the next object in the sweep list is white, we need to remove it from
                        // the main list and destruct it, otherwise it should be black, and we
                        // simply turn it white again.
                        if sweep.flags.color() == GcColor::White {
                            if sweep.flags.traced_weak_ref() {
                                // Keep the `GcBox` as part of the linked list if we traced a weak pointer
                                // to it. The weak pointer still needs access to the `GcBox` to be able to
                                // check if the object is still alive. We can only deallocate the `GcBox`,
                                // once there are no weak pointers left.
                                self.sweep_prev.set(Some(sweep_ptr));
                                sweep.flags.set_traced_weak_ref(false);
                                if sweep.flags.is_live() {
                                    sweep.flags.set_live(false);
                                    // SAFETY: Since this object is white, that means there are no more strong pointers
                                    // to this object, only weak pointers, so we can safely drop its contents.
                                    ManuallyDrop::drop(&mut sweep.value);
                                }
                            } else {
                                // If the next object in the sweep portion of the main list is white, we
                                // need to remove it from the main object list and destruct it.
                                if let Some(sweep_prev) = self.sweep_prev.get() {
                                    sweep_prev.as_ref().next.set(next_ptr);
                                } else {
                                    // If `sweep_prev` is None, then the sweep pointer is also the
                                    // beginning of the main object list, so we need to adjust it.
                                    debug_assert_eq!(self.all.get(), Some(sweep_ptr));
                                    self.all.set(next_ptr);
                                }
                                self.total_allocated
                                    .set(self.total_allocated.get() - sweep_size);
                                work_done += sweep_size as f64;
                                self.allocation_debt
                                    .set((self.allocation_debt.get() - sweep_size as f64).max(0.0));
                                free_gc_box(sweep_ptr);
                            }
                        } else {
                            // If the next object in the sweep portion of the main list is black, we
                            // need to keep it but turn it back white.  No gray objects should be in
                            // this part of the main list, they should be added to the beginning of
                            // the list before the sweep pointer, so it should not be possible for
                            // us to encounter them here.
                            debug_assert_eq!(sweep.flags.color(), GcColor::Black);
                            self.sweep_prev.set(Some(sweep_ptr));
                            self.remembered_size
                                .set(self.remembered_size.get() + sweep_size);
                            sweep.flags.set_traced_weak_ref(false);
                            sweep.flags.set_color(GcColor::White);
                        }
                    } else {
                        // We are done sweeping, so enter the sleeping phase.
                        self.sweep_prev.set(None);
                        self.phase.set(Phase::Sleep);

                        // Do not let debt accumulate across cycles, when we enter sleep, zero the debt out.
                        self.allocation_debt.set(0.0);

                        let sleep = f64_to_usize(
                            self.remembered_size.get() as f64 * self.parameters.pause_factor,
                        )
                        .min(self.parameters.min_sleep);

                        self.wakeup_total.set(self.total_allocated.get() + sleep);
                    }
                }
                Phase::Sleep => break,
            }
        }

        work_done
    }

    unsafe fn allocate<T: Collect>(&self, t: T) -> NonNull<GcBox<T>> {
        let alloc_size = mem::size_of::<GcBox<T>>();
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

        let flags = GcFlags::new();
        flags.set_live(true);
        flags.set_needs_trace(T::needs_trace());

        // Make the generated code easier to optimize into `T` being constructed in place or at the
        // very least only memcpy'd once.
        // For more information, see: https://github.com/kyren/gc-arena/pull/14
        /*
        let ptr = NonNull::new_unchecked(Box::into_raw(Box::new(GcBox {
            flags: flags,
            next: Cell::new(self.all.get()),
            value: UnsafeCell::new(t),
        })));
        */
        let mut uninitialized = Box::new(mem::MaybeUninit::<GcBox<T>>::uninit());
        core::ptr::write(
            uninitialized.as_mut_ptr(),
            GcBox {
                flags: flags,
                next: Cell::new(self.all.get()),
                value: ManuallyDrop::new(UnsafeCell::new(t)),
            },
        );
        let ptr = NonNull::new_unchecked(Box::into_raw(uninitialized) as *mut GcBox<T>);

        self.all.set(Some(static_gc_box(ptr)));
        if self.phase.get() == Phase::Sweep && self.sweep_prev.get().is_none() {
            self.sweep_prev.set(self.all.get());
        }

        ptr
    }

    unsafe fn write_barrier<'gc>(&self, ptr: NonNull<GcBox<dyn Collect + 'gc>>) {
        // During the propagating phase, if we are mutating a black object, we may add a white
        // object to it and invalidate the invariant that black objects may not point to white
        // objects.  Turn black obejcts to gray to prevent this.
        let gc_box = ptr.as_ref();
        if self.phase.get() == Phase::Propagate && gc_box.flags.color() == GcColor::Black {
            gc_box.flags.set_color(GcColor::Gray);
            self.gray_again.borrow_mut().push(static_gc_box(ptr));
        }
    }

    unsafe fn trace<T: Collect>(&self, ptr: NonNull<GcBox<T>>) {
        let gc_box = ptr.as_ref();
        match gc_box.flags.color() {
            GcColor::Black | GcColor::Gray => {}
            GcColor::White => {
                if gc_box.flags.needs_trace() {
                    // A white traceable object is not in the gray queue, becomes gray and enters
                    // the normal gray queue.
                    gc_box.flags.set_color(GcColor::Gray);
                    self.gray.borrow_mut().push(static_gc_box(ptr));
                } else {
                    // A white object that doesn't need tracing simply becomes black.
                    gc_box.flags.set_color(GcColor::Black);
                }
            }
        }
    }

    /// Determines whether or not a Gc pointer is safe to be upgraded.
    /// This is used by weak pointers to determine if it can safely upgrade to a strong pointer.
    ///
    /// Safety: `ptr` must be a valid pointer to a GcBox<T>.
    unsafe fn upgrade<T: Collect>(&self, ptr: NonNull<GcBox<T>>) -> bool {
        let gc_box = ptr.as_ref();

        // This object has already been freed, definitely not safe to upgrade.
        if !gc_box.flags.is_live() {
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
        //   If the allocation is white and has the `traced_weak_ref()` flag set, then it's impossile
        //   for it to have been freshly-created during this `Phase::Sweep`. `traced_weak_ref` is only
        //   set when a `GcWeak/GcWeakCell` is traced. A `GcWeak/GcWeakCell` must be created from
        //   an existing `Gc/GcCell` via `downgrade()`, so `traced_weak_ref() == true` means that a
        //   `GcWeak` / `GcWeakCell` existed during the last `Phase::Propagate.`
        //
        //   Therefore, a white object with `traced_weak_ref() == true` is guaranteed to be deallocated
        //   during this `Phase::Sweep`, and we must not upgrade it.
        //
        //   Conversely, it's always safe to upgrade a white object with `traced_weak_ref() == false`.
        //   In order to call `upgrade`, you must have a `GcWeak/GcWeakCell`. Since `traced_weak_ref` is false,
        //   there cannot have been any `GcWeak/GcWeakCell`s during the last `Phase::Propagate`, so
        //   the weak pointer must have been created during this `Phase::Sweep`.
        //   This is only possible if the underlying allocation was freshly-created - if the allocation existed during
        //   `Phase::Propagate` but was not traced, then it must have been unreachable,
        //   which means that the user wouldn't have been able to call `downgrade`.
        //   Therefore, we can safely upgrade, knowing that the object will not be freed
        //   during this phase, despite being white.
        if self.phase.get() == Phase::Sweep
            && gc_box.flags.color() == GcColor::White
            && gc_box.flags.traced_weak_ref()
        {
            return false;
        }
        true
    }
}

unsafe fn free_gc_box<'gc>(mut ptr: NonNull<GcBox<dyn Collect + 'gc>>) {
    // Safety: We need to construct this mutable reference here - we *can not* take
    // it as a parameter. Stacked borrows requires that a reference from a function
    // parameter be live for the entire function, but we deallocate the memory at the end
    // of this function.
    // Constructing the reference here only requires that it be live where it's actually
    // used in the function.
    let ptr: &mut GcBox<dyn Collect + 'gc> = ptr.as_mut();
    if ptr.flags.is_live() {
        // If the alive flag is set, that means we haven't dropped the inner value of this object,
        ManuallyDrop::drop(&mut ptr.value);
    }
    drop(Box::from_raw(ptr));
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Phase {
    Propagate,
    Sweep,
    Sleep,
}

#[inline]
unsafe fn static_gc_box<'gc>(
    ptr: NonNull<GcBox<dyn Collect + 'gc>>,
) -> NonNull<GcBox<dyn Collect>> {
    mem::transmute(ptr)
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

    #[cfg(feature = "std")]
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
