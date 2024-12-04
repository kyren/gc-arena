use alloc::rc::Rc;
use core::cell::Cell;

/// Tuning parameters for a given garbage collected [`crate::Arena`].
///
/// Any allocation that occurs during a collection cycle will incur "debt" that is exactly equal to
/// the allocated bytes. This "debt" is paid off by running the collection algorithm some amount of
/// time proportional to the debt. Exactly how much "debt" is paid off and in what proportion by the
/// different parts of the collection algorithm is configured by the chosen values here. We refer to
/// the amount of "debt" paid off by running the collection algorithm as "work".
///
/// The most important idea behind choosing these tuning parameters is that we always want the
/// collector (when it is not paused) to deterministically run *faster* than allocation, to make
/// sure that the collection cycle finishes and memory does not grow without bound. If we are tuning
/// for low pause time however, it is also important that not *too* many costly operations are run
/// within a single call to [`crate::Arena::collect_debt`], and this goal is in tension with the
/// first, more important goal.
///
/// How these two goals are balanced is that we must choose our tuning parameters so that the total
/// amount of "work" performed to either *remember* or *free* one byte of allocated data is always
/// *less than one*, and this makes the collector deterministically run faster than the rate of
/// allocation (which is crucial). The closer the amount of "work" performed to remember or free
/// one byte is to 1.0, the slower the collector will go and the higher the maximum amount of
/// used memory will be. The closer the amount of "work" performed to remember or free one byte
/// is to 0.0, the faster the collector will go and the closer it will get to behaving like a
/// stop-the-world collector.
///
/// All live pointers in a cycle are either remembered or freed once, but it is important that
/// *both paths* require less than one unit of "work" per byte to fully complete. There is no way to
/// predict a priori the ratio of remembered vs forgotten values, so if either path takes too close
/// to or over 1.0 unit of work per byte to complete, collection may run too slowly.
///
/// # Factors that control the pause time
///
/// `pause_factor` is fairly self explanatory. Setting this too low will reduce the time of the
/// [`crate::arena::CollectionPhase::Sleeping`] phase but this is not directly harmful (it can even
/// be set to zero!), setting it much larger than 1.0 will make the collector wait a very long time
/// before collecting again, and usually not what you want.
///
/// `min_sleep` is also self explanatory and usually does not need changing from the default value.
/// It should always be relatively small.
///
/// # Timing factors for remembered values
///
/// Every live `Gc` value in an [`crate::Arena`] that is reachable from the root is "remembered".
/// Every remembered value will always have exactly three things done to it in a given cycle:
///
/// 1) It will at some point be found and marked as reachable queued for tracing. When this happens,
///    `mark_factor * alloc_size` work is recorded.
/// 2) Entries in the queue for tracing will eventually be traced by having their
///    [`crate::Collect::trace`] method called. At this time, `trace_factor * alloc_size` work is
///    recorded. Calling `Collect::trace` will usually mark other pointers as reachable and queue
///    them for tracing if they have not already been, so this step may also transitively perform
///    other work, but each step is only performed exactly once for each individual remembered
///    value.
/// 3) During the [`crate::arena::CollectionPhase::Sweeping`] phase, each remembered value has to
///    be iterated over in the sweep list and removed from it. This a small, constant amount of
///    work that is very fast, but it should always perform *some* work to keep pause time low, so
///    `keep_factor * alloc_size` work is recorded.
///
/// # Timing factors for forgotten values
///
/// Allocated values that are not reachable in a GC cycle are simpler than
/// remembered values. Only two operations are performed on them, and only during
/// [`crate::arena::CollectionPhase::Sweeping`]: dropping and freeing.
///
/// Usually these two operations will take place at the same time, but if a value is only *weakly*
/// reachable (through [`crate::GcWeak`] pointers), then the value may be dropped at a different
/// time than its backing allocation is actually freed.
///
/// When a value is dropped, `drop_factor * alloc_size` work is recorded, and when it is freed
/// `free_factor * alloc_size` work is recorded.
#[derive(Debug, Copy, Clone)]
pub struct Pacing {
    /// Controls the length of the [`crate::arena::CollectionPhase::Sleeping`] phase.
    ///
    /// At the start of a new GC cycle, the collector will wait until the live size reaches
    /// `<current heap size> + <previous remembered size> * pause_multiplier` before starting
    /// collection.
    ///
    /// External memory is ***not*** included in the "remembered size" for the purposes of
    /// calculating a new cycle's pause time.
    pub pause_factor: f64,

    /// The minimum length of the [`crate::arena::CollectionPhase::Sleeping`] phase.
    ///
    /// if the calculated sleep amount using `pause_factor` is lower than `min_sleep`, this will
    /// be used instead. This is mostly useful when the heap is very small to prevent rapidly
    /// restarting collections.
    pub min_sleep: usize,

    /// The multiplicative factor for "work" performed per byte when a `Gc` value is first marked as
    /// reachable and queued for tracing.
    pub mark_factor: f64,

    /// The multiplicative factor for "work" performed per byte when a `Gc` value has its
    /// [`crate::Collect::trace`] method called.
    pub trace_factor: f64,

    /// The multiplicative factor for "work" performed per byte when a reachable `Gc` value is
    /// iterated over during [`crate::arena::CollectionPhase::Sweeping`].
    pub keep_factor: f64,

    /// The multiplicative factor for "work" performed per byte when a `Gc`
    /// value that is only weakly reachable is dropped (but not freed) during
    /// [`crate::arena::CollectionPhase::Sweeping`].
    pub drop_factor: f64,

    /// The multiplicative factor for "work" performed per byte when a forgotten `Gc` value is freed
    /// during [`crate::arena::CollectionPhase::Sweeping`].
    pub free_factor: f64,
}

pub const DEFAULT_PACING: Pacing = Pacing {
    pause_factor: 0.5,
    min_sleep: 4096,
    mark_factor: 0.1,
    trace_factor: 0.4,
    keep_factor: 0.05,
    drop_factor: 0.2,
    free_factor: 0.3,
};

impl Default for Pacing {
    #[inline]
    fn default() -> Pacing {
        DEFAULT_PACING
    }
}

pub const STOP_THE_WORLD_PACING: Pacing = Pacing {
    pause_factor: 1.0,
    min_sleep: 4096,
    mark_factor: 0.0,
    trace_factor: 0.0,
    keep_factor: 0.0,
    drop_factor: 0.0,
    free_factor: 0.0,
};

impl Pacing {
    /// Construct a good default "stop-the-world" [`Pacing`] configuration.
    ///
    /// This has all of the work factors set to zero so that as soon as the collector wakes from
    /// sleep, it will immediately perform a full collection.
    ///
    /// It is important to set the pause factor fairly high when configuring a collector this way
    /// (close to or even somewhat larger than 1.0).
    #[inline]
    pub fn stop_the_world() -> Pacing {
        STOP_THE_WORLD_PACING
    }
}

#[derive(Debug, Default)]
struct MetricsInner {
    pacing: Cell<Pacing>,

    total_gcs: Cell<usize>,
    total_gc_bytes: Cell<usize>,
    total_external_bytes: Cell<usize>,

    wakeup_amount: Cell<f64>,
    artificial_debt: Cell<f64>,

    // The number of external bytes that have been marked as allocated at the beginning of this
    // cycle.
    external_bytes_start: Cell<usize>,

    // Statistics for `Gc` allocations and deallocations that happen during a GC cycle.
    allocated_gc_bytes: Cell<usize>,
    dropped_gc_bytes: Cell<usize>,
    freed_gc_bytes: Cell<usize>,

    // Statistics for `Gc` pointers that have been marked gray and queued for tracing (the first
    // time only).
    queued_gcs: Cell<usize>,
    queued_gc_bytes: Cell<usize>,

    // Statistics for `Gc` pointers that have their contents traced.
    traced_gcs: Cell<usize>,
    traced_gc_bytes: Cell<usize>,

    // Statistics for reachable `Gc` pointers as they are iterated through during the sweep phase.
    remembered_gcs: Cell<usize>,
    remembered_gc_bytes: Cell<usize>,
}

#[derive(Clone)]
pub struct Metrics(Rc<MetricsInner>);

#[derive(Copy, Clone)]
pub struct CycleResult {
    pub remembered_gc_count: usize,
    pub rememberd_gc_size: usize,

    /// Set to `true` when all forgotten pointers were properly freed this cycle.
    ///
    /// Will not be true if any allocations were performed during the
    /// [`crate::arena::CollectionPhase::Sweeping`] phase.
    pub full_collection: bool,
}

impl Metrics {
    pub(crate) fn new() -> Self {
        let this = Self(Default::default());
        this.finish_cycle();
        this
    }

    /// Return a value identifying the arena, for logging purposes.
    #[cfg(feature = "tracing")]
    pub(crate) fn arena_id(&self) -> tracing::field::DebugValue<*const ()> {
        // Be very cheeky and use the `Metrics` address as a (temporally) unique ID.
        // TODO: use a monotonically increasing global counter instead?
        tracing::field::debug(Rc::as_ptr(&self.0) as *const ())
    }

    /// Sets the pacing parameters used by the collection algorithm.
    ///
    /// The factors that affect the gc pause time will not take effect until the start of the next
    /// collection.
    #[inline]
    pub fn set_pacing(&self, pacing: Pacing) {
        self.0.pacing.set(pacing);
    }

    /// Returns the current number of `Gc`s allocated that have not yet been freed.
    #[inline]
    pub fn total_gc_count(&self) -> usize {
        self.0.total_gcs.get()
    }

    /// Returns the total bytes allocated by all live `Gc` pointers.
    #[inline]
    pub fn total_gc_allocation(&self) -> usize {
        self.0.total_gc_bytes.get()
    }

    /// Returns the total bytes that have been marked as externally allocated.
    ///
    /// A call to [`Metrics::mark_external_allocation`] will increase this count, and a call to
    /// [`Metrics::mark_external_deallocation`] will decrease it.
    #[inline]
    pub fn total_external_allocation(&self) -> usize {
        self.0.total_external_bytes.get()
    }

    /// Returns the sum of `Metrics::total_gc_allocation()` and
    /// `Metrics::total_external_allocation()`.
    #[inline]
    pub fn total_allocation(&self) -> usize {
        self.0
            .total_gc_bytes
            .get()
            .saturating_add(self.0.total_external_bytes.get())
    }

    /// Call to mark that bytes have been externally allocated that are owned by an arena.
    ///
    /// This affects the GC pacing, marking external bytes as allocated will trigger allocation
    /// debt.
    #[inline]
    pub fn mark_external_allocation(&self, bytes: usize) {
        cell_update(&self.0.total_external_bytes, |b| b.saturating_add(bytes));
    }

    /// Call to mark that bytes which have been marked as allocated with
    /// [`Metrics::mark_external_allocation`] have been since deallocated.
    ///
    /// This affects the GC pacing, marking external bytes as deallocated will reduce allocation
    /// debt.
    ///
    /// It is safe, but may result in unspecified behavior (such as very weird GC pacing), if the
    /// amount of bytes marked for deallocation is greater than the number of bytes marked for
    /// allocation.
    #[inline]
    pub fn mark_external_deallocation(&self, bytes: usize) {
        cell_update(&self.0.total_external_bytes, |b| b.saturating_sub(bytes));
    }

    /// Add artificial debt equivalent to allocating the given number of bytes.
    ///
    /// This is different than marking external allocation because it will not show up in a call to
    /// [`Metrics::total_external_allocation`] or [`Metrics::total_allocation`] and instead *only*
    /// speeds up collection.
    #[inline]
    pub fn add_debt(&self, bytes: usize) {
        cell_update(&self.0.artificial_debt, |d| d + bytes as f64);
    }

    /// All arena allocation causes the arena to accumulate "allocation debt". This debt is then
    /// used to time incremental garbage collection based on the tuning parameters in the current
    /// `Pacing`. The allocation debt is measured in bytes, but will generally increase at a rate
    /// faster than that of allocation so that collection will always complete.
    #[inline]
    pub fn allocation_debt(&self) -> f64 {
        let total_gcs = self.0.total_gcs.get();
        if total_gcs == 0 {
            // If we have no live `Gc`s, then there is no possible collection to do so always
            // return zero debt.
            return 0.0;
        }

        // Right now, we treat allocating an external byte as 1.0 units of debt and deallocating an
        // external byte as 1.0 units of work (we also treat freeing more external bytes than were
        // allocated in a cycle as performing *no* work). The result is that the *total* increase
        // of externally allocated bytes (allocated minus freed) incurs debt exactly the same as GC
        // allocated bytes.
        let allocated_external_bytes = self
            .0
            .total_external_bytes
            .get()
            .checked_sub(self.0.external_bytes_start.get())
            .unwrap_or(0);

        let allocated_bytes =
            self.0.allocated_gc_bytes.get() as f64 + allocated_external_bytes as f64;

        // Every allocation after the `wakeup_amount` in a cycle is a debit.
        let cycle_debits =
            allocated_bytes - self.0.wakeup_amount.get() + self.0.artificial_debt.get();

        // If our debits are not positive, then we know the total debt is not positive.
        if cycle_debits <= 0.0 {
            return 0.0;
        }

        let pacing = self.0.pacing.get();

        let cycle_credits = self.0.queued_gc_bytes.get() as f64 * pacing.mark_factor
            + self.0.traced_gc_bytes.get() as f64 * pacing.trace_factor
            + self.0.remembered_gc_bytes.get() as f64 * pacing.keep_factor
            + self.0.dropped_gc_bytes.get() as f64 * pacing.drop_factor
            + self.0.freed_gc_bytes.get() as f64 * pacing.free_factor;

        (cycle_debits - cycle_credits).max(0.0)
    }

    pub(crate) fn finish_cycle(&self) -> CycleResult {
        let pacing = self.0.pacing.get();
        let total_gcs = self.0.total_gcs.get();
        let remembered_gcs = self.0.remembered_gcs.get();
        let remembered_size = self.0.remembered_gc_bytes.get();
        let wakeup_amount =
            (remembered_size as f64 * pacing.pause_factor).max(pacing.min_sleep as f64);

        // If we had no allocations during the entire sweep phase, then every unreachable pointer
        // should be freed and our remembered count should be the same as our total count.
        //
        // We are relying on the count of total_gcs and remembered_gcs being *completely* accurate
        // here to determine whether a full collection took place.
        let full_collection = total_gcs == remembered_gcs;

        // If we have completed a full collection, then we forcibly *reset* the artifical debt to
        // zero. Otherwise, we set it to carry-over any remaining current debt.
        //
        // This prevents uselessly speeding up the next cycle when we can be *sure* that every
        // pointer is already accounted for, and limits the impact of very high induced debt to a
        // single full collection cycle.
        let artificial_debt = if full_collection {
            0.0
        } else {
            self.allocation_debt()
        };

        self.0.wakeup_amount.set(wakeup_amount);
        self.0.artificial_debt.set(artificial_debt);

        self.0
            .external_bytes_start
            .set(self.0.total_external_bytes.get());
        self.0.allocated_gc_bytes.set(0);
        self.0.dropped_gc_bytes.set(0);
        self.0.freed_gc_bytes.set(0);
        self.0.queued_gcs.set(0);
        self.0.queued_gc_bytes.set(0);
        self.0.traced_gcs.set(0);
        self.0.traced_gc_bytes.set(0);
        self.0.remembered_gcs.set(0);
        self.0.remembered_gc_bytes.set(0);

        CycleResult {
            remembered_gc_count: remembered_gcs,
            rememberd_gc_size: remembered_size,
            full_collection,
        }
    }

    #[inline]
    pub(crate) fn mark_gc_allocation(&self, bytes: usize) {
        cell_update(&self.0.total_gcs, |c| c + 1);
        cell_update(&self.0.total_gc_bytes, |b| b + bytes);
        cell_update(&self.0.allocated_gc_bytes, |b| b.saturating_add(bytes));
    }

    #[inline]
    pub(crate) fn mark_gc_drop(&self, bytes: usize) {
        cell_update(&self.0.dropped_gc_bytes, |b| b.saturating_add(bytes));
    }

    #[inline]
    pub(crate) fn mark_gc_deallocation(&self, bytes: usize) {
        cell_update(&self.0.total_gcs, |c| c - 1);
        cell_update(&self.0.total_gc_bytes, |b| b - bytes);
        cell_update(&self.0.freed_gc_bytes, |b| b.saturating_add(bytes));
    }

    #[inline]
    pub(crate) fn mark_gc_queued(&self, bytes: usize) {
        cell_update(&self.0.queued_gcs, |c| c + 1);
        cell_update(&self.0.queued_gc_bytes, |b| b.saturating_add(bytes));
    }

    #[inline]
    pub(crate) fn mark_gc_traced(&self, bytes: usize) {
        cell_update(&self.0.traced_gcs, |c| c + 1);
        cell_update(&self.0.traced_gc_bytes, |b| b.saturating_add(bytes));
    }

    #[inline]
    pub(crate) fn mark_gc_remembered(&self, bytes: usize) {
        cell_update(&self.0.remembered_gcs, |c| c + 1);
        cell_update(&self.0.remembered_gc_bytes, |b| b.saturating_add(bytes));
    }
}

// TODO: Use `Cell::update` when it is available, see:
// https://github.com/rust-lang/rust/issues/50186
#[inline]
fn cell_update<T: Copy>(c: &Cell<T>, f: impl FnOnce(T) -> T) {
    c.set(f(c.get()))
}
