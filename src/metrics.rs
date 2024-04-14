use alloc::rc::Rc;
use core::cell::Cell;

/// Tuning parameters for a given garbage collected [`crate::Arena`].
#[derive(Debug, Copy, Clone)]
pub struct Pacing {
    pub(crate) pause_factor: f64,
    pub(crate) debit_factor: f64,
    pub(crate) min_sleep: usize,
}

/// Creates a default `Pacing` with `pause_factor` set to 0.5, `timing_factor` set to 1.5,
/// and `min_sleep` set to 4096.
impl Default for Pacing {
    #[inline]
    fn default() -> Pacing {
        const PAUSE_FACTOR: f64 = 0.5;
        const TIMING_FACTOR: f64 = 1.5;
        const MIN_SLEEP: usize = 4096;

        Pacing {
            pause_factor: PAUSE_FACTOR,
            debit_factor: timing_to_debit_factor(TIMING_FACTOR),
            min_sleep: MIN_SLEEP,
        }
    }
}

impl Pacing {
    /// The garbage collector will wait until the live size reaches `<current heap size> + <previous
    /// retained size> * pause_multiplier` before beginning a new collection. Must be >= 0.0,
    /// setting this to 0.0 causes the collector to never sleep longer than `min_sleep` before
    /// beginning a new collection.
    #[inline]
    pub fn with_pause_factor(mut self, pause_factor: f64) -> Pacing {
        assert!(pause_factor >= 0.0);
        self.pause_factor = pause_factor;
        self
    }

    /// The garbage collector will try and finish a collection by the time `<current heap size> *
    /// timing_factor` additional bytes are allocated. For example, if the collection is started
    /// when the arena has 100KB live data, and the timing_multiplier is 1.0, the collector should
    /// finish its final phase of this collection after another 100KB has been allocated. Must be >=
    /// 0.0, setting this to 0.0 causes the collector to behave like a stop-the-world collector.
    #[inline]
    pub fn with_timing_factor(mut self, timing_factor: f64) -> Pacing {
        self.debit_factor = timing_to_debit_factor(timing_factor);
        self
    }

    /// The minimum allocation amount during sleep before the arena starts collecting again. This is
    /// mostly useful when the heap is very small to prevent rapidly restarting collections.
    #[inline]
    pub fn with_min_sleep(mut self, min_sleep: usize) -> Pacing {
        self.min_sleep = min_sleep;
        self
    }
}

#[derive(Debug, Default)]
struct MetricsInner {
    pacing: Cell<Pacing>,

    total_gcs: Cell<usize>,
    total_gc_bytes: Cell<usize>,
    total_external_bytes: Cell<usize>,

    wakeup_amount: Cell<f64>,

    allocated_gc_bytes: Cell<usize>,
    traced_gcs: Cell<usize>,
    traced_gc_bytes: Cell<usize>,
    freed_gc_bytes: Cell<usize>,

    remembered_gcs: Cell<usize>,
    remembered_gc_bytes: Cell<usize>,

    allocated_external_bytes: Cell<usize>,
    freed_external_bytes: Cell<usize>,
}

#[derive(Clone)]
pub struct Metrics(Rc<MetricsInner>);

impl Metrics {
    pub(crate) fn new() -> Self {
        let this = Self(Default::default());
        this.start_cycle();
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

    /// Returns the total bytes allocated by the arena itself, used as the backing storage for `Gc`
    /// pointers.
    #[inline]
    pub fn total_gc_allocation(&self) -> usize {
        self.0.total_gc_bytes.get()
    }

    /// Returns the total bytes that have been marked as externally allocated.
    ///
    /// A call to `Metrics::mark_external_allocation` will increase this count, and a call to
    /// `Metrics::mark_external_deallocation` will decrease it.
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

        // Every allocation in a cycle is a debit.
        let raw_cycle_debits =
            self.0.allocated_gc_bytes.get() as f64 + self.0.allocated_external_bytes.get() as f64;

        // We do not begin collection at all until the allocations in a cycle reach the wakeup
        // amount.
        let cycle_debits = raw_cycle_debits - self.0.wakeup_amount.get();
        if cycle_debits <= 0.0 {
            return 0.0;
        }

        // Estimate the amount of external memory that has been traced assuming that each Gc
        // owns an even share of the external memory.
        let traced_external_estimate = self.0.traced_gcs.get() as f64 / total_gcs as f64
            * self.0.total_external_bytes.get() as f64;

        // Tracing or freeing memory counts as a credit.
        let cycle_credits = self.0.traced_gc_bytes.get() as f64
            + self.0.freed_gc_bytes.get() as f64
            + traced_external_estimate
            + self.0.freed_external_bytes.get() as f64;

        let debt = cycle_debits * self.0.pacing.get().debit_factor - cycle_credits;

        debt.max(0.0)
    }

    /// Call to mark that bytes have been externally allocated that are owned by an arena.
    ///
    /// This affects the GC pacing, marking external bytes as allocated will trigger allocation
    /// debt.
    #[inline]
    pub fn mark_external_allocation(&self, bytes: usize) {
        cell_update(&self.0.total_external_bytes, |b| b.saturating_add(bytes));
        cell_update(&self.0.allocated_external_bytes, |b| {
            b.saturating_add(bytes)
        });
    }

    /// Call to mark that bytes which have been marked as allocated with
    /// `Metrics::mark_external_allocation` have been since deallocated.
    ///
    /// This affects the GC pacing, marking external bytes as deallocated will reduce allocation
    /// debt.
    ///
    /// It is safe, but may result in unspecified behavior (such as very weird or non-existent gc
    /// pacing), if the amount of bytes marked for deallocation is greater than the number of bytes
    /// marked for allocation.
    #[inline]
    pub fn mark_external_deallocation(&self, bytes: usize) {
        cell_update(&self.0.total_external_bytes, |b| b.saturating_sub(bytes));
        cell_update(&self.0.freed_external_bytes, |b| b.saturating_add(bytes));
    }

    pub(crate) fn start_cycle(&self) {
        let pacing = self.0.pacing.get();
        let total_gcs = self.0.total_gcs.get();
        let wakeup_amount = if total_gcs == 0 {
            // If we have no live `Gc`s, then the root cannot possibly own any data, so we should
            // sleep for `min_sleep.`
            pacing.min_sleep as f64
        } else {
            // Estimate the amount of external memory that is remembered assuming that each Gc owns
            // an even share of the external memory.
            let remembered_external_size_estimate = self.0.remembered_gcs.get() as f64
                / total_gcs as f64
                * self.0.total_external_bytes.get() as f64;

            let remembered_size =
                self.0.remembered_gc_bytes.get() as f64 + remembered_external_size_estimate;

            (remembered_size * pacing.pause_factor).max(pacing.min_sleep as f64)
        };

        self.0.wakeup_amount.set(wakeup_amount);
        self.0.allocated_gc_bytes.set(0);
        self.0.freed_gc_bytes.set(0);
        self.0.traced_gcs.set(0);
        self.0.traced_gc_bytes.set(0);
        self.0.remembered_gcs.set(0);
        self.0.remembered_gc_bytes.set(0);
        self.0.allocated_external_bytes.set(0);
        self.0.freed_external_bytes.set(0);
    }

    #[inline]
    pub(crate) fn mark_gc_allocated(&self, bytes: usize) {
        cell_update(&self.0.total_gcs, |c| c + 1);
        cell_update(&self.0.total_gc_bytes, |b| b + bytes);
        cell_update(&self.0.allocated_gc_bytes, |b| b.saturating_add(bytes));
    }

    #[inline]
    pub(crate) fn mark_gc_traced(&self, bytes: usize) {
        cell_update(&self.0.traced_gcs, |c| c.saturating_add(1));
        cell_update(&self.0.traced_gc_bytes, |b| b.saturating_add(bytes));
    }

    #[inline]
    pub(crate) fn mark_gc_deallocated(&self, bytes: usize) {
        cell_update(&self.0.total_gcs, |c| c - 1);
        cell_update(&self.0.total_gc_bytes, |b| b - bytes);
        cell_update(&self.0.freed_gc_bytes, |b| b.saturating_add(bytes));
    }

    #[inline]
    pub(crate) fn mark_gc_remembered(&self, bytes: usize) {
        cell_update(&self.0.remembered_gcs, |c| c + 1);
        cell_update(&self.0.remembered_gc_bytes, |b| b + bytes);
    }
}

// TODO: Use `Cell::update` when it is available, see:
// https://github.com/rust-lang/rust/issues/50186
#[inline]
fn cell_update<T: Copy>(c: &Cell<T>, f: impl FnOnce(T) -> T) {
    c.set(f(c.get()))
}

// Computes the `debit_factor` (aka, the amount that debits are multiplied by) from the
// `timing_factor` configured by the user. It should always be > 1.0.
//
// Debits count for `1.0 + 1.0 / timing_factor` times their actual value. This means that if
// `wakeup_amount` is 100KB, and `timing_factor` is 1.0, then it will take 100KB more allocation
// before the collector finishes a cycle... when the cycle_debits hit 100KB, the cycle_credits must
// be 200KB to get back to 0 debt, and since the total heap size is now 200KB, this is a full cycle.
//
// Lowering `timing_factor` makes the collector behave more like a stop-the-world collector, and
// raising it slows the collector down. This factor is configured this way because the lowest
// allowable value (0.0) is a sensible stop-the-world behavior, and higher (finite) values are
// slower but still valid. If `debit_factor` ended up being <= 1.0, then we could not be sure that
// collection would ever *finish*.
fn timing_to_debit_factor(timing_factor: f64) -> f64 {
    assert!(timing_factor >= 0.0);
    1.0 + 1.0 / timing_factor
}
