use core::{cell::RefCell, mem};

use alloc::{collections::VecDeque, vec::Vec};

use crate::{
    barrier::Unlock, lock::RefLock, metrics::Metrics, types::GcColor, Collect, Gc, GcWeak, Mutation,
};

/// Monitor for objects that are about to be freed by the garbage collector.
///
/// Allows you to register any `Gc<'gc, T>` pointer. Once registered, if that object was *going* to
/// be freed otherwise, it will instead be placed into this queue.
pub struct FinalizeQueue<'gc, T>(Gc<'gc, InnerQueue<'gc, T>>);

unsafe impl<'gc, T: Collect> Collect for FinalizeQueue<'gc, T> {
    fn trace(&self, cc: &crate::Collection) {
        self.0.trace(cc);
    }
}

impl<'gc, T: Collect> FinalizeQueue<'gc, T> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        let finalized = Gc::new(
            mc,
            RefLock::new(Finalized {
                metrics: mc.metrics().clone(),
                finalized: VecDeque::new(),
            }),
        );
        let inner = Gc::new(
            mc,
            InnerQueue {
                metrics: mc.metrics().clone(),
                registered: RefCell::new(Vec::new()),
                finalized,
            },
        );

        unsafe { inner.ptr.as_ref() }.header.set_trace_at_end(true);

        Self(inner)
    }

    /// Register a pointer to be placed into the `FinalizeQueue` if it would otherwise be freed.
    ///
    /// A pointer registered here will enter the queue *once per registration*. Once it enters the
    /// queue, it remains alive and can be pulled from the queue. If an object pulled from the queue
    /// is then lost, it will simply be freed unless it is again registered into a `FinalizeQueue`.
    pub fn register(&self, item: Gc<'gc, T>) {
        self.0.register(item);
    }

    /// Returns an object from the queue if one is queued.
    pub fn poll(&self) -> Option<Gc<'gc, T>> {
        self.0.poll()
    }
}

struct InnerQueue<'gc, T> {
    metrics: Metrics,
    registered: RefCell<Vec<GcWeak<'gc, T>>>,
    finalized: Gc<'gc, RefLock<Finalized<'gc, T>>>,
}

unsafe impl<'gc, T: Collect> Collect for InnerQueue<'gc, T> {
    fn trace(&self, cc: &crate::Collection) {
        self.finalized.trace(cc);

        self.registered.borrow_mut().retain(|p| {
            let prev_color = unsafe { p.inner.ptr.as_ref() }.header.color();
            debug_assert!(prev_color != GcColor::Gray);
            let p = p.trace_strong(cc).unwrap();
            // Since we are at the end of the mark cycle, if the color of the object was not already
            // black, then we know the object *would* have been collected.
            if prev_color != GcColor::Black {
                // SAFETY: The adopted pointer is known to be reachable already from
                // `GcWeak::trace_strong`.
                unsafe { self.finalized.unlock_unchecked() }
                    .borrow_mut()
                    .push_back(p);
                false
            } else {
                true
            }
        });
    }
}

impl<'gc, T> InnerQueue<'gc, T> {
    fn register(&self, item: Gc<'gc, T>) {
        let old_size = self.allocation_size();
        self.registered.borrow_mut().push(Gc::downgrade(item));
        let new_size = self.allocation_size();

        mark_size_change(&self.metrics, old_size, new_size);
    }

    fn poll(&self) -> Option<Gc<'gc, T>> {
        // SAFETY: No new pointers are adopted
        unsafe { self.finalized.unlock_unchecked() }
            .borrow_mut()
            .pop_front()
    }

    fn allocation_size(&self) -> usize {
        self.registered.borrow().capacity() * mem::size_of::<GcWeak<'gc, T>>()
    }
}

impl<'gc, T> Drop for InnerQueue<'gc, T> {
    fn drop(&mut self) {
        self.metrics
            .mark_external_deallocation(self.allocation_size());
    }
}

struct Finalized<'gc, T> {
    metrics: Metrics,
    finalized: VecDeque<Gc<'gc, T>>,
}

unsafe impl<'gc, T: Collect> Collect for Finalized<'gc, T> {
    fn trace(&self, cc: &crate::Collection) {
        self.finalized.trace(cc);
    }
}

impl<'gc, T> Drop for Finalized<'gc, T> {
    fn drop(&mut self) {
        self.metrics
            .mark_external_deallocation(self.allocation_size());
    }
}

impl<'gc, T> Finalized<'gc, T> {
    fn allocation_size(&self) -> usize {
        self.finalized.capacity() * mem::size_of::<Gc<'gc, T>>()
    }

    fn push_back(&mut self, ptr: Gc<'gc, T>) {
        let old_size = self.allocation_size();
        self.finalized.push_back(ptr);
        let new_size = self.allocation_size();

        mark_size_change(&self.metrics, old_size, new_size);
    }

    fn pop_front(&mut self) -> Option<Gc<'gc, T>> {
        self.finalized.pop_front()
    }
}

fn mark_size_change(metrics: &Metrics, old_size: usize, new_size: usize) {
    if new_size > old_size {
        metrics.mark_external_allocation(new_size - old_size);
    } else if old_size > new_size {
        metrics.mark_external_deallocation(old_size - new_size);
    }
}
