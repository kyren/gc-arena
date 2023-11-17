use core::cell::RefCell;

use alloc::{collections::VecDeque, vec::Vec};

use crate::{
    barrier::Unlock, context::Finalizer, lock::RefLock, types::GcColor, Collect, Gc, GcWeak,
    Mutation,
};

/// Monitor for objects that are about to be freed by the garbage collector.
///
/// Allows you to register any `Gc<'gc, T>` pointer. Once registered, if that object was *going* to
/// be freed otherwise, it will instead be placed into this queue.
pub struct FinalizeQueue<'gc, T>(Gc<'gc, FinalizeQueueInner<'gc, T>>);

unsafe impl<'gc, T: Collect> Collect for FinalizeQueue<'gc, T> {
    fn trace(&self, cc: &crate::Collection) {
        self.0.trace(cc);
    }
}

impl<'gc, T: Collect> FinalizeQueue<'gc, T> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        let inner = Gc::new(
            mc,
            FinalizeQueueInner {
                registered: RefCell::new(Vec::new()),
                finalized: Gc::new(mc, RefLock::new(VecDeque::new())),
            },
        );

        mc.register_finalizer(inner.ptr);

        Self(inner)
    }

    /// Register a pointer to be placed into the `FinalizeQueue` if it would otherwise be freed.
    ///
    /// A pointer registered here will enter the queue *once per registration*. Once it enters the
    /// queue, it remains alive and can be pulled from the queue. If an object pulled from the queue
    /// is then lost, it will simply be freed unless it is again registered into a `FinalizeQueue`.
    ///
    /// Objects can be registered into multiple queues. There is a defined priority for objects to
    /// enter a `FinalizationQueue`: the object will enter queues created later before it enters
    /// queues created earlier.
    pub fn register(&self, item: Gc<'gc, T>) {
        self.0.registered.borrow_mut().push(Gc::downgrade(item));
    }

    pub fn poll(&self) -> Option<Gc<'gc, T>> {
        // SAFETY: No new pointers are adopted
        unsafe { self.0.finalized.unlock_unchecked() }
            .borrow_mut()
            .pop_front()
    }
}

struct FinalizeQueueInner<'gc, T> {
    registered: RefCell<Vec<GcWeak<'gc, T>>>,
    finalized: Gc<'gc, RefLock<VecDeque<Gc<'gc, T>>>>,
}

unsafe impl<'gc, T: Collect> Collect for FinalizeQueueInner<'gc, T> {
    fn trace(&self, cc: &crate::Collection) {
        // SAFETY:
        //
        // The `registered` list is not traced at all, because we know that only otherwise live
        // values are ever inside the registered list.
        //
        // We know that objects are live when they are added to the `registered` list. We also
        // know that `FinalizeQueueInner::finalize` will be called at the end of the mark phase,
        // and that that method will take every object that is not already black and place it into
        // the `finalized` list. Therefore, no value in the `registered` list can ever be freed,
        // a white object cannot be freed until it lives *past* the end of one Mark phase, and no
        // white object can be in the `registered` list when the the mark phase ends.
        //
        // TODO: Soundness relies on the fact that it is impossible to call
        // `FinalizeQueue::register` from *another* finalizer, therefore putting a dead pointer in
        // the `registered` list after `FinalizeQueueInner::finalize` is called but before the end
        // of the Mark phase. This is impossible currently because the finalizer API is not public.
        // If it is made public, this will not necessarily be sound and may need to be changed.

        self.finalized.trace(cc);
    }
}

impl<'gc, T: Collect> Finalizer<'gc> for FinalizeQueueInner<'gc, T> {
    fn finalize(&self, mc: &Mutation<'gc>) {
        self.registered.borrow_mut().retain(|p| {
            let p = p.upgrade(mc).unwrap();
            let color = unsafe { p.ptr.as_ref() }.header.color();
            debug_assert!(color != GcColor::Gray);
            // Since we are at the end of the mark cycle, if the color of the object is not black,
            // then we know the object *would* have been collected.
            if color != GcColor::Black {
                self.finalized.borrow_mut(mc).push_back(p);
                false
            } else {
                true
            }
        });
    }
}
