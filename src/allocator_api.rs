use core::{alloc::Layout, marker::PhantomData, ops::Index, ptr::NonNull};

use allocator_api2::{
    alloc::{AllocError, Allocator, Global},
    boxed, vec,
};

use crate::{
    barrier::{DerefWrite, IndexWrite},
    collect::{Collect, Trace},
    context::Mutation,
    metrics::Metrics,
    types::Invariant,
};

#[derive(Clone)]
pub struct MetricsAlloc<'gc, A = Global> {
    metrics: Metrics,
    allocator: A,
    _marker: Invariant<'gc>,
}

impl<'gc> MetricsAlloc<'gc> {
    #[inline]
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self::new_in(mc, Global)
    }

    /// `MetricsAlloc` is normally branded with the `'gc` branding lifetime to ensure that it is not
    /// placed in the wrong arena or used outside of the enclosing arena.
    ///
    /// This is actually completely artificial and only used as a lint: `gc_arena::metrics::Metrics`
    /// has no lifetime at all. Therefore, we can safely provide a method that returns a
    /// `MetricsAlloc` with an arbitrary lifetime.
    ///
    /// NOTE: Use `MetricsAlloc::new` if at all possible, because it is harder to misuse.
    #[inline]
    pub fn from_metrics(metrics: Metrics) -> Self {
        Self::from_metrics_in(metrics, Global)
    }
}

impl<'gc, A> MetricsAlloc<'gc, A> {
    #[inline]
    pub fn new_in(mc: &Mutation<'gc>, allocator: A) -> Self {
        Self {
            metrics: mc.metrics().clone(),
            allocator,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn from_metrics_in(metrics: Metrics, allocator: A) -> Self {
        Self {
            metrics,
            allocator,
            _marker: PhantomData,
        }
    }
}

unsafe impl<'gc, A: Allocator> Allocator for MetricsAlloc<'gc, A> {
    #[inline]
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let ptr = self.allocator.allocate(layout)?;
        self.metrics.mark_external_allocation(layout.size());
        Ok(ptr)
    }

    #[inline]
    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        self.metrics.mark_external_deallocation(layout.size());
        self.allocator.deallocate(ptr, layout);
    }

    #[inline]
    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let ptr = self.allocator.allocate_zeroed(layout)?;
        self.metrics.mark_external_allocation(layout.size());
        Ok(ptr)
    }

    #[inline]
    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        let ptr = self.allocator.grow(ptr, old_layout, new_layout)?;
        self.metrics
            .mark_external_allocation(new_layout.size() - old_layout.size());
        Ok(ptr)
    }

    #[inline]
    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        let ptr = self.allocator.grow_zeroed(ptr, old_layout, new_layout)?;
        self.metrics
            .mark_external_allocation(new_layout.size() - old_layout.size());
        Ok(ptr)
    }

    #[inline]
    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        let ptr = self.allocator.shrink(ptr, old_layout, new_layout)?;
        self.metrics
            .mark_external_deallocation(old_layout.size() - new_layout.size());
        Ok(ptr)
    }
}

unsafe impl<'gc, A: 'static> Collect<'gc> for MetricsAlloc<'gc, A> {
    const NEEDS_TRACE: bool = false;
}

unsafe impl<'gc> Collect<'gc> for Global {
    const NEEDS_TRACE: bool = false;
}

unsafe impl<'gc, T, A> Collect<'gc> for boxed::Box<T, A>
where
    T: Collect<'gc> + ?Sized,
    A: Collect<'gc> + Allocator,
{
    const NEEDS_TRACE: bool = T::NEEDS_TRACE || A::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        cc.trace(&**self);
        cc.trace(boxed::Box::allocator(self));
    }
}

unsafe impl<T: ?Sized, A: Allocator> DerefWrite for boxed::Box<T, A> {}

unsafe impl<'gc, T: Collect<'gc>, A: Collect<'gc> + Allocator> Collect<'gc> for vec::Vec<T, A> {
    const NEEDS_TRACE: bool = T::NEEDS_TRACE || A::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        for v in self {
            cc.trace(v);
        }
        cc.trace(self.allocator());
    }
}

unsafe impl<T, A: Allocator> DerefWrite for vec::Vec<T, A> {}
unsafe impl<T, A: Allocator, I> IndexWrite<I> for vec::Vec<T, A>
where
    [T]: IndexWrite<I>,
    Self: Index<I>,
{
}
