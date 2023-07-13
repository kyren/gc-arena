use std::{alloc::Layout, marker::PhantomData, ptr::NonNull};

use allocator_api2::{
    alloc::{AllocError, Allocator, Global},
    boxed, vec,
};

use crate::{
    collect::Collect,
    context::{Collection, Mutation},
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
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self::new_in(mc, Global)
    }
}

impl<'gc, A> MetricsAlloc<'gc, A> {
    pub fn new_in(mc: &Mutation<'gc>, allocator: A) -> Self {
        Self {
            metrics: mc.metrics().clone(),
            allocator,
            _marker: PhantomData,
        }
    }
}

unsafe impl<'gc, A: Allocator> Allocator for MetricsAlloc<'gc, A> {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let ptr = self.allocator.allocate(layout)?;
        self.metrics.mark_external_allocation(layout.size());
        Ok(ptr)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        self.metrics.mark_external_deallocation(layout.size());
        self.allocator.deallocate(ptr, layout);
    }

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let ptr = self.allocator.allocate_zeroed(layout)?;
        self.metrics.mark_external_allocation(layout.size());
        Ok(ptr)
    }

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

unsafe impl<'gc, A: 'static> Collect for MetricsAlloc<'gc, A> {
    #[inline]
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl Collect for Global {
    #[inline]
    fn needs_trace() -> bool {
        false
    }
}

unsafe impl<T: Collect, A: Collect + Allocator> Collect for boxed::Box<T, A> {
    #[inline]
    fn needs_trace() -> bool {
        T::needs_trace() || A::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: &Collection) {
        boxed::Box::allocator(self).trace(cc);
        (**self).trace(cc)
    }
}

unsafe impl<T: Collect, A: Collect + Allocator> Collect for vec::Vec<T, A> {
    #[inline]
    fn needs_trace() -> bool {
        T::needs_trace() || A::needs_trace()
    }

    #[inline]
    fn trace(&self, cc: &Collection) {
        self.allocator().trace(cc);
        for v in self {
            v.trace(cc);
        }
    }
}
