use core::alloc::Layout;
use core::cell::Cell;
use core::marker::PhantomData;
use core::ptr::NonNull;
use core::{mem, ptr};

use crate::collect::Collect;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct GcBox(NonNull<GcBoxInner<()>>);

impl GcBox {
    #[inline(always)]
    pub(crate) unsafe fn erase<T: ?Sized>(ptr: NonNull<GcBoxInner<T>>) -> Self {
        // This cast is sound because `GcBoxInner` is `repr(C)`.
        let erased = ptr.as_ptr() as *mut GcBoxInner<()>;
        Self(NonNull::new_unchecked(erased))
    }

    #[inline(always)]
    fn erased_value(&self) -> *mut () {
        unsafe {
            let ptr = self.0.as_ptr();
            // Don't create a reference, to keep the full provenance.
            // Also, this gives us interior mutability "for free".
            ptr::addr_of_mut!((*ptr).value) as *mut ()
        }
    }

    #[inline(always)]
    pub(crate) fn flags(&self) -> &GcFlags {
        unsafe { &self.0.as_ref().flags }
    }

    #[inline(always)]
    pub(crate) fn next(&self) -> &Cell<Option<GcBox>> {
        unsafe { &self.0.as_ref().next }
    }

    #[inline(always)]
    pub(crate) fn size_of_box(&self) -> usize {
        let inner = unsafe { self.0.as_ref() };
        inner.vtable.box_layout.size()
    }

    #[inline(always)]
    pub(crate) unsafe fn trace_value(&self, cc: crate::CollectionContext) {
        let vtable = self.0.as_ref().vtable;
        (vtable.trace_value)(*self, cc)
    }

    #[inline(always)]
    pub(crate) unsafe fn drop_in_place(&mut self) {
        let vtable = self.0.as_ref().vtable;
        (vtable.drop_value)(*self)
    }

    #[inline(always)]
    pub(crate) unsafe fn dealloc(self) {
        let vtable = self.0.as_ref().vtable;
        let ptr = self.0.as_ptr() as *mut u8;
        alloc::alloc::dealloc(ptr, vtable.box_layout);
    }
}

#[repr(C)]
pub(crate) struct GcBoxInner<T: ?Sized> {
    flags: GcFlags,
    next: Cell<Option<GcBox>>,
    vtable: &'static CollectVtable,
    value: mem::ManuallyDrop<T>,
}

impl<T: ?Sized> GcBoxInner<T> {
    #[inline(always)]
    pub(crate) fn new(flags: GcFlags, next: Option<GcBox>, t: T) -> Self
    where
        T: Collect + Sized,
    {
        // Helper trait to allow borrowing a 'static reference.
        trait HasCollectVtable {
            const VTABLE: CollectVtable;
        }

        impl<T: Collect> HasCollectVtable for T {
            const VTABLE: CollectVtable = CollectVtable::vtable_for::<T>();
        }

        Self {
            flags,
            next: Cell::new(next),
            vtable: &<T as HasCollectVtable>::VTABLE,
            value: mem::ManuallyDrop::new(t),
        }
    }

    #[inline(always)]
    pub(crate) fn value(&self) -> &T {
        &*self.value
    }
}

struct CollectVtable {
    box_layout: Layout,
    drop_value: unsafe fn(GcBox),
    trace_value: unsafe fn(GcBox, crate::CollectionContext<'_>),
}

impl CollectVtable {
    #[inline(always)]
    const fn vtable_for<T: Collect>() -> Self {
        Self {
            box_layout: Layout::new::<GcBoxInner<T>>(),
            drop_value: Self::erased_drop_shim::<T>,
            trace_value: Self::erased_trace_shim::<T>,
        }
    }

    unsafe fn erased_drop_shim<T>(erased: GcBox) {
        ptr::drop_in_place(erased.erased_value() as *mut T)
    }

    unsafe fn erased_trace_shim<T: Collect>(erased: GcBox, cc: crate::CollectionContext<'_>) {
        let val = &*(erased.erased_value() as *mut T);
        val.trace(cc)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub(crate) enum GcColor {
    /// An object that has not yet been reached by tracing (if we're in a tracing phase).
    ///
    /// During `Phase::Sweep`, we will free all white objects
    /// that existed *before* the start of the current `Phase::Sweep`.
    /// Objects allocated during `Phase::Sweep` will be white, but will
    /// not be freed.
    White,
    /// An object reachable from a Black object, but that has not
    /// yet been traced using `Collect::trace`. We also mark black
    /// objects as gray during `Phase::Propagate` in response to a
    /// `write_barrier` call, so that we re-trace and find any objects
    /// newly reachable from the mutated object.
    Gray,
    /// An object that was reached during tracing. It will not be freed
    /// during `Phase::Sweep`. At the end of `Phase::Sweep`, all black
    /// objects will be reset to white.
    Black,
}

pub(crate) struct GcFlags(Cell<u8>);

impl GcFlags {
    #[inline]
    pub(crate) fn new() -> GcFlags {
        GcFlags(Cell::new(0))
    }

    #[inline]
    pub(crate) fn color(&self) -> GcColor {
        match self.0.get() & 0x3 {
            0x0 => GcColor::White,
            0x1 => GcColor::Gray,
            0x2 => GcColor::Black,
            // this is needed for the compiler to codegen a simple AND.
            // SAFETY: only possible extra value is 0x3,
            // and the only place where we set these bits is in set_color
            _ => unsafe { core::hint::unreachable_unchecked() },
        }
    }

    #[inline]
    pub(crate) fn set_color(&self, color: GcColor) {
        self.0.set(
            (self.0.get() & !0x3)
                | match color {
                    GcColor::White => 0x0,
                    GcColor::Gray => 0x1,
                    GcColor::Black => 0x2,
                },
        )
    }

    #[inline]
    pub(crate) fn needs_trace(&self) -> bool {
        self.0.get() & 0x4 != 0x0
    }

    /// This is `true` if we've traced a weak pointer during to this `GcBox`
    /// during the most recent `Phase::Propagate`. This is reset back to
    /// `false` during `Phase::Sweep`.
    #[inline]
    pub(crate) fn traced_weak_ref(&self) -> bool {
        self.0.get() & 0x8 != 0x0
    }

    /// Determines whether or not we've dropped the `dyn Collect` value
    /// stored in `GcBox.value`
    /// When we garbage-collect a `GcBox` that still has outstanding weak pointers,
    /// we set `alive` to false. When there are no more weak pointers remaining,
    /// we will deallocate the `GcBox`, but skip dropping the `dyn Collect` value
    /// (since we've already done it).
    #[inline]
    pub(crate) fn is_live(&self) -> bool {
        self.0.get() & 0x10 != 0x0
    }

    #[inline]
    pub(crate) fn set_needs_trace(&self, needs_trace: bool) {
        self.0
            .set((self.0.get() & !0x4) | if needs_trace { 0x4 } else { 0x0 });
    }

    #[inline]
    pub(crate) fn set_traced_weak_ref(&self, traced_weak_ref: bool) {
        self.0
            .set((self.0.get() & !0x8) | if traced_weak_ref { 0x8 } else { 0x0 });
    }

    #[inline]
    pub(crate) fn set_live(&self, alive: bool) {
        self.0
            .set((self.0.get() & !0x10) | if alive { 0x10 } else { 0x0 });
    }
}

// Phantom type that holds a lifetime and ensures that it is invariant.
pub(crate) type Invariant<'a> = PhantomData<Cell<&'a ()>>;
