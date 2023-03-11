use core::alloc::Layout;
use core::cell::Cell;
use core::marker::PhantomData;
use core::ptr::NonNull;
use core::{mem, ptr};

use crate::collect::Collect;

/// A thin-pointer-sized box containing a type-erased GC object.
/// Stores the metadata required by the GC algorithm inline (see `GcBoxInner`
/// for its typed counterpart).

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct GcBox(NonNull<GcBoxInner<()>>);

impl GcBox {
    /// Erases a pointer to a typed GC object.
    ///
    /// **SAFETY:** The pointer must point to a valid `GcBoxInner` allocated
    /// in a `Box`.
    #[inline(always)]
    pub(crate) unsafe fn erase<T: ?Sized>(ptr: NonNull<GcBoxInner<T>>) -> Self {
        // This cast is sound because `GcBoxInner` is `repr(C)`.
        let erased = ptr.as_ptr() as *mut GcBoxInner<()>;
        Self(NonNull::new_unchecked(erased))
    }

    /// Gets a pointer to the value stored inside this box.
    /// `T` must be the same type that was used with `erase`, so that
    /// we can correctly compute the field offset.
    #[inline(always)]
    fn unerased_value<T>(&self) -> *mut T {
        unsafe {
            let ptr = self.0.as_ptr() as *mut GcBoxInner<T>;
            // Don't create a reference, to keep the full provenance.
            // Also, this gives us interior mutability "for free".
            ptr::addr_of_mut!((*ptr).value) as *mut T
        }
    }

    #[inline(always)]
    pub(crate) fn header(&self) -> &GcBoxHeader {
        unsafe { &self.0.as_ref().header }
    }

    /// Traces the stored value.
    ///
    /// **SAFETY**: `Self::drop_in_place` must not have been called.
    #[inline(always)]
    pub(crate) unsafe fn trace_value(&self, cc: crate::CollectionContext) {
        (self.header().vtable().trace_value)(*self, cc)
    }

    /// Drops the stored value.
    ///
    /// **SAFETY**: once called, no GC pointers should access the stored value
    /// (but accessing the `GcBox` itself is still safe).
    #[inline(always)]
    pub(crate) unsafe fn drop_in_place(&mut self) {
        (self.header().vtable().drop_value)(*self)
    }

    /// Deallocates the box. Failing to call `Self::drop_in_place` beforehand
    /// will cause the stored value to be leaked.
    ///
    /// **SAFETY**: once called, this `GcBox` should never be accessed by any GC
    /// pointers again.
    #[inline(always)]
    pub(crate) unsafe fn dealloc(self) {
        let layout = self.header().vtable().box_layout;
        let ptr = self.0.as_ptr() as *mut u8;
        // SAFETY: the pointer was `Box`-allocated with this layout.
        alloc::alloc::dealloc(ptr, layout);
    }
}

pub(crate) struct GcBoxHeader {
    /// The next element in the global linked list of allocated objects.
    next: Cell<Option<GcBox>>,
    /// A custom virtual function table for handling type-specific operations.
    ///
    /// The lower bits of the pointer are used to store GC flags:
    /// - bits 0 & 1 for the current `GcColor`;
    /// - bit 2 for the `needs_trace` flag;
    /// - bit 3 for the `is_live` flag.
    tagged_vtable: Cell<*const CollectVtable>,
}

impl GcBoxHeader {
    #[inline(always)]
    pub fn new<T: Collect>() -> Self {
        // Helper trait to materialize vtables in static memory.
        trait HasCollectVtable {
            const VTABLE: CollectVtable;
        }

        impl<T: Collect> HasCollectVtable for T {
            const VTABLE: CollectVtable = CollectVtable::vtable_for::<T>();
        }

        let vtable: &'static _ = &<T as HasCollectVtable>::VTABLE;
        Self {
            next: Cell::new(None),
            tagged_vtable: Cell::new(vtable as *const _),
        }
    }

    /// Gets a reference to the `CollectVtable` used by this box.
    #[inline(always)]
    fn vtable(&self) -> &'static CollectVtable {
        let ptr = tagged_ptr::untag(self.tagged_vtable.get());
        // SAFETY:
        // - the pointer was properly untagged.
        // - the vtable is stored in static memory.
        unsafe { &*ptr }
    }

    /// Gets the next element in the global linked list of allocated objects.
    #[inline(always)]
    pub(crate) fn next(&self) -> Option<GcBox> {
        self.next.get()
    }

    /// Sets the next element in the global linked list of allocated objects.
    #[inline(always)]
    pub(crate) fn set_next(&self, next: Option<GcBox>) {
        self.next.set(next)
    }

    /// Returns the (shallow) size occupied by this box in memory.
    #[inline(always)]
    pub(crate) fn size_of_box(&self) -> usize {
        self.vtable().box_layout.size()
    }

    #[inline]
    pub(crate) fn color(&self) -> GcColor {
        match tagged_ptr::get(self.tagged_vtable.get(), 0x3) {
            0x0 => GcColor::White,
            0x1 => GcColor::WhiteWeak,
            0x2 => GcColor::Gray,
            _ => GcColor::Black,
        }
    }

    #[inline]
    pub(crate) fn set_color(&self, color: GcColor) {
        tagged_ptr::set(
            &self.tagged_vtable,
            0x3,
            match color {
                GcColor::White => 0x0,
                GcColor::WhiteWeak => 0x1,
                GcColor::Gray => 0x2,
                GcColor::Black => 0x3,
            },
        );
    }
    #[inline]
    pub(crate) fn needs_trace(&self) -> bool {
        tagged_ptr::get(self.tagged_vtable.get(), 0x4) != 0x0
    }

    /// Determines whether or not we've dropped the `dyn Collect` value
    /// stored in `GcBox.value`
    /// When we garbage-collect a `GcBox` that still has outstanding weak pointers,
    /// we set `alive` to false. When there are no more weak pointers remaining,
    /// we will deallocate the `GcBox`, but skip dropping the `dyn Collect` value
    /// (since we've already done it).
    #[inline]
    pub(crate) fn is_live(&self) -> bool {
        tagged_ptr::get(self.tagged_vtable.get(), 0x8) != 0x0
    }

    #[inline]
    pub(crate) fn set_needs_trace(&self, needs_trace: bool) {
        tagged_ptr::set_bool(&self.tagged_vtable, 0x4, needs_trace);
    }

    #[inline]
    pub(crate) fn set_live(&self, alive: bool) {
        tagged_ptr::set_bool(&self.tagged_vtable, 0x8, alive);
    }
}

/// Type-specific operations for GC'd values.
///
/// We use a custom vtable instead of `dyn Collect` for extra flexibility.
/// The type is over-aligned so that `GcBoxHeader` can store flags into the LSBs of the vtable pointer.
#[repr(align(16))]
struct CollectVtable {
    /// The layout of the `GcBox` the GC'd value is stored in.
    box_layout: Layout,
    /// Drops the value stored in the given `GcBox` (without deallocating the box).
    drop_value: unsafe fn(GcBox),
    /// Traces the value stored in the given `GcBox`.
    trace_value: unsafe fn(GcBox, crate::CollectionContext<'_>),
}

impl CollectVtable {
    /// Makes a vtable for a known, `Sized` type.
    /// Because `T: Sized`, we can recover a typed pointer
    /// directly from the erased `GcBox`.
    #[inline(always)]
    const fn vtable_for<T: Collect>() -> Self {
        Self {
            box_layout: Layout::new::<GcBoxInner<T>>(),
            drop_value: |erased| unsafe {
                ptr::drop_in_place(erased.unerased_value::<T>());
            },
            trace_value: |erased, cc| unsafe {
                let val = &*(erased.unerased_value::<T>());
                val.trace(cc)
            },
        }
    }
}

/// A typed GC'd value, together with its metadata.
/// This type is never manipulated directly by the GC algorithm, allowing
/// user-facing `Gc`s to freely cast their pointer to it.
pub(crate) struct GcBoxInner<T: ?Sized> {
    header: GcBoxHeader,
    /// The typed value stored in this `GcBox`.
    pub(crate) value: mem::ManuallyDrop<T>,
}

impl<T: ?Sized> GcBoxInner<T> {
    #[inline(always)]
    pub(crate) fn new(header: GcBoxHeader, t: T) -> Self
    where
        T: Collect + Sized,
    {
        Self {
            header,
            value: mem::ManuallyDrop::new(t),
        }
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
    /// Like White, but for objects weakly reachable from a Black object.
    ///
    /// These objects may drop their contents during `Phase::Sweep`, but must
    /// stay allocated so that weak references can check the alive status.
    WhiteWeak,
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

// Phantom type that holds a lifetime and ensures that it is invariant.
pub(crate) type Invariant<'a> = PhantomData<Cell<&'a ()>>;

/// Utility functions for tagging and untagging pointers.
mod tagged_ptr {
    #![cfg_attr(not(miri), allow(unstable_name_collisions))]

    #[cfg(not(miri))]
    use sptr::Strict as _;

    use core::cell::Cell;

    #[inline(always)]
    pub(super) fn untag<T>(tagged_ptr: *const T) -> *const T {
        let mask = core::mem::align_of::<T>() - 1;
        tagged_ptr.map_addr(|addr| addr & !mask)
    }

    #[inline(always)]
    pub(super) fn get<T>(tagged_ptr: *const T, mask: usize) -> usize {
        tagged_ptr.addr() & mask
    }

    #[inline(always)]
    pub(super) fn set<T>(pcell: &Cell<*const T>, mask: usize, tag: usize) {
        let ptr = pcell.get();
        let ptr = ptr.map_addr(|addr| (addr & !mask) | tag);
        pcell.set(ptr)
    }

    #[inline(always)]
    pub(super) fn set_bool<T>(pcell: &Cell<*const T>, mask: usize, value: bool) {
        let ptr = pcell.get();
        let ptr = ptr.map_addr(|addr| (addr & !mask) | if value { mask } else { 0 });
        pcell.set(ptr)
    }
}
