use alloc::alloc;
use core::alloc::Layout;
use core::cell::Cell;
use core::marker::PhantomData;
use core::ptr::NonNull;
use core::{fmt, mem, ptr};

use crate::{collect::Collect, context::Context};

/// A thin-pointer-sized pointer to a type-erased GC object.
///
/// Pointers to GC objects have the metadata required by the GC algorithm in the same allocation
/// *before* the stored pointer.
pub(crate) struct GcPtr<T: ?Sized = ()>(NonNull<T>);

impl<T: ?Sized> fmt::Debug for GcPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Pointer::fmt(&self.as_ptr(), f)
    }
}

impl<T: ?Sized> Copy for GcPtr<T> {}

impl<T: ?Sized> Clone for GcPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'gc, T: Collect<'gc>> GcPtr<T> {
    /// Allocate a new GC value with a default header.
    ///
    /// # Panics
    ///
    /// Panics if there is no valid layout we can allocate.
    #[inline(always)]
    pub(crate) fn alloc(value: T) -> Self {
        unsafe {
            let gc_layout = GcLayout::new(Layout::new::<T>()).unwrap();
            let gc_header = GcHeader::new::<T>();

            let bytes = alloc::alloc(gc_layout.alloc_layout);
            let Some(bytes) = NonNull::new(bytes) else {
                alloc::handle_alloc_error(gc_layout.alloc_layout);
            };

            let value_ptr = gc_layout.value_ptr::<T>(bytes);
            let header_ptr = GcLayout::header_ptr(value_ptr);
            debug_assert!(header_ptr.is_aligned() && value_ptr.is_aligned());

            ptr::write(header_ptr.as_ptr(), gc_header);
            ptr::write(value_ptr.as_ptr(), value);

            GcPtr(value_ptr)
        }
    }
}

impl<T: ?Sized> GcPtr<T> {
    #[inline(always)]
    pub(crate) fn header(&self) -> &GcHeader {
        unsafe { GcLayout::header_ptr(self.0).as_ref() }
    }

    #[inline(always)]
    pub(crate) fn as_ptr(self) -> *const T {
        self.0.as_ptr()
    }

    /// # Safety
    ///
    /// The provided ptr must have come from `GcPtr::as_ptr`.
    #[inline(always)]
    pub(crate) unsafe fn from_ptr(p: *const T) -> Self {
        Self(unsafe { NonNull::new_unchecked(p as *mut T) })
    }

    #[inline(always)]
    pub(crate) fn cast<U>(self) -> GcPtr<U> {
        GcPtr(self.0.cast())
    }

    /// A convenience method to cast to `()`.
    #[inline(always)]
    pub(crate) fn erase(self) -> GcPtr {
        self.cast()
    }

    /// Returns true if two `GcPtr`s point to the same allocation.
    ///
    /// This function ignores the metadata of `dyn` pointers.
    #[inline(always)]
    pub(crate) fn addr_eq(self, other: GcPtr<T>) -> bool {
        ptr::addr_eq(self.as_ptr(), other.as_ptr())
    }

    /// Return a shared reference to the value.
    ///
    /// # Safety
    ///
    /// You must ensure that this pointer was not cast to a type incompatible with its allocated
    /// type and has not been dropped or deallocated.
    ///
    /// Additionally, the returned reference has an unbound lifetime so the returned reference must
    /// not be live when the value is dropped or the pointer deallocated.
    #[inline(always)]
    pub(crate) unsafe fn as_ref<'a>(self) -> &'a T {
        unsafe { self.0.as_ref() }
    }

    /// Traces the stored value.
    ///
    /// # Safety
    ///
    /// The value must not have been dropped and the pointer must not have been deallocated.
    #[inline(always)]
    pub(crate) unsafe fn trace_value(self, cc: &mut Context) {
        unsafe { (self.header().vtable().trace_value)(self.0.cast::<()>().as_ptr(), cc) }
    }

    /// Drops the stored value.
    ///
    /// # Safety:
    ///
    /// The value must not have been previously dropped and the pointer must not have been
    /// deallocated.
    #[inline(always)]
    pub(crate) unsafe fn drop_in_place(&mut self) {
        unsafe { (self.header().vtable().drop_value)(self.0.cast::<()>().as_ptr()) }
    }

    /// Deallocates the allocation this pointer points to.
    ///
    /// Failing to call `Self::drop_in_place` beforehand will cause the stored value to be leaked.
    ///
    /// # Safety:
    ///
    /// The pointer must not already be deallocated.
    #[inline(always)]
    pub(crate) unsafe fn dealloc(self) {
        let gc_layout = GcLayout::new(self.header().vtable().value_layout).unwrap();
        unsafe {
            let ptr = gc_layout.alloc_ptr(self.0).as_ptr();
            // SAFETY: the pointer was allocated with this layout.
            alloc::dealloc(ptr, gc_layout.alloc_layout);
        }
    }
}

pub(crate) struct GcHeader {
    /// The next element in the global linked list of allocated objects.
    next: Cell<Option<GcPtr>>,
    /// A custom virtual function table for handling type-specific operations.
    ///
    /// The lower bits of the pointer are used to store GC flags:
    /// - bits 0 & 1 for the current `GcColor`;
    /// - bit 2 for the `needs_trace` flag;
    /// - bit 3 for the `is_live` flag.
    tagged_vtable: Cell<*const CollectVtable>,
}

impl GcHeader {
    /// Create a new `GcHeader` with:
    /// 1) color set to `White`
    /// 2) `needs_trace` set to `false`
    /// 3) `is_live` set to `false`
    #[inline(always)]
    fn new<'gc, T: Collect<'gc>>() -> Self {
        // Helper trait to materialize vtables in static memory.
        trait HasCollectVtable {
            const VTABLE: CollectVtable;
        }

        impl<'gc, T: Collect<'gc>> HasCollectVtable for T {
            const VTABLE: CollectVtable = CollectVtable::vtable_for::<T>();
        }

        let vtable: &'static _ = &<T as HasCollectVtable>::VTABLE;

        Self {
            next: Cell::new(None),
            tagged_vtable: Cell::new(vtable as *const _),
        }
    }

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
    pub(crate) fn next(&self) -> Option<GcPtr> {
        self.next.get()
    }

    /// Sets the next element in the global linked list of allocated objects.
    #[inline(always)]
    pub(crate) fn set_next(&self, next: Option<GcPtr>) {
        self.next.set(next)
    }

    #[inline]
    pub(crate) fn color(&self) -> GcColor {
        match tagged_ptr::get::<0x3, _>(self.tagged_vtable.get()) {
            0x0 => GcColor::White,
            0x1 => GcColor::WhiteWeak,
            0x2 => GcColor::Gray,
            _ => GcColor::Black,
        }
    }

    #[inline]
    pub(crate) fn set_color(&self, color: GcColor) {
        self.tagged_vtable.update(|p| {
            tagged_ptr::set::<0x3, _>(
                p,
                match color {
                    GcColor::White => 0x0,
                    GcColor::WhiteWeak => 0x1,
                    GcColor::Gray => 0x2,
                    GcColor::Black => 0x3,
                },
            )
        });
    }

    #[inline]
    pub(crate) fn needs_trace(&self) -> bool {
        tagged_ptr::get_bool::<0x4, _>(self.tagged_vtable.get())
    }

    #[inline]
    pub(crate) fn set_needs_trace(&self, needs_trace: bool) {
        self.tagged_vtable
            .update(|p| tagged_ptr::set_bool::<0x4, _>(p, needs_trace));
    }

    /// Determines whether or not we've dropped the `dyn Collect` value stored in `GcPtr.value`
    ///
    /// When we garbage-collect a `GcPtr` that still has outstanding weak pointers, we set `alive`
    /// to false. When there are no more weak pointers remaining, we will deallocate the `GcPtr`,
    /// but skip dropping the `dyn Collect` value (since we've already done it).
    #[inline]
    pub(crate) fn is_live(&self) -> bool {
        tagged_ptr::get_bool::<0x8, _>(self.tagged_vtable.get())
    }

    #[inline]
    pub(crate) fn set_live(&self, alive: bool) {
        self.tagged_vtable
            .update(|p| tagged_ptr::set_bool::<0x8, _>(p, alive));
    }
}

/// Type-specific operations for GC'd values.
///
/// We use a custom vtable instead of `dyn Collect` for extra flexibility. The type is over-aligned
/// so that `GcHeader` can store flags into the LSBs of the vtable pointer.
#[repr(align(16))]
struct CollectVtable {
    /// Traces the value at the given pointer.
    trace_value: unsafe fn(*const (), &mut Context),
    /// Drops the value at the given pointer.
    drop_value: unsafe fn(*mut ()),
    /// The layout of the value stored in this `GcPtr`.
    value_layout: Layout,
}

impl CollectVtable {
    /// Makes a vtable for a known, `Sized` type.
    /// Because `T: Sized`, we can recover a typed pointer
    /// directly from the erased `GcPtr`.
    #[inline(always)]
    const fn vtable_for<'gc, T: Collect<'gc>>() -> Self {
        Self {
            trace_value: |ptr, cc| unsafe {
                ptr.cast::<T>().as_ref_unchecked().trace(cc);
            },
            drop_value: |ptr| unsafe {
                ptr::drop_in_place(ptr.cast::<T>());
            },
            value_layout: Layout::new::<T>(),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub(crate) enum GcColor {
    /// An object that has not yet been reached by tracing (if we're in a tracing phase).
    ///
    /// During `Phase::Sweep`, we will free all white objects that existed *before* the start of the
    /// current `Phase::Sweep`. Objects allocated during `Phase::Sweep` will be white, but will not
    /// be freed.
    White,
    /// Like White, but for objects weakly reachable from a Black object.
    ///
    /// These objects may drop their contents during `Phase::Sweep`, but must stay allocated so that
    /// weak references can check the alive status.
    WhiteWeak,
    /// An object reachable from a Black object, but that has not yet been traced using
    /// `Collect::trace`. We also mark black objects as gray during `Phase::Mark` in response to
    /// a write barrier, so that we re-trace and find any objects newly reachable from the mutated
    /// object.
    Gray,
    /// An object that was reached during tracing. It will not be freed during `Phase::Sweep`. At
    /// the end of `Phase::Sweep`, all black objects will be reset to white.
    Black,
}

// Phantom type that holds a lifetime and ensures that it is invariant.
pub(crate) type Invariant<'a> = PhantomData<Cell<&'a ()>>;

/// The layout of an allocated buffer backing a `Gc` pointer.
#[derive(Debug, Copy, Clone)]
struct GcLayout {
    alloc_layout: Layout,
    value_offset: usize,
}

impl GcLayout {
    #[inline(always)]
    const fn new(value_layout: Layout) -> Option<Self> {
        #[inline(always)]
        const fn max(a: usize, b: usize) -> usize {
            if a > b { a } else { b }
        }

        let header_size = mem::size_of::<GcHeader>();
        let header_align = mem::align_of::<GcHeader>();

        let value_size = value_layout.size();
        let value_align = value_layout.align();

        // We want to allocate a buffer that is large enough to hold both the value and the header
        // at the proper alignment.
        //
        // We operate on pointers to the value itself, not the entire allocated buffer, so we need
        // to be careful how we offset objects in this buffer so that the location of the header is
        // predictable. We want the location of the header to be at a fixed position relative to the
        // value pointer that does *not* depend on the pointer type, so that we can freely cast the
        // value pointer.

        // The allocation must be aligned to both header and value alignment so that when we offset
        // from this pointer, if the offset is properly aligned then we know the resulting pointer
        // is still aligned.
        let alloc_align = max(header_align, value_align);

        // We need to make sure the value offset is enough to store the header before it while also
        // making sure that the value is aligned to *both* the header and value alignment. This way,
        // when we subtract the header size from the value pointer, we know the resulting location
        // is properly aligned for the header.
        let value_offset = max(header_size, value_align);

        let Some(alloc_size) = value_offset.checked_add(value_size) else {
            return None;
        };

        let Ok(alloc_layout) = Layout::from_size_align(alloc_size, alloc_align) else {
            return None;
        };

        // Ensure that the value and header offsets are both properly aligned.
        debug_assert!(value_offset.is_multiple_of(value_align));
        debug_assert!((value_offset - header_size).is_multiple_of(header_align));

        Some(Self {
            alloc_layout,
            value_offset,
        })
    }

    /// Compute the pointer to the GC value from the pointer to the GC allocation.
    ///
    /// # Safety
    ///
    /// Allocation must have been allocated according to `alloc_layout`.
    #[inline(always)]
    unsafe fn value_ptr<T>(&self, alloc_ptr: NonNull<u8>) -> NonNull<T> {
        let value = unsafe { alloc_ptr.add(self.value_offset).cast() };
        debug_assert!(value.is_aligned());
        value
    }

    /// Compute the pointer to the GC header from a GC value pointer.
    ///
    /// # Safety
    ///
    /// Must be a valid GC value pointer.
    #[inline(always)]
    unsafe fn header_ptr<T: ?Sized>(value_ptr: NonNull<T>) -> NonNull<GcHeader> {
        let header = unsafe {
            value_ptr
                .cast::<GcHeader>()
                .byte_sub(mem::size_of::<GcHeader>())
        };
        debug_assert!(header.is_aligned());
        header
    }

    /// Compute the pointer to the beginning of the GC allocation from a GC value pointer.
    ///
    /// # Safety
    ///
    /// Must be a valid GC value pointer.
    #[inline(always)]
    unsafe fn alloc_ptr<T: ?Sized>(&self, value_ptr: NonNull<T>) -> NonNull<u8> {
        unsafe { value_ptr.cast::<u8>().byte_sub(self.value_offset) }
    }
}

/// Utility functions for tagging and untagging pointers.
mod tagged_ptr {
    /// Checks that `mask` can be used to tag a pointer to `T`.
    const fn is_valid_mask<T>(mask: usize) -> bool {
        mask < core::mem::align_of::<T>()
    }

    /// Checks that `mask` is exactly 1 bit wide.
    const fn is_boolean_mask(mask: usize) -> bool {
        mask.is_power_of_two()
    }

    trait ValidMask<const MASK: usize> {
        const CHECK: ();
    }

    impl<T, const MASK: usize> ValidMask<MASK> for T {
        const CHECK: () = assert!(is_valid_mask::<T>(MASK));
    }

    trait BooleanMask<const MASK: usize> {
        const CHECK: ();
    }

    impl<T, const MASK: usize> BooleanMask<MASK> for T {
        const CHECK: () = assert!(is_boolean_mask(MASK));
    }

    /// Checks that `$mask` can be used to tag a pointer to `$type`.
    ///
    /// If this isn't true, this macro will cause a post-monomorphization error.
    macro_rules! check_mask {
        ($type:ty, $mask:expr) => {
            let _ = <$type as ValidMask<$mask>>::CHECK;
        };
    }

    macro_rules! check_bool_mask {
        ($type:ty, $mask:expr) => {
            let _ = <$type as ValidMask<$mask>>::CHECK;
            let _ = <$type as BooleanMask<$mask>>::CHECK;
        };
    }

    #[inline(always)]
    pub(super) fn untag<T>(tagged_ptr: *const T) -> *const T {
        let mask = core::mem::align_of::<T>() - 1;
        tagged_ptr.map_addr(|addr| addr & !mask)
    }

    #[inline(always)]
    pub(super) fn get<const MASK: usize, T>(tagged_ptr: *const T) -> usize {
        check_mask!(T, MASK);
        tagged_ptr.addr() & MASK
    }

    pub(super) fn set<const MASK: usize, T>(tagged_ptr: *const T, tag: usize) -> *const T {
        check_mask!(T, MASK);
        tagged_ptr.map_addr(|addr| (addr & !MASK) | (tag & MASK))
    }

    #[inline(always)]
    pub(super) fn get_bool<const MASK: usize, T>(tagged_ptr: *const T) -> bool {
        check_bool_mask!(T, MASK);
        tagged_ptr.addr() & MASK != 0x0
    }

    #[inline(always)]
    pub(super) fn set_bool<const MASK: usize, T>(tagged_ptr: *const T, value: bool) -> *const T {
        check_bool_mask!(T, MASK);
        tagged_ptr.map_addr(|addr| (addr & !MASK) | if value { MASK } else { 0 })
    }
}
