use alloc::alloc::{alloc, dealloc, handle_alloc_error};
use contracts::ensures;
use core::{
    alloc::Layout,
    cell::Cell,
    fmt::{self, Debug, Formatter},
    marker::PhantomData,
    mem::MaybeUninit,
    num::NonZero,
    ptr::{self, NonNull},
};

use crate::{collect::Collect, context::Context};

/// A type storing layout information about a box containing a GC'd object.
#[derive(Clone, Copy, Debug, PartialEq)]
struct GcBoxLayout {
    /// The overall layout of the box.
    box_layout: Layout,

    /// The offset from the start of the allocation to the layout, if this is a dynamic layout. If
    /// this is a fixed layout, always 0.
    offset_of_box_layout: usize,

    /// The offset from the start of the allocation to the GC header.
    offset_of_header: usize,

    /// The offset from the start of the allocation to the boxed value.
    offset_of_value: usize,
}

impl GcBoxLayout {
    /// Returns the `GcBoxLayout` to use for an allocation of the given type with a fixed layout.
    #[inline(always)]
    #[ensures(ret.box_layout.size() != 0)]
    #[ensures(ret.offset_of_box_layout == 0)]
    #[ensures((ret.offset_of_header % Layout::new::<GcBoxHeader>().align()) == 0)]
    #[ensures((ret.offset_of_value % Layout::new::<T>().align()) == 0)]
    pub const fn for_fixed<T>() -> GcBoxLayout {
        let header_layout = Layout::new::<GcBoxHeader>();
        let value_layout = Layout::new::<T>();

        let (box_layout, offset_of_value) = layout_extend_or_die(header_layout, value_layout);

        assert!(header_layout.size() <= offset_of_value);
        let offset_of_header = offset_of_value - header_layout.size();

        GcBoxLayout {
            box_layout,
            offset_of_box_layout: 0,
            offset_of_header,
            offset_of_value,
        }
    }

    /// Returns the `GcBoxLayout` to use for an allocation of the given type with a dynamic layout.
    #[inline(always)]
    #[ensures(ret.box_layout.size() != 0)]
    #[ensures((ret.offset_of_box_layout % Layout::new::<Layout>().align()) == 0)]
    #[ensures((ret.offset_of_header % Layout::new::<GcBoxHeader>().align()) == 0)]
    #[ensures((ret.offset_of_value % value_layout.align()) == 0)]
    pub const fn for_dynamic(value_layout: Layout) -> GcBoxLayout {
        let box_layout_layout = Layout::new::<Layout>();
        let header_layout = Layout::new::<GcBoxHeader>();

        let (box_layout, _) = layout_extend_or_die(box_layout_layout, header_layout);
        let (box_layout, offset_of_value) = layout_extend_or_die(box_layout, value_layout);

        assert!(header_layout.size() <= offset_of_value);
        let offset_of_header = offset_of_value - header_layout.size();
        assert!(box_layout_layout.size() <= offset_of_header);
        let offset_of_box_layout = offset_of_header - box_layout_layout.size();

        GcBoxLayout {
            box_layout,
            offset_of_box_layout,
            offset_of_header,
            offset_of_value,
        }
    }
}

/// A wrapper for `Layout::extend(...).unwrap()` to work around `.unwrap()` not being const.
const fn layout_extend_or_die(l1: Layout, l2: Layout) -> (Layout, usize) {
    match l1.extend(l2) {
        Ok(out) => out,
        Err(_) => panic!("GcBox would be impossibly large"),
    }
}

/// A box containing a GC'd object.
///
/// The metadata is stored before the value in memory, possibly including a layout if this object
/// was allocated with a dynamic layout.
///
/// Note that this means there may be padding at the _start_ of the allocation, if the object's
/// alignment is greater than the header and layout's alignment.
pub(crate) struct GcBox<T: ?Sized>(NonNull<T>);

impl<'gc, T: Collect<'gc>> GcBox<T> {
    /// Allocates a `GcBox` with a fixed layout.
    pub fn alloc_fixed(next: Option<ErasedGcBox>) -> GcBox<MaybeUninit<T>> {
        let header = GcBoxHeader::new_fixed::<T>(next);
        let layout = GcBoxLayout::for_fixed::<T>();

        // SAFETY: The post-condition on GcBoxLayout::for_fixed guarantees that the size is
        // non-zero.
        let ptr = unsafe { alloc(layout.box_layout) };
        let ptr = NonNull::new(ptr).unwrap_or_else(|| {
            handle_alloc_error(layout.box_layout);
        });

        // SAFETY: If this overflows, the allocation does too.
        let value_ptr = unsafe { ptr.byte_add(layout.offset_of_value) };

        // SAFETY: Uninitialized memory is a valid MaybeUninit, and we initialize the header before
        // returning this.
        let gc_box = unsafe { GcBox::from_ptr(value_ptr.cast::<MaybeUninit<T>>()) };

        // SAFETY: TODO
        unsafe {
            gc_box.erase().header_ptr().write(header);
        }

        assert_eq!(layout.box_layout, gc_box.erase().layout());
        gc_box
    }

    /// Allocates a `GcBox` with the given layout for its value.
    pub fn alloc_dynamic(next: Option<ErasedGcBox>, value_layout: Layout) -> GcBox<MaybeUninit<T>> {
        let header = GcBoxHeader::new_dynamic::<T>(next);
        let layout = GcBoxLayout::for_dynamic(value_layout);

        // SAFETY: The post-condition on GcBoxLayout::for_fixed guarantees that the size is
        // non-zero.
        let ptr = unsafe { alloc(layout.box_layout) };
        let ptr = NonNull::new(ptr).unwrap_or_else(|| {
            handle_alloc_error(layout.box_layout);
        });

        // SAFETY: This has to be in-bounds.
        unsafe {
            ptr.byte_add(layout.offset_of_box_layout)
                .cast()
                .write(layout.box_layout);
            ptr.byte_add(layout.offset_of_header).cast().write(header);
        };

        // SAFETY: If this overflows, the allocation does too.
        let value_ptr = unsafe { ptr.byte_add(layout.offset_of_value) };

        // SAFETY: Uninitialized memory is a valid MaybeUninit, and we initialize the header before
        // returning this.
        let gc_box = unsafe { GcBox::from_ptr(value_ptr.cast::<MaybeUninit<T>>()) };

        assert_eq!(layout.box_layout, gc_box.erase().layout());
        gc_box
    }
}

impl<T: ?Sized> GcBox<T> {
    /// TODO: Docs
    #[inline(always)]
    pub unsafe fn from_ptr(ptr: NonNull<T>) -> GcBox<T> {
        GcBox(ptr)
    }

    /// TODO
    #[inline(always)]
    pub fn cast<U>(self) -> GcBox<U> {
        GcBox(self.0.cast())
    }

    /// Gets a mutable reference to the value stored inside this box.
    ///
    /// TODO: docs
    #[inline(always)]
    pub unsafe fn get_mut(&mut self) -> &mut T {
        unsafe { self.0.as_mut() }
    }

    /// Gets a pointer to the value stored inside this box.
    #[inline(always)]
    pub fn get_ptr(self) -> NonNull<T> {
        self.0
    }

    /// Type-erases the box.
    #[inline(always)]
    pub fn erase(self) -> ErasedGcBox {
        ErasedGcBox(self.0.cast())
    }

    /// Returns a reference to the GC header.
    #[inline(always)]
    pub fn header(&self) -> &GcBoxHeader {
        unsafe { self.erase().header_ptr().as_ref() }
    }
}

impl<T: ?Sized> Copy for GcBox<T> {}

impl<T: ?Sized> Clone for GcBox<T> {
    fn clone(&self) -> GcBox<T> {
        GcBox(self.0)
    }
}

impl<T: ?Sized> Debug for GcBox<T> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.debug_tuple("GcBox").field(&self.0).finish()
    }
}

impl<T: ?Sized> Eq for GcBox<T> {}

impl<T: ?Sized> PartialEq for GcBox<T> {
    fn eq(&self, other: &GcBox<T>) -> bool {
        ptr::eq(self.0.as_ptr(), other.0.as_ptr())
    }
}

/// A type-erased version of `GcBox`.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct ErasedGcBox(NonNull<()>);

impl ErasedGcBox {
    /// "Unerases" the type.
    ///
    /// **SAFETY**: The type `T` must be the same type as in the `GcBox<T>` that was erased.
    pub(crate) unsafe fn unerase<T>(self) -> GcBox<T> {
        GcBox(self.0.cast())
    }

    /// Returns a pointer to the start of the box.
    fn box_ptr(self) -> NonNull<()> {
        // SAFETY: TODO
        let unaligned_ptr = if self.header().dynamic_layout() {
            unsafe {
                self.0
                    .cast::<GcBoxHeader>()
                    .sub(1)
                    .cast::<Layout>()
                    .sub(1)
                    .cast::<()>()
            }
        } else {
            unsafe { self.0.cast::<GcBoxHeader>().sub(1).cast::<()>() }
        };

        let layout = self.layout();
        unaligned_ptr.map_addr(|addr| NonZero::new(addr.get() & !(layout.align() - 1)).unwrap())
    }

    /// Deallocates the box. Failing to call `Self::drop_in_place` beforehand
    /// will cause the stored value to be leaked.
    ///
    /// **SAFETY**: once called, this `ErasedGcBox` should never be accessed by any GC
    /// pointers again.
    #[inline(always)]
    pub(crate) unsafe fn dealloc(self) {
        let ptr = self.box_ptr();
        let layout = self.layout();

        // SAFETY: the pointer was `Box`-allocated with this layout.
        unsafe { dealloc(ptr.as_ptr().cast(), layout) }
    }

    /// Returns a reference to the GC header.
    #[inline(always)]
    pub fn header(&self) -> &GcBoxHeader {
        unsafe { self.header_ptr().as_ref() }
    }

    /// Returns a pointer to the GC header.
    #[inline(always)]
    fn header_ptr(&self) -> NonNull<GcBoxHeader> {
        unsafe { self.0.cast::<GcBoxHeader>().sub(1) }
    }

    #[inline]
    pub fn layout(&self) -> Layout {
        if self.header().dynamic_layout() {
            // SAFETY: The pointer was allocated by the GC, so we know that this all stays
            // in-bounds and the layout is at the appropriate location.
            unsafe {
                let value_ptr = self.0;
                let header_ptr = value_ptr.cast::<GcBoxHeader>().sub(1);
                let layout_ptr = header_ptr.cast::<Layout>().sub(1);
                layout_ptr.read()
            }
        } else {
            self.header().vtable().box_layout
        }
    }

    /// Traces the stored value.
    ///
    /// **SAFETY**: `Self::drop_in_place` must not have been called.
    #[inline(always)]
    pub(crate) unsafe fn trace_value(&self, cc: &mut Context) {
        unsafe { (self.header().vtable().trace_value)(*self, cc) }
    }

    /// Drops the stored value.
    ///
    /// **SAFETY**: once called, no GC pointers should access the stored value
    /// (but accessing the `GcBox` itself is still safe).
    #[inline(always)]
    pub(crate) unsafe fn drop_in_place(&mut self) {
        unsafe { (self.header().vtable().drop_value)(*self) }
    }
}

pub(crate) struct GcBoxHeader {
    /// The next element in the global linked list of allocated objects.
    next: Cell<Option<ErasedGcBox>>,

    /// A custom virtual function table for handling type-specific operations.
    ///
    /// The lower bits of the pointer are used to store GC flags:
    /// - bits 0 & 1 for the current `GcColor`;
    /// - bit 2 for the `needs_trace` flag;
    /// - bit 3 for the `is_live` flag;
    /// - bit 4 for the `dynamic_layout` flag.
    tagged_vtable: Cell<*const CollectVtable>,
}

impl GcBoxHeader {
    /// Creates a header for an allocation of the given type with a fixed layout.
    #[inline(always)]
    pub fn new_fixed<'gc, T: Collect<'gc>>(next: Option<ErasedGcBox>) -> GcBoxHeader {
        let vtable: *const _ = CollectVtable::new::<T>();
        let header = GcBoxHeader {
            next: Cell::new(next),
            tagged_vtable: Cell::new(vtable),
        };
        header.set_live(true);
        header.set_needs_trace(T::NEEDS_TRACE);
        header
    }

    /// Creates a header for an allocation of the given type with a dynamic layout.
    ///
    /// This still needs a generic `T` bound to get the function pointers in the vtable.
    #[inline(always)]
    pub fn new_dynamic<'gc, T: Collect<'gc>>(next: Option<ErasedGcBox>) -> GcBoxHeader {
        let vtable: *const _ = CollectVtable::new::<T>();
        let header = GcBoxHeader {
            next: Cell::new(next),
            tagged_vtable: Cell::new(vtable),
        };
        header.set_live(true);
        header.set_needs_trace(T::NEEDS_TRACE);
        header.set_dynamic_layout(true);
        header
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
    pub(crate) fn next(&self) -> Option<ErasedGcBox> {
        self.next.get()
    }

    /// Sets the next element in the global linked list of allocated objects.
    #[inline(always)]
    pub(crate) fn set_next(&self, next: Option<ErasedGcBox>) {
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
        tagged_ptr::set::<0x3, _>(
            &self.tagged_vtable,
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
        tagged_ptr::get::<0x4, _>(self.tagged_vtable.get()) != 0x0
    }

    #[inline]
    pub(crate) fn set_needs_trace(&self, needs_trace: bool) {
        tagged_ptr::set_bool::<0x4, _>(&self.tagged_vtable, needs_trace);
    }

    /// Determines whether or not we've dropped the `dyn Collect` value
    /// stored in `GcBox.value`
    /// When we garbage-collect a `GcBox` that still has outstanding weak pointers,
    /// we set `alive` to false. When there are no more weak pointers remaining,
    /// we will deallocate the `GcBox`, but skip dropping the `dyn Collect` value
    /// (since we've already done it).
    #[inline]
    pub(crate) fn is_live(&self) -> bool {
        tagged_ptr::get::<0x8, _>(self.tagged_vtable.get()) != 0x0
    }

    #[inline]
    pub(crate) fn set_live(&self, alive: bool) {
        tagged_ptr::set_bool::<0x8, _>(&self.tagged_vtable, alive);
    }

    /// Returns whether the `Layout` is stored just before the `GcBoxHeader` instead of inside the
    /// vtable.
    #[inline]
    pub(crate) fn dynamic_layout(&self) -> bool {
        tagged_ptr::get::<0x10, _>(self.tagged_vtable.get()) != 0x0
    }

    /// Sets the `dynamic_layout` flag.
    #[inline]
    fn set_dynamic_layout(&self, dynamic_layout: bool) {
        tagged_ptr::set_bool::<0x10, _>(&self.tagged_vtable, dynamic_layout);
    }
}

/// Type-specific operations for GC'd values.
///
/// We use a custom vtable instead of `dyn Collect` for extra flexibility.
/// The type is over-aligned so that `GcBoxHeader` can store flags into the LSBs of the vtable pointer.
#[repr(align(32))]
struct CollectVtable {
    /// The layout of the `GcBox` the GC'd value is stored in, if the object does not have a
    /// dynamic layout. Ignored if it does.
    box_layout: Layout,

    /// Given an `ErasedGcBox`, drops the value without deallocating the box.
    drop_value: unsafe fn(ErasedGcBox),

    /// Given an `ErasedGcBox`, traces the value.
    trace_value: unsafe fn(ErasedGcBox, &mut Context),
}

impl CollectVtable {
    /// Returns a vtable for the current type, allocated in static memory.
    fn new<'gc, T: Collect<'gc>>() -> &'static CollectVtable {
        // Helper trait to materialize vtables in static memory.
        trait HasCollectVtable {
            const VTABLE: CollectVtable;
        }

        impl<'gc, T: Collect<'gc>> HasCollectVtable for T {
            const VTABLE: CollectVtable = CollectVtable {
                box_layout: GcBoxLayout::for_fixed::<T>().box_layout,
                drop_value: |erased| unsafe {
                    let gc_box = erased.unerase::<T>();
                    ptr::drop_in_place(gc_box.get_ptr().as_ptr());
                },
                trace_value: |erased, cc| unsafe {
                    let gc_box = erased.unerase::<T>();
                    gc_box.get_ptr().as_ref().trace(cc)
                },
            };
        }

        &<T as HasCollectVtable>::VTABLE
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

/// Utility functions for tagging and untagging pointers.
mod tagged_ptr {
    use core::cell::Cell;

    trait ValidMask<const MASK: usize> {
        const CHECK: ();
    }

    impl<T, const MASK: usize> ValidMask<MASK> for T {
        const CHECK: () = assert!(MASK < core::mem::align_of::<T>());
    }

    /// Checks that `$mask` can be used to tag a pointer to `$type`.
    /// If this isn't true, this macro will cause a post-monomorphization error.
    macro_rules! check_mask {
        ($type:ty, $mask:expr) => {
            let _ = <$type as ValidMask<$mask>>::CHECK;
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

    #[inline(always)]
    pub(super) fn set<const MASK: usize, T>(pcell: &Cell<*const T>, tag: usize) {
        check_mask!(T, MASK);
        let ptr = pcell.get();
        let ptr = ptr.map_addr(|addr| (addr & !MASK) | (tag & MASK));
        pcell.set(ptr)
    }

    #[inline(always)]
    pub(super) fn set_bool<const MASK: usize, T>(pcell: &Cell<*const T>, value: bool) {
        check_mask!(T, MASK);
        let ptr = pcell.get();
        let ptr = ptr.map_addr(|addr| (addr & !MASK) | if value { MASK } else { 0 });
        pcell.set(ptr)
    }
}

#[cfg(test)]
mod tests {
    use super::GcBoxLayout;
    use core::alloc::Layout;

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn gcbox_layout() {
        #[repr(align(32))]
        struct Align32<T>(T);

        assert_eq!(
            GcBoxLayout::for_fixed::<u8>(),
            GcBoxLayout {
                box_layout: Layout::from_size_align(17, 8).unwrap(),
                offset_of_box_layout: 0,
                offset_of_header: 0,
                offset_of_value: 16,
            }
        );
        assert_eq!(
            GcBoxLayout::for_fixed::<usize>(),
            GcBoxLayout {
                box_layout: Layout::from_size_align(24, 8).unwrap(),
                offset_of_box_layout: 0,
                offset_of_header: 0,
                offset_of_value: 16,
            }
        );
        assert_eq!(
            GcBoxLayout::for_fixed::<u128>(),
            GcBoxLayout {
                box_layout: Layout::from_size_align(32, 16).unwrap(),
                offset_of_box_layout: 0,
                offset_of_header: 0,
                offset_of_value: 16,
            }
        );
        assert_eq!(
            GcBoxLayout::for_fixed::<[u8; 32]>(),
            GcBoxLayout {
                box_layout: Layout::from_size_align(48, 8).unwrap(),
                offset_of_box_layout: 0,
                offset_of_header: 0,
                offset_of_value: 16,
            }
        );
        assert_eq!(
            GcBoxLayout::for_fixed::<[u8; 64]>(),
            GcBoxLayout {
                box_layout: Layout::from_size_align(80, 8).unwrap(),
                offset_of_box_layout: 0,
                offset_of_header: 0,
                offset_of_value: 16,
            }
        );
        assert_eq!(
            GcBoxLayout::for_fixed::<Align32<[u8; 32]>>(),
            GcBoxLayout {
                box_layout: Layout::from_size_align(64, 32).unwrap(),
                offset_of_box_layout: 0,
                offset_of_header: 16,
                offset_of_value: 32,
            }
        );
        assert_eq!(
            GcBoxLayout::for_fixed::<Align32<[u8; 64]>>(),
            GcBoxLayout {
                box_layout: Layout::from_size_align(96, 32).unwrap(),
                offset_of_box_layout: 0,
                offset_of_header: 16,
                offset_of_value: 32,
            }
        );
    }
}
