use alloc::alloc;
use core::{
    alloc::{Layout, LayoutError},
    cell::Cell,
    fmt,
    marker::PhantomData,
    mem,
    ptr::{self, NonNull},
};

use crate::{
    collect::Collect,
    context::Context,
    meta::{self, AllocMeta, PtrMeta, TypeMeta},
    types::GcColor,
};

/// A pointer to a GC object.
///
/// Pointers to GC objects store metadata in the same allocation before* the stored pointer.
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

impl<'gc, T: ?Sized + Collect<'gc>> GcPtr<T> {
    /// Allocate a new GC value with a default header.
    ///
    /// The `GcPtr` is returned with its held value uninitialized.
    ///
    /// # Panics
    ///
    /// Panics if there is no valid layout we can allocate.
    #[inline]
    pub(crate) fn alloc<TM: TypeMeta, P: AllocMeta<T, TM::TypeMetadata>>(
        ptr_meta: P::PtrMetadata,
    ) -> Self {
        let meta_header_layout = PtrProps::<T, TM::TypeMetadata, P>::META_HEADER_LAYOUT;
        let value_layout = P::layout(TM::TYPE_METADATA, ptr_meta).expect("no layout for value");
        let (alloc_layout, value_offset) = prefix_header_layout(meta_header_layout, value_layout)
            .expect("no layout for GC allocation");

        unsafe {
            let block = alloc::alloc(alloc_layout).cast::<()>();
            let Some(block) = NonNull::new(block) else {
                alloc::handle_alloc_error(alloc_layout);
            };

            let value_ptr = block.byte_add(value_offset);

            let meta_ptr = value_ptr
                .byte_sub(meta_header_layout.size())
                .cast::<P::PtrMetadata>();

            let header_ptr = value_ptr
                .byte_sub(mem::size_of::<GcHeader>())
                .cast::<GcHeader>();

            let fat_ptr = P::from_thin(
                TM::TYPE_METADATA,
                value_ptr.cast::<P::Thin>().as_ptr(),
                ptr_meta,
            )
            .cast_mut();

            debug_assert!(
                meta_ptr.is_aligned()
                    && header_ptr.is_aligned()
                    && fat_ptr.addr().is_multiple_of(value_layout.align())
            );

            meta_ptr.write(ptr_meta);
            header_ptr.write(GcHeader::new(
                &TM::__type_properties::<VtableFor<T, TM, P>>(),
            ));

            GcPtr(NonNull::new_unchecked(fat_ptr))
        }
    }
}

impl<T: ?Sized> GcPtr<T> {
    #[inline(always)]
    pub(crate) fn header(&self) -> &GcHeader {
        unsafe {
            self.0
                .byte_sub(mem::size_of::<GcHeader>())
                .cast::<GcHeader>()
                .as_ref()
        }
    }

    /// # Safety
    ///
    /// The given `M` per-type and `P` per-ptr metadata types must be compatible with the one used
    /// to allocate the `GcPtr`.
    #[inline(always)]
    pub(crate) unsafe fn fat_ptr<F: ?Sized, M: Copy, P: PtrMeta<F, M>>(self) -> GcPtr<F> {
        unsafe {
            GcPtr(PtrProps::<F, M, P>::fat_ptr(
                self.type_metadata::<M>(),
                self.0,
            ))
        }
    }

    /// # Safety
    ///
    /// The given `M` per-type and `P` per-ptr metadata types must be compatible with the one used
    /// to allocate the `GcPtr`.
    #[inline(always)]
    pub(crate) unsafe fn thin_ptr<M: Copy, P: PtrMeta<T, M>>(self) -> GcPtr<P::Thin> {
        unsafe {
            let p = P::to_thin(self.type_metadata(), self.0.as_ptr());
            GcPtr(NonNull::new_unchecked(p.cast_mut()))
        }
    }

    /// # Safety
    ///
    /// The given `M` per-type metadata type must be compatible with the `TM::Metadata` used to
    /// allocate the `GcPtr`.
    #[inline(always)]
    pub(crate) unsafe fn type_metadata<M: Copy>(self) -> M {
        let header_ptr = self.header().vtable_ptr();
        let type_props_ptr = header_ptr as *const meta::__TypeProperties<M>;
        unsafe { (*type_props_ptr).metadata }
    }

    #[inline(always)]
    pub(crate) fn as_ptr(self) -> *mut T {
        self.0.as_ptr()
    }

    /// # Safety
    ///
    /// The provided ptr must have come from `GcPtr::as_ptr`.
    #[inline(always)]
    pub(crate) unsafe fn from_ptr(p: *mut T) -> Self {
        Self(unsafe { NonNull::new_unchecked(p) })
    }

    /// A convenience method to cast to `()`.
    #[inline(always)]
    pub(crate) fn erase(self) -> GcPtr {
        GcPtr(self.0.cast::<()>())
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
        unsafe { (self.header().vtable().trace_value)(self.0.cast::<()>(), cc) }
    }

    /// Drops the stored value.
    ///
    /// # Safety:
    ///
    /// The value must not have been previously dropped and the pointer must not have been
    /// deallocated.
    #[inline(always)]
    pub(crate) unsafe fn drop_in_place(&mut self) {
        unsafe { (self.header().vtable().drop_value)(self.0.cast::<()>()) }
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
        unsafe { (self.header().vtable().dealloc)(self.0.cast::<()>()) }
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
    tagged_vtable: Cell<*const GcVtable>,
}

impl GcHeader {
    /// Create a new `GcHeader` with:
    /// 1) color set to `White`
    /// 2) `needs_trace` set to `false`
    /// 3) `is_live` set to `false`
    #[inline(always)]
    fn new<M>(type_props: &'static meta::__TypeProperties<M>) -> Self {
        Self {
            next: Cell::new(None),
            tagged_vtable: Cell::new(ptr::from_ref(type_props) as *const GcVtable),
        }
    }

    #[inline(always)]
    fn vtable(&self) -> &'static GcVtable {
        let ptr = tagged_ptr::untag(self.tagged_vtable.get());
        // SAFETY:
        // - the pointer was properly untagged.
        // - the vtable is stored in static memory.
        unsafe { &*ptr }
    }

    #[inline(always)]
    fn vtable_ptr(&self) -> *const GcVtable {
        tagged_ptr::untag(self.tagged_vtable.get())
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
    /// When we garbage-collect a `GcPtr` that still has outstanding weak pointers, we set `is_live`
    /// to false. When there are no more weak pointers remaining, we will deallocate the `GcPtr`,
    /// but skip dropping the `dyn Collect` value (since we've already done it).
    #[inline]
    pub(crate) fn is_live(&self) -> bool {
        tagged_ptr::get_bool::<0x8, _>(self.tagged_vtable.get())
    }

    #[inline]
    pub(crate) fn set_live(&self, is_live: bool) {
        self.tagged_vtable
            .update(|p| tagged_ptr::set_bool::<0x8, _>(p, is_live));
    }
}

/// Type-specific operations for GC'd values.
///
/// We use a custom vtable instead of `dyn Collect` for extra flexibility. The type is over-aligned
/// so that `GcHeader` can store flags into the LSBs of the vtable pointer.
#[repr(align(16))]
pub(crate) struct GcVtable {
    /// Traces the value at the given pointer.
    trace_value: unsafe fn(NonNull<()>, &mut Context),
    /// Drops the value at the given pointer.
    drop_value: unsafe fn(NonNull<()>),
    /// Frees the allocation for given value pointer.
    dealloc: unsafe fn(NonNull<()>),
}

struct PtrProps<T: ?Sized, P, M>(PhantomData<(*const T, P, M)>);

impl<T: ?Sized, M, P: PtrMeta<T, M>> PtrProps<T, M, P> {
    const META_HEADER_LAYOUT: Layout = {
        if let Ok((layout, _)) = Layout::new::<P::PtrMetadata>().extend(Layout::new::<GcHeader>()) {
            layout.pad_to_align()
        } else {
            unreachable!();
        }
    };

    #[inline(always)]
    unsafe fn read_ptr_meta<U: ?Sized>(value_ptr: NonNull<U>) -> P::PtrMetadata {
        unsafe {
            value_ptr
                .byte_sub(Self::META_HEADER_LAYOUT.size())
                .cast::<P::PtrMetadata>()
                .read()
        }
    }

    #[inline(always)]
    unsafe fn fat_ptr<U: ?Sized>(type_meta: M, value_ptr: NonNull<U>) -> NonNull<T> {
        unsafe {
            let ptr_meta = Self::read_ptr_meta(value_ptr);
            NonNull::new_unchecked(
                P::from_thin(type_meta, value_ptr.as_ptr() as *const P::Thin, ptr_meta).cast_mut(),
            )
        }
    }
}

struct VtableFor<T: ?Sized, TM, P>(PhantomData<(*const T, TM, P)>);

impl<'gc, T: ?Sized + Collect<'gc>, TM: TypeMeta, P: AllocMeta<T, TM::TypeMetadata>>
    meta::__VtableProxy for VtableFor<T, TM, P>
{
    const VTABLE: meta::__Vtable = meta::__Vtable(GcVtable {
        trace_value: |value_ptr, cc| unsafe {
            PtrProps::<T, TM::TypeMetadata, P>::fat_ptr(TM::TYPE_METADATA, value_ptr)
                .as_ref()
                .trace(cc);
        },
        drop_value: |value_ptr| unsafe {
            ptr::drop_in_place(
                PtrProps::<T, TM::TypeMetadata, P>::fat_ptr(TM::TYPE_METADATA, value_ptr).as_ptr(),
            );
        },
        dealloc: |value_ptr| {
            unsafe {
                let ptr_meta = PtrProps::<T, TM::TypeMetadata, P>::read_ptr_meta(value_ptr);
                let (alloc_layout, value_offset) = prefix_header_layout(
                    PtrProps::<T, TM::TypeMetadata, P>::META_HEADER_LAYOUT,
                    P::layout(TM::TYPE_METADATA, ptr_meta).unwrap(),
                )
                .unwrap();

                let alloc_ptr = value_ptr.byte_sub(value_offset).as_ptr();
                // SAFETY: the pointer was allocated with this layout.
                alloc::dealloc(alloc_ptr as *mut u8, alloc_layout);
            }
        },
    });
}

/// Compute the layout of a block of memory composed of a header and a value. Returns the layout and
/// the value offset.
///
/// The layout is computed such that the header can always be placed exactly the behind the value by
/// the size of the header. Any necessary padding for proper alignment of both types can always go
/// *before* the header.
///
/// This is a simple wrapper around `Layout::extend` but it exists to document assumptions.
///
/// # Panics
///
/// This method panics if `header_layout`'s size is not a multiple of its alignment.
const fn prefix_header_layout(
    header_layout: Layout,
    value_layout: Layout,
) -> Result<(Layout, usize), LayoutError> {
    assert!(
        header_layout.size().is_multiple_of(header_layout.align()),
        "header size must be a multiple of the alignment"
    );

    match header_layout.extend(value_layout) {
        Ok((alloc_layout, value_offset)) => {
            // The header and value positions should be properly aligned.
            //
            // Value is obviuosly aligned to itself because the implementation of `Layout::extend`
            // ensures that it is.
            //
            // The position of the header (value_ptr - header_size) is also aligned to the header
            // because:
            // 1) If the header alignment is greater than the value alignment, then the value
            //    will fit right next to the header since the header's size is a multiple of its
            //    alignment and thus also the value alignment. Thus (value_ptr - header_size) is the
            //    beginning of the allocation which is already aligned to the header.
            // 2) If the value alignment is greater than the header alignment, then the value is
            //    also aligned to the header so (value_ptr - header_size) must be aligned to the
            //    header since the header size is a multiple of its alignment.

            // Assert that the above logic is true.
            debug_assert!(value_offset.is_multiple_of(value_layout.align()));
            debug_assert!(
                (value_offset - header_layout.size()).is_multiple_of(header_layout.align())
            );

            Ok((alloc_layout, value_offset))
        }
        Err(err) => Err(err),
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
