use core::{
    alloc::Layout,
    mem::{self, ManuallyDrop, MaybeUninit},
    ptr,
};

use crate::{
    Gc,
    collect::Collect,
    context::Mutation,
    gc::{GcBuilder, GcFat, GcThin},
    meta::{AllocMeta, PtrMeta, TypeMeta, UnitTypeMeta},
    static_wrapper::Static,
};

/// A dynamically sized slice of type `[E]` together with a custom header `H` in a single
/// allocation.
#[repr(C)]
pub struct SliceWithHeader<H, E> {
    pub header: H,
    pub slice: [E],
}

unsafe impl<'gc, H: Collect<'gc>, E: Collect<'gc>> Collect<'gc> for SliceWithHeader<H, E> {
    const NEEDS_TRACE: bool = H::NEEDS_TRACE || E::NEEDS_TRACE;

    fn trace<T: crate::collect::Trace<'gc>>(&self, cc: &mut T) {
        self.header.trace(cc);
        for t in &self.slice {
            t.trace(cc);
        }
    }
}

impl<H, E> SliceWithHeader<H, E> {
    #[inline(always)]
    fn ptr_to_thin(slice: *const SliceWithHeader<H, E>) -> *const H {
        slice as *const H
    }

    #[inline(always)]
    fn ptr_from_thin(ptr: *const H, len: usize) -> *const SliceWithHeader<H, E> {
        ptr::slice_from_raw_parts(ptr as *const u8, len) as *const SliceWithHeader<H, E>
    }

    #[inline(always)]
    fn layout(len: usize) -> Option<Layout> {
        // We manually construct the proper layout for `SliceWithHeader`, which we can do because it
        // is `#[repr(C)]`.
        let header_layout = Layout::new::<H>();
        let array_layout = Layout::array::<E>(len).ok()?;
        Some(header_layout.extend(array_layout).ok()?.0.pad_to_align())
    }
}

pub struct SliceWithHeaderPtrMeta;

impl<H, E, M> PtrMeta<SliceWithHeader<H, E>, M> for SliceWithHeaderPtrMeta {
    type PtrMetadata = usize;
    type Thin = H;

    #[inline]
    fn to_thin(_type_meta: M, slice: *const SliceWithHeader<H, E>) -> *const H {
        SliceWithHeader::ptr_to_thin(slice)
    }

    #[inline]
    fn from_thin(_type_meta: M, ptr: *const H, len: usize) -> *const SliceWithHeader<H, E> {
        SliceWithHeader::ptr_from_thin(ptr, len)
    }
}

impl<H, E, M> AllocMeta<SliceWithHeader<H, E>, M> for SliceWithHeaderPtrMeta {
    #[inline]
    fn layout(_type_meta: M, len: usize) -> Option<Layout> {
        SliceWithHeader::<H, E>::layout(len)
    }
}

pub type GcSliceWithHeader<'gc, H, E, M = ()> =
    GcFat<'gc, SliceWithHeader<H, E>, M, SliceWithHeaderPtrMeta>;

pub type GcThinSliceWithHeader<'gc, H, E, M = ()> =
    GcThin<'gc, SliceWithHeader<H, E>, M, SliceWithHeaderPtrMeta>;

/// Provides a way to construct a new `GcSliceWithHeader<H, E>`.
pub struct GcSliceWithHeaderBuilder<'gc, H, E, M = ()> {
    inner: GcBuilder<'gc, SliceWithHeader<H, E>, M, SliceWithHeaderPtrMeta>,
}

impl<'gc, H: Collect<'gc>, E: Collect<'gc>> GcSliceWithHeaderBuilder<'gc, H, E> {
    /// Create a new `GcSliceWithHeaderBuilder` with an uninitialized slice of length `len`.
    pub fn new(len: usize) -> Self {
        Self::new_with_type_meta::<UnitTypeMeta>(len)
    }
}

impl<'gc, H: Collect<'gc>, E: Collect<'gc>, M> GcSliceWithHeaderBuilder<'gc, H, E, M> {
    /// Create a new `GcSliceWithHeaderBuilder` with an uninitialized slice of length `len` and
    /// per-type metadata from `TM`.
    pub fn new_with_type_meta<TM: TypeMeta<TypeMetadata = M>>(len: usize) -> Self {
        // SAFETY: `SliceWithHeaderPtrMeta` is implemented correctly for `SliceWithHeader` and not
        // depend on any particular per-type metadata.
        Self {
            inner: unsafe { GcBuilder::new_with_type_and_ptr_meta::<TM>(len) },
        }
    }
}

impl<'gc, H, E, M> GcSliceWithHeaderBuilder<'gc, Static<H>, E, M> {
    /// Safely unwrap a `GcSliceWithHeaderBuilder<Static<H>, _, _>` into a
    /// `GcSliceWithHeaderBuilder<H, _, _>`.
    #[inline]
    pub fn unwrap_static_header(self) -> GcSliceWithHeaderBuilder<'gc, H, E, M> {
        unsafe {
            let builder = GcSliceWithHeaderBuilder {
                inner: GcBuilder::from_raw(self.inner.into_raw() as *mut SliceWithHeader<H, E>),
            };
            builder
        }
    }
}

impl<'gc, H, E, M> GcSliceWithHeaderBuilder<'gc, H, E, M> {
    /// Return a pointer to the (possibly uninitialized) slice being constructed.
    #[inline]
    pub fn header_ptr(&mut self) -> *mut H {
        unsafe { &raw mut (*self.inner.as_ptr()).header }
    }

    /// Convert to a [`GcSliceWithHeaderSliceBuilder`] by unsafely assuming that the held header is properly
    /// initialized.
    pub unsafe fn assume_init(self) -> GcSliceWithHeaderSliceBuilder<'gc, H, E, M> {
        GcSliceWithHeaderSliceBuilder {
            inner: ManuallyDrop::new(self.inner),
            init_length: 0,
        }
    }

    /// Convert to a [`GcSliceWithHeaderSliceBuilder`] by writing the header from `header`.
    ///
    /// The given callback will always be called in-order.
    pub fn write_header(mut self, header: H) -> GcSliceWithHeaderSliceBuilder<'gc, H, E, M> {
        unsafe {
            self.header_ptr().write(header);
            self.assume_init()
        }
    }
}

/// Used to construct the slice portion of an in-construction `GcSliceWithHeader<H, E>`.
pub struct GcSliceWithHeaderSliceBuilder<'gc, H, E, M = ()> {
    inner: ManuallyDrop<GcBuilder<'gc, SliceWithHeader<H, E>, M, SliceWithHeaderPtrMeta>>,
    init_length: usize,
}

impl<'gc, H, E, M> Drop for GcSliceWithHeaderSliceBuilder<'gc, H, E, M> {
    fn drop(&mut self) {
        unsafe {
            let slice_with_header_ptr = self.inner.as_ptr();
            let ptr = SliceWithHeader::ptr_to_thin(slice_with_header_ptr);
            let ptr = SliceWithHeader::<H, E>::ptr_from_thin(ptr, self.init_length);
            core::ptr::drop_in_place(ptr.cast_mut());

            ManuallyDrop::drop(&mut self.inner);
        }
    }
}

impl<'gc, H, E, M> GcSliceWithHeaderSliceBuilder<'gc, H, Static<E>, M> {
    /// Safely unwrap a `GcSliceWithHeaderSliceBuilder<_, Static<E>, _>` into a
    /// `GcSliceWithHeaderSliceBuilder<_, E, _>`.
    #[inline]
    pub fn unwrap_static_element(mut self) -> GcSliceWithHeaderSliceBuilder<'gc, H, E, M> {
        unsafe {
            let builder = GcSliceWithHeaderSliceBuilder {
                inner: ManuallyDrop::new(GcBuilder::from_raw(
                    ManuallyDrop::take(&mut self.inner).into_raw() as *mut SliceWithHeader<H, E>,
                )),
                init_length: self.init_length,
            };
            mem::forget(self);
            builder
        }
    }
}

impl<'gc, H, E, M> GcSliceWithHeaderSliceBuilder<'gc, H, E, M> {
    /// Return a pointer to the (possibly uninitialized) slice being constructed.
    #[inline]
    pub fn slice_ptr(&mut self) -> *mut [E] {
        unsafe { &raw mut (*self.inner.as_ptr()).slice }
    }

    /// Finish constructing a `GcSliceWithHeader<H, E>` by unsafely assuming that the held slice is
    /// properly initialized.
    pub unsafe fn assume_init(mut self, mc: &Mutation<'gc>) -> GcSliceWithHeader<'gc, H, E, M> {
        unsafe {
            let inner = ManuallyDrop::take(&mut self.inner);
            mem::forget(self);
            inner.assume_init(mc)
        }
    }

    /// Finish constructing a `GcSliceWithHeader<H, E>` by initializing elements from the given
    /// callback.
    ///
    /// The given callback will always be called in-order.
    pub fn write_slice_with(
        mut self,
        mc: &Mutation<'gc>,
        mut create_element: impl FnMut(usize) -> E,
    ) -> GcSliceWithHeader<'gc, H, E, M> {
        unsafe {
            for (i, element) in (self.slice_ptr() as *mut [MaybeUninit<E>])
                .as_mut_unchecked()
                .iter_mut()
                .enumerate()
            {
                element.write(create_element(i));
                self.init_length = i + 1;
            }
            self.assume_init(mc)
        }
    }
}

impl<'gc, H, E: Copy, M> GcSliceWithHeaderSliceBuilder<'gc, H, E, M> {
    /// Finish constructing a `GcSliceWithHeader<H, E>` by copying elements from the given slice.
    ///
    /// # Panics
    ///
    /// Panics if `elements.len()` is not at least as large as the length given in
    /// [`GcSliceWithHeaderBuilder::new`].
    pub fn copy_slice(
        mut self,
        mc: &Mutation<'gc>,
        elements: &[E],
    ) -> GcSliceWithHeader<'gc, H, E, M> {
        let len = self.slice_ptr().len();
        assert!(
            elements.len() >= len,
            "`elements` is less than length {len}"
        );
        unsafe {
            ptr::copy_nonoverlapping(
                elements.as_ptr(),
                self.slice_ptr() as *mut E,
                elements.len(),
            );
            self.assume_init(mc)
        }
    }
}

pub type GcSlice<'gc, E, M = ()> = GcFat<'gc, [E], M, SlicePtrMeta>;
pub type GcThinSlice<'gc, E, M = ()> = GcThin<'gc, [E], M, SlicePtrMeta>;

impl<'gc, E: Collect<'gc> + Copy> GcSlice<'gc, E> {
    pub fn new_slice(mc: &Mutation<'gc>, elements: &[E]) -> GcSlice<'gc, E> {
        GcSliceBuilder::new(elements.len()).copy_slice(mc, elements)
    }
}

impl<'gc, E: 'static + Copy> GcSlice<'gc, E> {
    pub fn new_slice_static(mc: &Mutation<'gc>, elements: &[E]) -> GcSlice<'gc, E> {
        GcSliceBuilder::new(elements.len())
            .unwrap_static()
            .copy_slice(mc, elements)
    }
}

pub struct SlicePtrMeta;

impl<E, M> PtrMeta<[E], M> for SlicePtrMeta {
    type PtrMetadata = usize;
    type Thin = ();

    #[inline]
    fn to_thin(_type_meta: M, slice: *const [E]) -> *const () {
        SliceWithHeader::ptr_to_thin(slice as *const SliceWithHeader<(), E>)
    }

    #[inline]
    fn from_thin(_type_meta: M, ptr: *const (), len: usize) -> *const [E] {
        SliceWithHeader::<(), E>::ptr_from_thin(ptr, len) as *const [E]
    }
}

impl<E, M> AllocMeta<[E], M> for SlicePtrMeta {
    #[inline]
    fn layout(_type_meta: M, len: usize) -> Option<Layout> {
        SliceWithHeader::<(), E>::layout(len)
    }
}

/// Provides a way to construct a new `GcSlice<E>`.
pub struct GcSliceBuilder<'gc, E, M = ()>(GcSliceWithHeaderSliceBuilder<'gc, (), E, M>);

impl<'gc, E: Collect<'gc>> GcSliceBuilder<'gc, E> {
    /// Create a new `GcSliceBuilder` with an uninitialized slice of length `len`.
    pub fn new(len: usize) -> Self {
        Self(GcSliceWithHeaderBuilder::<(), E>::new(len).write_header(()))
    }
}

impl<'gc, E: Collect<'gc>, M> GcSliceBuilder<'gc, E, M> {
    /// Create a new `GcSliceBuilder` with an uninitialized slice of length `len` and per-type
    /// metadata from `TM`.
    pub fn new_with_type_meta<TM: TypeMeta<TypeMetadata = M>>(len: usize) -> Self {
        Self(GcSliceWithHeaderBuilder::<(), E, M>::new_with_type_meta::<TM>(len).write_header(()))
    }
}

impl<'gc, E, M> GcSliceBuilder<'gc, Static<E>, M> {
    /// Safely unwrap a `GcSliceBuilder<Static<E>, _>` into a `GcSliceBuilder<E, _>`.
    #[inline]
    pub fn unwrap_static(self) -> GcSliceBuilder<'gc, E, M> {
        GcSliceBuilder(self.0.unwrap_static_element())
    }
}

impl<'gc, E, M> GcSliceBuilder<'gc, E, M> {
    /// Return a pointer to the (possibly uninitialized) slice being constructed.
    #[inline]
    pub fn slice_ptr(&mut self) -> *mut [E] {
        self.0.slice_ptr()
    }

    /// Finish constructing a `GcSlice<E>` by unsafely assuming that the held slice is properly
    /// initialized.
    pub unsafe fn assume_init(self, mc: &Mutation<'gc>) -> GcSlice<'gc, E, M> {
        unsafe { Gc::from_ptr_with_kind(Gc::as_ptr(self.0.assume_init(mc)) as *const [E]) }
    }

    /// Finish constructing a `GcSlice<E>` by initializing elements from the given callback.
    ///
    /// The given callback will always be called in-order.
    pub fn write_slice_with(
        self,
        mc: &Mutation<'gc>,
        create_element: impl FnMut(usize) -> E,
    ) -> GcSlice<'gc, E, M> {
        let val = self.0.write_slice_with(mc, create_element);
        unsafe { Gc::from_ptr_with_kind(Gc::as_ptr(val) as *const [E]) }
    }
}

impl<'gc, E: Copy, M> GcSliceBuilder<'gc, E, M> {
    /// Finish constructing a `GcSlice<E>` by copying elements from the given slice.
    ///
    /// # Panics
    ///
    /// Panics if `elements.len()` is not at least as large as the length given in
    /// [`GcSliceBuilder::new`].
    pub fn copy_slice(self, mc: &Mutation<'gc>, elements: &[E]) -> GcSlice<'gc, E, M> {
        let val = self.0.copy_slice(mc, elements);
        unsafe { Gc::from_ptr_with_kind(Gc::as_ptr(val) as *const [E]) }
    }
}

pub type GcStr<'gc, M = ()> = GcFat<'gc, str, M, StrPtrMeta>;
pub type GcThinStr<'gc, M = ()> = GcThin<'gc, str, M, StrPtrMeta>;

impl<'gc> GcStr<'gc> {
    pub fn new_str(mc: &Mutation<'gc>, s: &str) -> GcStr<'gc> {
        GcStrBuilder::new(s.len()).copy_str(mc, s)
    }
}

pub struct StrPtrMeta;

impl<M> PtrMeta<str, M> for StrPtrMeta {
    type PtrMetadata = usize;
    type Thin = ();

    #[inline]
    fn to_thin(_type_meta: M, s: *const str) -> *const () {
        SliceWithHeader::ptr_to_thin(s as *const SliceWithHeader<(), u8>)
    }

    #[inline]
    fn from_thin(_type_meta: M, s: *const (), len: usize) -> *const str {
        SliceWithHeader::<(), u8>::ptr_from_thin(s, len) as *const str
    }
}

impl<M> AllocMeta<str, M> for StrPtrMeta {
    #[inline]
    fn layout(_type_meta: M, len: usize) -> Option<Layout> {
        SliceWithHeader::<(), u8>::layout(len)
    }
}

/// Provides a way to construct a new `GcStr`.
pub struct GcStrBuilder<'gc, M = ()>(GcSliceBuilder<'gc, u8, M>);

impl<'gc> GcStrBuilder<'gc> {
    /// Create a new `GcStr` with an uninitialized str of length `len`.
    pub fn new(len: usize) -> Self {
        Self(GcSliceBuilder::<u8>::new(len))
    }
}

impl<'gc, M> GcStrBuilder<'gc, M> {
    /// Create a new `GcStrBuilder` with an uninitialized str of length `len` and per-type metadata
    /// from `TM`.
    pub fn new_with_type_meta<TM: TypeMeta<TypeMetadata = M>>(len: usize) -> Self {
        Self(GcSliceBuilder::<u8, M>::new_with_type_meta::<TM>(len))
    }
}

impl<'gc, M> GcStrBuilder<'gc, M> {
    /// Return a pointer to the (possibly uninitialized) str being constructed.
    #[inline]
    pub fn str_ptr(&mut self) -> *mut str {
        self.0.slice_ptr() as *mut str
    }

    /// Finish constructing a `GcStr` by unsafely assuming that the held str is properly
    /// initialized.
    pub unsafe fn assume_init(self, mc: &Mutation<'gc>) -> GcStr<'gc, M> {
        unsafe { Gc::from_ptr_with_kind(Gc::as_ptr(self.0.assume_init(mc)) as *const str) }
    }

    /// Finish constructing a `GcStr` by copying from the given `str`.
    ///
    /// # Panics
    ///
    /// Panics if `s.len()` is not at least as large as the length given in [`GcStrBuilder::new`].
    pub fn copy_str(self, mc: &Mutation<'gc>, s: &str) -> GcStr<'gc, M> {
        let s = self.0.copy_slice(mc, s.as_bytes());
        unsafe { Gc::from_ptr_with_kind(Gc::as_ptr(s) as *const str) }
    }
}
