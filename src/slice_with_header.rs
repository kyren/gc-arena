use core::{
    alloc::Layout,
    marker::PhantomData,
    mem::{self, ManuallyDrop, MaybeUninit},
    ptr,
};

use crate::{
    collect::Collect,
    context::Mutation,
    gc::{Gc, GcBuilder},
    meta::{Fat, PtrMeta, Thin, TypeMeta, UnitTypeMeta},
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

pub type GcSliceWithHeader<'gc, H, E, M = ()> =
    Gc<'gc, SliceWithHeader<H, E>, Fat<SliceWithHeaderPtrMeta<H, E, M>>>;

pub type GcThinSliceWithHeader<'gc, H, E, M = ()> =
    Gc<'gc, SliceWithHeader<H, E>, Thin<SliceWithHeaderPtrMeta<H, E, M>>>;

pub struct SliceWithHeaderPtrMeta<H, E, M>(PhantomData<(H, E, M)>);

unsafe impl<H, E, M: 'static> PtrMeta<SliceWithHeader<H, E>> for SliceWithHeaderPtrMeta<H, E, M> {
    type TypeMetadata = M;
    type PtrMetadata = usize;
    type Thin = H;

    #[inline]
    fn layout(len: usize) -> Option<Layout> {
        // We manually construct the proper layout for `HeaderSlice`, which we can do because it
        // is `#[repr(C)]`.
        let header_layout = Layout::new::<H>();
        let array_layout = Layout::array::<E>(len).ok()?;
        Some(header_layout.extend(array_layout).ok()?.0.pad_to_align())
    }

    #[inline]
    fn to_raw_parts(slice: *const SliceWithHeader<H, E>) -> (*const H, usize) {
        (slice as *const H, (slice as *const [u8]).len())
    }

    #[inline]
    fn from_raw_parts(ptr: *const H, len: usize) -> *const SliceWithHeader<H, E> {
        ptr::slice_from_raw_parts(ptr as *const u8, len) as *const SliceWithHeader<H, E>
    }
}

pub struct GcSliceWithHeaderBuilder<'gc, H, E, M: 'static = ()> {
    inner: ManuallyDrop<GcBuilder<'gc, SliceWithHeader<H, E>, SliceWithHeaderPtrMeta<H, E, M>>>,
    init_length: usize,
}

impl<'gc, H, E, M> Drop for GcSliceWithHeaderBuilder<'gc, H, E, M> {
    fn drop(&mut self) {
        unsafe {
            let slice_ptr = self.inner.as_ptr();
            let (ptr, _) = SliceWithHeaderPtrMeta::<H, E, M>::to_raw_parts(slice_ptr);
            let ptr = SliceWithHeaderPtrMeta::<H, E, M>::from_raw_parts(ptr, self.init_length);
            core::ptr::drop_in_place(ptr.cast_mut());

            ManuallyDrop::drop(&mut self.inner);
        }
    }
}

impl<'gc, H: Collect<'gc>, E: Collect<'gc>> GcSliceWithHeaderBuilder<'gc, H, E> {
    pub fn allocate(mc: &Mutation<'gc>, header: H, len: usize) -> Self {
        Self::allocate_with_type_meta::<UnitTypeMeta>(mc, header, len)
    }
}

impl<'gc, H: Collect<'gc>, E: Collect<'gc>, M> GcSliceWithHeaderBuilder<'gc, H, E, M> {
    pub fn allocate_with_type_meta<TM: TypeMeta<Metadata = M>>(
        mc: &Mutation<'gc>,
        header: H,
        len: usize,
    ) -> Self {
        let mut this = Self {
            inner: ManuallyDrop::new(GcBuilder::allocate_with_all_meta::<TM>(mc, len)),
            init_length: 0,
        };

        unsafe {
            ptr::write(&raw mut (*this.inner.as_mut_ptr()).header, header);
        }

        this
    }
}

impl<'gc, H, E, M> GcSliceWithHeaderBuilder<'gc, Static<H>, E, M> {
    pub fn unwrap_static_header(mut self) -> GcSliceWithHeaderBuilder<'gc, H, E, M> {
        unsafe {
            let builder = GcSliceWithHeaderBuilder {
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

impl<'gc, H, E, M> GcSliceWithHeaderBuilder<'gc, H, Static<E>, M> {
    pub fn unwrap_static_slice(mut self) -> GcSliceWithHeaderBuilder<'gc, H, E, M> {
        unsafe {
            let builder = GcSliceWithHeaderBuilder {
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

impl<'gc, H, E, M> GcSliceWithHeaderBuilder<'gc, H, E, M> {
    pub fn as_ptr(&self) -> *const SliceWithHeader<H, E> {
        self.inner.as_ptr()
    }

    pub fn as_mut_ptr(&mut self) -> *mut SliceWithHeader<H, E> {
        self.inner.as_mut_ptr()
    }

    pub unsafe fn assume_init(mut self) -> GcSliceWithHeader<'gc, H, E, M> {
        unsafe {
            let inner = ManuallyDrop::take(&mut self.inner);
            mem::forget(self);
            inner.assume_init()
        }
    }

    pub fn write_slice_with(
        mut self,
        mut create_element: impl FnMut(usize) -> E,
    ) -> GcSliceWithHeader<'gc, H, E, M> {
        unsafe {
            let slice_ptr = &raw mut (*self.as_mut_ptr()).slice;
            for (i, element) in (slice_ptr as *mut [MaybeUninit<E>])
                .as_mut_unchecked()
                .iter_mut()
                .enumerate()
            {
                element.write(create_element(i));
                self.init_length = i + 1;
            }
            self.assume_init()
        }
    }
}

impl<'gc, H, E: Copy, M> GcSliceWithHeaderBuilder<'gc, H, E, M> {
    pub fn write_slice_copy(mut self, elements: &[E]) -> GcSliceWithHeader<'gc, H, E, M> {
        unsafe {
            let slice_ptr = &raw mut (*self.as_mut_ptr()).slice;
            ptr::copy_nonoverlapping(elements.as_ptr(), slice_ptr as *mut E, elements.len());
            self.assume_init()
        }
    }
}
