use core::{
    alloc::{Layout, LayoutError},
    ptr,
};

use crate::collect::Collect;

/// A dynamically sized slice of type `[E]` together with a custom header `H` in a single
/// allocation.
#[repr(C)]
pub struct HeaderSlice<H, E> {
    pub header: H,
    pub slice: [E],
}

unsafe impl<'gc, H: Collect<'gc>, E: Collect<'gc>> Collect<'gc> for HeaderSlice<H, E> {
    const NEEDS_TRACE: bool = H::NEEDS_TRACE || E::NEEDS_TRACE;

    fn trace<T: crate::collect::Trace<'gc>>(&self, cc: &mut T) {
        self.header.trace(cc);
        for t in &self.slice {
            t.trace(cc);
        }
    }
}

impl<H, E> HeaderSlice<H, E> {
    /// Cast a pointer to a `HeaderSlice` into a thin pointer to just the header.
    #[inline]
    pub const fn to_thin_ptr(ptr: *const Self) -> *const H {
        ptr as *const H
    }

    /// Convert a thin `H` header pointer to a fat `HeaderSlice` pointer.
    ///
    /// # Safety
    ///
    /// This function is safe to call in all cases, but in order to safely dereference the resulting
    /// pointer, the following two conditions must hold:
    ///
    /// 1) The header pointer `ptr` must be a dereferencable pointer to a `HeaderSlice` object that
    ///    is compatible with this `HeaderSlice<H, E>` type.
    /// 2) The slice portion must point to `len` consecutive properly initialized values.
    #[inline]
    pub const fn from_thin_ptr(ptr: *const H, len: usize) -> *const Self {
        ptr::slice_from_raw_parts(ptr as *const u8, len) as *const Self
    }

    /// The allocation layout for a `HeaderSlice<H, E>` equivalent to a fixed size slice of length
    /// `len`.
    #[inline]
    pub const fn layout(len: usize) -> Result<Layout, LayoutError> {
        // We manually construct the proper layout for `HeaderSlice`, which we can do because it
        // is `#[repr(C)]`.

        let header_layout = Layout::new::<H>();

        let array_layout = match Layout::array::<E>(len) {
            Ok(l) => l,
            Err(e) => return Err(e),
        };

        match header_layout.extend(array_layout) {
            Ok((layout, _)) => Ok(layout.pad_to_align()),
            Err(e) => Err(e),
        }
    }
}
