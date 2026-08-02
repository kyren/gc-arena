use core::mem;

use crate::{
    collect::{Collect, Trace},
    context::Mutation,
    gc::Gc,
};

/// Provides an optimization for allocating [`Gc`] pointers to Zero Sized Types.
///
/// This type stores a single dummy allocation of the requested alignment. When an alloc method is
/// called, if the given value is a ZST whose alignment is less than or equal to the given one, then
/// the dummy allocation is cast to the correct type and that is returned instead.
///
/// All ZSTs allocated this way can share a single allocated pointer. This obviously breaks pointer
/// uniqueness, so if pointer uniqueness is required, [`Gc::new`] should be used instead.
#[derive(Copy, Clone)]
pub struct ZstCache<'gc, const MAX_ALIGN: usize> {
    cached_ptr: Gc<'gc, ()>,
}

unsafe impl<'gc, const MAX_ALIGN: usize> Collect<'gc> for ZstCache<'gc, MAX_ALIGN> {
    const NEEDS_TRACE: bool = true;

    fn trace<T: Trace<'gc>>(&self, cc: &mut T) {
        cc.trace_gc(self.cached_ptr);
    }
}

impl<'gc, const MAX_ALIGN: usize> ZstCache<'gc, MAX_ALIGN>
where
    Alignment<MAX_ALIGN>: ValidAlignment,
{
    pub fn new(mc: &Mutation<'gc>) -> Self {
        let cached_ptr = Gc::erase(Gc::new_static(
            mc,
            <Alignment<MAX_ALIGN> as HasAlignedType>::AlignedType::default(),
        ));
        ZstCache { cached_ptr }
    }

    /// Returns the internally held pointer used as a ZST ptr cache.
    ///
    /// It will always be aligned to `MAX_ALIGN`.
    pub fn cached_ptr(&self) -> Gc<'gc, ()> {
        self.cached_ptr
    }

    /// Returns true if the given pointer is cached by this `ZstCache`.
    #[inline]
    pub fn is_cached<T: ?Sized>(&self, p: Gc<'gc, T>) -> bool {
        Gc::ptr_eq(self.cached_ptr, Gc::erase(p))
    }

    /// Return the cached pointer as a pointer to `T`, if possible.
    ///
    /// If the given type `T` is not a ZST or has an alignment which is greater than `MAX_ALIGN`,
    /// returns `None`.
    ///
    /// This method never performs any actual allocation.
    #[inline]
    pub fn alloc_zst<T: 'gc>(&self) -> Option<Gc<'gc, T>> {
        if mem::size_of::<T>() == 0 && mem::align_of::<T>() <= MAX_ALIGN {
            debug_assert!(Gc::as_ptr(self.cached_ptr).align_offset(mem::align_of::<T>()) == 0);
            // SAFETY: The value is zero sized, and this pointer is at least of the correct
            // alignment for the pointed to type.
            Some(unsafe { Gc::cast::<T>(self.cached_ptr) })
        } else {
            None
        }
    }

    /// Like [`Gc::new`], but returns the cached pointer if possible.
    #[inline]
    pub fn alloc<T: Collect<'gc>>(&self, mc: &Mutation<'gc>, t: T) -> Gc<'gc, T> {
        if let Some(ptr) = self.alloc_zst() {
            ptr
        } else {
            Gc::new(mc, t)
        }
    }

    /// Like [`Gc::new_static`], but returns the cached pointer if possible.
    #[inline]
    pub fn alloc_static<T: 'static>(&self, mc: &Mutation<'gc>, t: T) -> Gc<'gc, T> {
        if let Some(ptr) = self.alloc_zst() {
            ptr
        } else {
            Gc::new_static(mc, t)
        }
    }
}

pub struct Alignment<const ALIGN: usize>;

/// For all alignments `ALIGN` that `ZstCache` supports, [`Alignment<ALIGN>`] will implement this
/// trait.
///
/// All positive powers of 2 up to 2^29 are supported.
#[allow(private_bounds)]
pub trait ValidAlignment: HasAlignedType {}

impl<T: HasAlignedType> ValidAlignment for T {}

trait HasAlignedType {
    type AlignedType: Default;
}

macro_rules! impl_has_aligned_type {
    ($($align:expr),* $(,)?) => {
        $(
            const _: () = {
                #[repr(align($align))]
                struct AlignedType;

                impl Default for AlignedType {
                    #[inline(always)]
                    fn default() -> Self {
                        Self
                    }
                }

                impl HasAlignedType for Alignment<$align> {
                    type AlignedType = AlignedType;
                }
            };
        )*
    };
}

impl_has_aligned_type!(
    1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072,
    262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216, 33554432, 67108864, 134217728,
    268435456, 536870912
);
