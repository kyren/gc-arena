use core::alloc::Layout;

/// A trait which can instantiate type-level metadata for `Gc` pointers.
///
/// Types implementing this trait are what *instantiates* the type-level metadata, so that different
/// instances of this can be used for the same type of `Gc` pointer.
///
/// The metadata pointer for allocated `Gc` values will be stored in a *per-type* static vtable (one
/// vtable per (allocated type <-> metadata) pair), there is no per-allocation cost.
pub trait TypeMeta {
    type Metadata: 'static;

    const METADATA: &'static Self::Metadata;
}

/// A simple implementation of [`TypeMeta`] that does not store any type-level metadata (only the
/// `()` (unit) type).
pub struct UnitTypeMeta;

impl TypeMeta for UnitTypeMeta {
    type Metadata = ();

    const METADATA: &'static Self::Metadata = &();
}

/// A trait to describe pointers to unsized values and the *per-value* metadata stored in the GC
/// header.
///
/// This trait is necessary to implement for the garbage collector to drop unsized values, and can
/// be used to convert `Gc` pointers into a "thin" representation.
///
/// The [`PtrMeta::Metadata`] value will be stored next to the allocated value in memory, so there
/// is a *per-allocation* cost.
pub unsafe trait PtrMeta<T: ?Sized> {
    type Metadata: Copy + Send;
    type Thin;

    fn to_raw_parts(fat: *const T) -> (*const Self::Thin, Self::Metadata);
    fn from_raw_parts(thin: *const Self::Thin, metadata: Self::Metadata) -> *const T;
}

/// An extension of the [`PtrMeta`] trait that tells the garbage collector how to allocate and free
/// memory associated with unsized values.
pub unsafe trait AllocMeta<T: ?Sized>: PtrMeta<T> {
    fn layout(metadata: Self::Metadata) -> Option<Layout>;
}

/// A trivial implementation of [`PtrMeta`] and [`AllocMeta`] that can only allocate sized values
/// and cannot be used to convert a "fat" pointer to a "thin" one.
///
/// In this representation, the `PtrMeta::Metadata` type is `()` (unit) and the "fat" `*const T`
/// pointer is the same as the "thin" `*const PtrMeta::Thin` pointer.
pub struct UnitPtrMeta;

unsafe impl<T> PtrMeta<T> for UnitPtrMeta {
    type Metadata = ();
    type Thin = T;

    #[inline]
    fn to_raw_parts(fat: *const T) -> (*const T, Self::Metadata) {
        (fat, ())
    }

    #[inline]
    fn from_raw_parts(thin: *const T, _metadata: Self::Metadata) -> *const T {
        thin
    }
}

unsafe impl<T> AllocMeta<T> for UnitPtrMeta {
    #[inline]
    fn layout(_metadata: ()) -> Option<Layout> {
        Some(Layout::new::<T>())
    }
}
