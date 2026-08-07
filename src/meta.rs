use core::alloc::Layout;

/// A trait which can instantiate type-level metadata for `Gc` pointers.
///
/// Types implementing this trait are what *instantiates* the type-level metadata, so that different
/// instances of this can be used for the same type of `Gc` pointer.
///
/// The metadata pointer for allocated `Gc` values will be stored in a *per-type* static vtable (one
/// vtable per (allocated type <-> metadata) pair), there is no per-allocation cost.
pub trait TypeMeta {
    type TypeMetadata: 'static;

    const TYPE_METADATA: &'static Self::TypeMetadata;
}

/// A simple implementation of [`TypeMeta`] that does not store any type-level metadata (only the
/// `()` (unit) type).
pub struct UnitTypeMeta;

impl TypeMeta for UnitTypeMeta {
    type TypeMetadata = ();

    const TYPE_METADATA: &'static Self::TypeMetadata = &();
}

/// A trait to describe pointers to unsized values and the *per-value* metadata stored in the GC
/// header.
///
/// This trait is necessary to implement for the garbage collector to drop unsized values, and can
/// be used to convert `Gc` pointers into a "thin" representation.
///
/// The [`PtrMeta::Metadata`] value will be stored next to the allocated value in memory, so there
/// is a *per-allocation* cost.
///
/// # Safety
///
/// Though it is not unsafe to implement this trait, it is unsafe to construct a new `Gc` pointer
/// with an arbitrary implementation of `PtrMeta` and you must assert that it is implemented
/// correctly when doing so.
///
/// If `from_raw_parts` is given `thin` and `ptr_meta` that came from a call to `to_raw_parts`, it
/// must return a valid and dereferencable pointer.
pub trait PtrMeta<T: ?Sized, M> {
    type PtrMetadata: Copy + Send;
    type Thin;

    fn to_raw_parts(type_meta: &'static M, fat: *const T)
    -> (*const Self::Thin, Self::PtrMetadata);
    fn from_raw_parts(
        type_meta: &'static M,
        thin: *const Self::Thin,
        ptr_meta: Self::PtrMetadata,
    ) -> *const T;
}

/// An extension of the [`PtrMeta`] trait that tells the garbage collector how to allocate and free
/// memory associated with unsized values.
///
/// # Safety
///
/// Though it is not unsafe to implement this trait, it is unsafe to construct a new `Gc` pointer
/// with an arbitrary implementation of `AllocMeta` and you must assert that it is implemented
/// correctly when doing so.
///
/// The returned layout must be of sufficient size and alignment to hold an allocated value.
pub trait AllocMeta<T: ?Sized, M>: PtrMeta<T, M> {
    fn layout(type_meta: &'static M, ptr_meta: Self::PtrMetadata) -> Option<Layout>;
}

/// A trivial implementation of [`PtrMeta`] and [`AllocMeta`] that can only allocate sized values
/// and cannot be used to convert a "fat" pointer to a "thin" one.
///
/// In this representation, the `PtrMeta::Metadata` type is `()` (unit) and the "fat" `*const T`
/// pointer is the same as the "thin" `*const PtrMeta::Thin` pointer.
///
/// It is always safe to create or cast to a `Gc` with this `PtrMeta` implementation, because it
/// does no pointer conversion, and assumes nothing about the per-type or per-value metadata.
pub struct UnitPtrMeta;

impl<T, M> PtrMeta<T, M> for UnitPtrMeta {
    type PtrMetadata = ();
    type Thin = T;

    #[inline]
    fn to_raw_parts(_type_meta: &M, fat: *const T) -> (*const T, Self::PtrMetadata) {
        (fat, ())
    }

    #[inline]
    fn from_raw_parts(_type_meta: &M, thin: *const T, _ptr_meta: Self::PtrMetadata) -> *const T {
        thin
    }
}

impl<T, M> AllocMeta<T, M> for UnitPtrMeta {
    #[inline]
    fn layout(_type_meta: &M, _ptr_meta: ()) -> Option<Layout> {
        Some(Layout::new::<T>())
    }
}
