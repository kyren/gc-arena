use core::alloc::Layout;

/// A trait which can instantiate per-type metadata for `Gc` pointers.
///
/// Types implementing this trait are what *instantiates* the per-type metadata, so that different
/// instances of this can be used for the same type of `Gc` pointer.
///
/// The metadata pointer for allocated `Gc` values will be stored in a *per-type* static vtable (one
/// vtable per (allocated type <-> metadata) pair), there is no per-allocation cost.
pub trait TypeMeta {
    type TypeMetadata: 'static;

    const TYPE_METADATA: &'static Self::TypeMetadata;
}

/// A trivial implementation of [`TypeMeta`] that sets the per-type metadata to `()` (unit).
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
pub trait PtrMeta<T: ?Sized, M> {
    type PtrMetadata: Copy + Send;
    type Thin;

    /// This method should return the "thin" version of the given "fat" pointer.
    ///
    /// The returned "thin" pointer must have the same address as the "fat" one, and also be
    /// valid and dereferencable.
    fn to_thin(type_meta: &'static M, fat: *const T) -> *const Self::Thin;

    /// This method should return the "fat" version of the given "thin" pointer.
    ///
    /// The returned "fat" pointer must have the same address as the provided "thin" one, and
    /// additionally, round-tripping through `to_thin` and `from_thin` must result in the same
    /// exact pointer.
    fn from_thin(
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

/// A trivial implementation of [`PtrMeta`] and [`AllocMeta`] that sets the per-value metadata to
/// `()` (unit).
///
/// This type only implements [`PtrMeta`] and [`AllocMeta`] for *sized* types. This means that it
/// can only allocate and free sized types and it cannot be used to convert a "fat" pointer to a
/// "thin" one.
///
/// It is always safe to create or cast to a `Gc` with this `PtrMeta` implementation, because it
/// cannot do pointer conversion and assumes nothing about the per-type or per-value metadata.
pub struct UnitPtrMeta;

impl<T, M> PtrMeta<T, M> for UnitPtrMeta {
    type PtrMetadata = ();
    type Thin = T;

    #[inline]
    fn to_thin(_type_meta: &M, fat: *const T) -> *const T {
        fat
    }

    #[inline]
    fn from_thin(_type_meta: &M, thin: *const T, _ptr_meta: Self::PtrMetadata) -> *const T {
        thin
    }
}

impl<T, M> AllocMeta<T, M> for UnitPtrMeta {
    #[inline]
    fn layout(_type_meta: &M, _ptr_meta: ()) -> Option<Layout> {
        Some(Layout::new::<T>())
    }
}
