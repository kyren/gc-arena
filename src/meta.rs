use core::alloc::Layout;

use crate::gc_ptr::GcVtable;

#[doc(hidden)]
#[repr(transparent)]
pub struct __Vtable(pub(crate) GcVtable);

#[doc(hidden)]
pub trait __VtableProxy {
    const VTABLE: __Vtable;
}

#[doc(hidden)]
#[repr(C)]
pub struct __TypeProperties<M> {
    pub vtable: __Vtable,
    pub metadata: M,
}

/// A trait which can instantiate per-type metadata for `Gc` pointers.
///
/// Types implementing this trait are what *instantiates* the per-type metadata, so that different
/// instances of this can be used for the same type of `Gc` pointer.
///
/// The metadata pointer for allocated `Gc` values will be stored in a *per-type* static vtable (one
/// vtable per (allocated type <-> metadata) pair), there is no per-allocation cost.
///
/// Implementers of this trait must include a mechanical private implementation by including the
/// [`type_meta_const_promotion`] macro in the body of this trait impl.
pub trait TypeMeta {
    type TypeMetadata: Copy + Send + Sync + 'static;

    /// The actual metadata value. A reference to this value MUST be promotable to a `'static` for
    /// this trait impl to compile, which means that this must not contain interior mutability or
    /// destructors.
    const TYPE_METADATA: Self::TypeMetadata;

    // Why do we need this method?
    //
    // This is to allow `TYPE_METADATA` to be stored by *value* in the internal structure holding
    // static type properties, and the only way to do this is by having the implementer of this
    // trait construct this internal structure (opaquely exposed as `TypeProperties`).
    //
    // The only way to create a `&'static TypeProperties<M>` is by creating a const
    // `TypeProperties<M>` and promiting a reference to it to `'static`, which can only be done for
    // concrete `M` types. Thus, the implementation of this trait must be the one to produce this
    // static reference.
    //
    // SAFETY: This method is expected to return the *correct* vtable from the given `__VtableProxy`
    // impl for soundness. Since this method is internal and can only be implemented by macro, this
    // should be guaranteed.
    fn __type_properties<V: __VtableProxy>() -> &'static __TypeProperties<Self::TypeMetadata>;
}

#[macro_export]
macro_rules! __type_meta_const_promotion {
    () => {
        fn __type_properties<V: $crate::meta::__VtableProxy>()
        -> &'static $crate::meta::__TypeProperties<Self::TypeMetadata> {
            &$crate::meta::__TypeProperties {
                vtable: V::VTABLE,
                metadata: Self::TYPE_METADATA,
            }
        }
    };
}

/// Invocations of this macro must be included in all implementations of [`TypeMeta`], and assert
/// that a reference to the `TYPE_METADATA` value can be promoted to `'static`.
#[doc(inline)]
pub use crate::__type_meta_const_promotion as type_meta_const_promotion;

/// A trivial implementation of [`TypeMeta`] that sets the per-type metadata to `()` (unit).
pub struct UnitTypeMeta;

impl TypeMeta for UnitTypeMeta {
    type TypeMetadata = ();

    const TYPE_METADATA: () = ();

    type_meta_const_promotion!();
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
    fn to_thin(type_meta: M, fat: *const T) -> *const Self::Thin;

    /// This method should return the "fat" version of the given "thin" pointer.
    ///
    /// The returned "fat" pointer must have the same address as the provided "thin" one, and
    /// additionally, round-tripping through `to_thin` and `from_thin` must result in the same
    /// exact pointer.
    fn from_thin(type_meta: M, thin: *const Self::Thin, ptr_meta: Self::PtrMetadata) -> *const T;
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
    fn layout(type_meta: M, ptr_meta: Self::PtrMetadata) -> Option<Layout>;
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
    fn to_thin(_type_meta: M, fat: *const T) -> *const T {
        fat
    }

    #[inline]
    fn from_thin(_type_meta: M, thin: *const T, _ptr_meta: Self::PtrMetadata) -> *const T {
        thin
    }
}

impl<T, M> AllocMeta<T, M> for UnitPtrMeta {
    #[inline]
    fn layout(_type_meta: M, _ptr_meta: ()) -> Option<Layout> {
        Some(Layout::new::<T>())
    }
}
