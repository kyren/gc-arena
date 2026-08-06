use core::{alloc::Layout, marker::PhantomData};

pub trait TypeMeta {
    type Metadata: 'static;

    const METADATA: &'static Self::Metadata;
}

pub struct UnitTypeMeta;

impl TypeMeta for UnitTypeMeta {
    type Metadata = ();

    const METADATA: &'static Self::Metadata = &();
}

pub unsafe trait PtrMeta<T: ?Sized> {
    type Metadata: Copy;
    type Thin;

    fn to_raw_parts(fat: *const T) -> (*const Self::Thin, Self::Metadata);
    fn from_raw_parts(thin: *const Self::Thin, metadata: Self::Metadata) -> *const T;
}

pub unsafe trait AllocMeta<T: ?Sized>: PtrMeta<T> {
    fn layout(metadata: Self::Metadata) -> Option<Layout>;
}

pub struct SizedPtrMeta;

unsafe impl<T> PtrMeta<T> for SizedPtrMeta {
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

unsafe impl<T> AllocMeta<T> for SizedPtrMeta {
    #[inline]
    fn layout(_metadata: ()) -> Option<Layout> {
        Some(Layout::new::<T>())
    }
}

pub struct Fat<M>(PhantomData<M>);

pub struct Thin<M>(PhantomData<M>);

pub type DefaultPtrKind = Fat<SizedPtrMeta>;
