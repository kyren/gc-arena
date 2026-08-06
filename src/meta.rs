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
    type TypeMetadata: 'static;
    type PtrMetadata: Copy;
    type Thin;

    fn layout(metadata: Self::PtrMetadata) -> Option<Layout>;
    fn to_raw_parts(fat: *const T) -> (*const Self::Thin, Self::PtrMetadata);
    fn from_raw_parts(thin: *const Self::Thin, metadata: Self::PtrMetadata) -> *const T;
}

pub struct SizedPtrMeta<M = ()>(PhantomData<M>);

unsafe impl<T, M: 'static> PtrMeta<T> for SizedPtrMeta<M> {
    type TypeMetadata = M;
    type PtrMetadata = ();
    type Thin = T;

    #[inline]
    fn layout(_metadata: ()) -> Option<Layout> {
        Some(Layout::new::<T>())
    }

    #[inline]
    fn to_raw_parts(fat: *const T) -> (*const T, Self::PtrMetadata) {
        (fat, ())
    }

    #[inline]
    fn from_raw_parts(thin: *const T, _metadata: Self::PtrMetadata) -> *const T {
        thin
    }
}

pub struct Fat<M>(PhantomData<M>);

pub struct Thin<M>(PhantomData<M>);

pub type DefaultPtrKind<M = ()> = Fat<SizedPtrMeta<M>>;
