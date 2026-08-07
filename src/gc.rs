use core::{
    borrow::Borrow,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem,
    ops::Deref,
};

use crate::{
    barrier::{Unlock, Write},
    collect::{Collect, Trace},
    context::{Finalization, Mutation},
    gc_ptr::GcPtr,
    gc_weak::GcWeak,
    meta::{AllocMeta, PtrMeta, TypeMeta, UnitPtrMeta, UnitTypeMeta},
    static_wrapper::{Static, StaticPtrMeta},
    types::{GcColor, Invariant},
};

/// A marker type to state that a `Gc` stores a "fat" pointer to the value.
///
/// If there is a `PtrMeta<T>` implementation for the `T: ?Sized` of a `Gc`, then this pointer
/// contains compatible ptr metadata in its GC header section, and the `Gc` can automatically be
/// converted to a "thin" representation which will read this metadata from the header.
///
/// This type is used as the "default" pointer representation, it is possible to create a `Gc`
/// with an unsized `T` where a valid `PtrMeta<T>` implementation is *not* present (such as via
/// the `unsize!` macro). This means that a `Gc` cannot be automatically converted to a "thin"
/// representation, because the pointer was not allocated with the correct ptr metadata to
/// reconstruct a "fat" pointer from a "thin" one.
///
/// Similarly, if the `T` is `Sized`, then the "thin" and "fat" pointer types are the same, so
/// `Gc<T, GcKind<Fat, M, UnitPtrMeta>>` will really store a "thin sized" pointer.
pub struct Fat;

/// A marker type to state that a `Gc` stores a "thin" pointer to the value with pointer metadata
/// stored in the header section.
///
/// `Gc`s with this representation must have a valid `PtrMeta<T>` implementation and will store a
/// "thin" pointer that is automatically converted to a "fat" pointer on access.
pub struct Thin;

/// A type which describes the stored pointer type of a `Gc` along with what header metadata is
/// present
///
/// The `PtrSize` representation must be either [`Fat`] or [`Thin`] and describes whether the `Gc`
/// stores a "thin" pointer which can be reconstructed into a "fat" one.
///
/// The `TypeMeta` parameter describes the *per-type* metadata stored in the pointer's vtable.
///
/// The `PtrMeta` parameter describes the *per-value* metadata stored in the GC header.
pub struct GcKind<PtrSize, PtrMeta, TypeMeta>(PhantomData<(PtrSize, TypeMeta, PtrMeta)>);

/// A `GcKind` which stores the `T` pointer directly and contains `()` for the per-value metadata.
pub type DefaultGcKind<M = ()> = GcKind<Fat, M, UnitPtrMeta>;

/// A garbage collected pointer to a type T.
///
/// This type is `Copy` and is implemented as a plain machine pointer to `T`.
///
/// You can only allocate `Gc` pointers through a `&Mutation<'gc>` inside an arena type, and through
/// "generativity" such `Gc` pointers may not escape the arena they were born in or be stored inside
/// TLS. This, combined with correct `Collect` implementations, means that `Gc` pointers will never
/// be dangling and are always safe to access.
///
/// # Kind
///
/// `Gc` carries a `K` parameter which is expected to be a valid [`GcKind`] (implements
/// [`IsGcKind`]). This type acts as an indicator of the storage mode of the `Gc` as well as the
/// per-type and per-value metadata stored with the value.
pub struct Gc<'gc, T: ?Sized + 'gc, K = DefaultGcKind>
where
    K: IsGcKind<'gc, T>,
{
    pub(crate) ptr: GcPtr<K::Store>,
    _marker: PhantomData<(Invariant<'gc>, K)>,
}

pub type GcFat<'gc, T, M, P> = Gc<'gc, T, GcKind<Fat, M, P>>;

pub type GcThin<'gc, T, M, P> = Gc<'gc, T, GcKind<Thin, M, P>>;

/// Trait to fetch the "fat" version of the stored pointer.
pub(crate) unsafe trait GcStore<'gc, T: ?Sized> {
    type Store: ?Sized + 'gc;

    fn from_store(store_ptr: GcPtr<Self::Store>) -> GcPtr<T>;
    fn to_store(ptr: GcPtr<T>) -> GcPtr<Self::Store>;
}

unsafe impl<'gc, T: ?Sized + 'gc, M, P> GcStore<'gc, T> for GcKind<Fat, M, P> {
    type Store = T;

    #[inline(always)]
    fn from_store(store_ptr: GcPtr<Self::Store>) -> GcPtr<T> {
        store_ptr
    }

    #[inline(always)]
    fn to_store(ptr: GcPtr<T>) -> GcPtr<Self::Store> {
        ptr
    }
}

unsafe impl<'gc, T: ?Sized + 'gc, M, P> GcStore<'gc, T> for GcKind<Thin, M, P>
where
    M: 'static,
    P: PtrMeta<T, M>,
    P::Thin: 'gc,
{
    type Store = P::Thin;

    #[inline(always)]
    fn from_store(store_ptr: GcPtr<Self::Store>) -> GcPtr<T> {
        unsafe { store_ptr.fat_ptr::<T, M, P>() }
    }

    #[inline(always)]
    fn to_store(ptr: GcPtr<T>) -> GcPtr<Self::Store> {
        unsafe { ptr.thin_ptr::<M, P>() }
    }
}

/// A trait declaring that a [`GcKind`] is valid.
///
/// A `GcKind<Fat, _, _>` is always valid because the stored pointer is `*const T` (which will be
/// "thin sized" if `T: Sized`).
///
/// A `GcKind<Thin, P, _>` is only valid when `P: PtrMeta<'gc, T>` because the stored pointer is
/// `*const P::Thin` and the correct pointer metadata must be present to reconstruct the "fat"
/// version of the pointer.
#[allow(private_bounds)]
pub trait IsGcKind<'gc, T: ?Sized>: GcStore<'gc, T> {}

impl<'gc, T: ?Sized, K: GcStore<'gc, T>> IsGcKind<'gc, T> for K {}

impl<'gc, T: ?Sized, K> fmt::Pointer for Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Pointer::fmt(&Gc::as_ptr(*self), fmt)
    }
}

impl<'gc, T: ?Sized, K> fmt::Debug for Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
    T: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self.as_ref(), fmt)
    }
}

impl<'gc, T: ?Sized, K> fmt::Display for Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
    T: fmt::Display,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self.as_ref(), fmt)
    }
}

impl<'gc, T: ?Sized, K> Copy for Gc<'gc, T, K> where K: IsGcKind<'gc, T> {}

impl<'gc, T: ?Sized, K> Clone for Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
{
    #[inline]
    fn clone(&self) -> Gc<'gc, T, K> {
        *self
    }
}

unsafe impl<'gc, T: ?Sized, K> Collect<'gc> for Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
{
    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        cc.trace_gc(Self::erase(*self))
    }
}

impl<'gc, T: ?Sized, K> AsRef<T> for Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
{
    #[inline]
    fn as_ref(&self) -> &T {
        unsafe { K::from_store(self.ptr).as_ref() }
    }
}

impl<'gc, T: ?Sized, K> Deref for Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
{
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<'gc, T: ?Sized, K> Borrow<T> for Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
{
    #[inline]
    fn borrow(&self) -> &T {
        self.as_ref()
    }
}

impl<'gc, T: Collect<'gc>> Gc<'gc, T> {
    /// Allocate a new `Gc` pointer.
    #[inline]
    pub fn new(mc: &Mutation<'gc>, t: T) -> Gc<'gc, T> {
        GcBuilder::new().write(mc, t)
    }
}

impl<'gc, T: 'static> Gc<'gc, T> {
    /// Create a new `Gc` pointer from a `'static` value.
    ///
    /// This method does not require that the type `T` implement `Collect`. This uses [`Static`]
    /// internally to automatically provide a trivial `Collect` impl and is equivalent to the
    /// following code:
    ///
    /// ```rust
    /// # use gc_arena::{Gc, Static};
    /// # fn main() {
    /// # gc_arena::arena::rootless_mutate(|mc| {
    /// struct MyStaticStruct;
    /// let p = Gc::new(mc, Static(MyStaticStruct));
    /// // This is allowed because `Static` is `#[repr(transparent)]`
    /// let p: Gc<MyStaticStruct> = unsafe { Gc::cast(p) };
    /// # });
    /// # }
    /// ```
    #[inline]
    pub fn new_static(mc: &Mutation<'gc>, t: T) -> Gc<'gc, T> {
        GcBuilder::new().unwrap_static().write(mc, t)
    }
}

impl<'gc, T: ?Sized, K> Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
{
    /// Downgrade a `Gc` pointer into a [`GcWeak`] one.
    #[inline]
    pub fn downgrade(this: Self) -> GcWeak<'gc, T, K> {
        GcWeak { inner: this }
    }

    /// Returns true when a pointer is *dead* during finalization. This is equivalent to
    /// [`GcWeak::is_dead`] for strong pointers.
    ///
    /// Any strong pointer reachable from the root will never be dead, BUT there can be strong
    /// pointers reachable only through other weak pointers that can be dead.
    #[inline]
    pub fn is_dead(_: &Finalization<'gc>, gc: Gc<'gc, T, K>) -> bool {
        matches!(gc.ptr.header().color(), GcColor::White | GcColor::WhiteWeak)
    }

    /// Manually marks a dead `Gc` pointer as reachable and keeps it alive.
    ///
    /// Equivalent to [`GcWeak::resurrect`] for strong pointers. Manually marks this pointer and
    /// all transitively held pointers as reachable, thus keeping them from being dropped this
    /// collection cycle.
    #[inline]
    pub fn resurrect(fc: &Finalization<'gc>, gc: Gc<'gc, T, K>) {
        fc.resurrect(gc.ptr.erase());
    }

    /// Returns the pointer held inside the `Gc`.
    ///
    /// This always returns the "fat" version of the pointer, the same one accessed on dereference.
    #[inline]
    pub fn as_ptr(gc: Gc<'gc, T, K>) -> *const T {
        K::from_store(gc.ptr).as_ptr()
    }

    /// Retrieve a `Gc` from a raw pointer obtained from `Gc::as_ptr`
    ///
    /// # Safety
    ///
    /// The provided pointer must have been obtained from `Gc::as_ptr`, and the pointer must not
    /// have been collected yet.
    ///
    /// The `K` kind parameter may be changed here and MUST be "compatible" with the original GcKind
    /// used to allocate the `ptr`.
    ///
    /// To be "compatible" means that for both the per-type and per-value metadata types, you can
    /// dereference a (valid, dereferenceble) pointer to the old type `*const Old` as `*const New`.
    ///
    /// The implementation of `PtrMeta` within the `K` kind must also be valid and always return
    /// dereferencable fat pointers for both the existing per-type metadata (cast to its new type)
    /// and the existing per-value metadata (cast to its new type).
    #[inline]
    pub unsafe fn from_ptr_with_kind(ptr: *const T) -> Gc<'gc, T, K> {
        unsafe {
            Gc {
                ptr: K::to_store(GcPtr::from_ptr(ptr.cast_mut())),
                _marker: PhantomData,
            }
        }
    }

    /// Returns true if two `Gc`s point to the same allocation.
    ///
    /// Similarly to `Rc::ptr_eq` and `Arc::ptr_eq`, this function ignores the metadata of `dyn`
    /// pointers.
    #[inline]
    pub fn ptr_eq(this: Self, other: Self) -> bool {
        this.ptr.addr_eq(other.ptr)
    }

    /// Cast a `Gc` to the unit type.
    ///
    /// This converts the `Gc` to point to a `()`, and changes the kind to [`DefaultGcKind`].
    ///
    /// This is always safe to do as it is always safe to dereference a `*const ()` which comes from
    /// some other dereferencable pointer type and `DefaultGcKind` has `()` for both per-type and
    /// per-value metadata.
    #[inline]
    pub fn erase(this: Self) -> Gc<'gc, ()> {
        Gc {
            ptr: unsafe { GcPtr::from_ptr(GcPtr::as_ptr(this.ptr) as *mut ()) },
            _marker: PhantomData,
        }
    }
}

impl<'gc, T: ?Sized, M, P> GcFat<'gc, T, M, P> {
    /// Cast a `Gc` pointer to a different type.
    ///
    /// # Safety
    ///
    /// It must be valid to dereference a `*const U` that has come from casting a `*const T`.
    #[inline]
    pub unsafe fn cast<U: 'gc>(this: Self) -> GcFat<'gc, U, M, P> {
        Gc {
            ptr: unsafe { GcPtr::from_ptr(GcPtr::as_ptr(this.ptr) as *mut U) },
            _marker: PhantomData,
        }
    }
}

impl<'gc, T: ?Sized> Gc<'gc, T> {
    /// Retrieve a `Gc` from a raw pointer obtained from `Gc::as_ptr`.
    ///
    /// # Safety
    ///
    /// The provided pointer must have been obtained from `Gc::as_ptr`, and the pointer must not
    /// have been collected yet.
    #[inline]
    pub unsafe fn from_ptr(ptr: *const T) -> Gc<'gc, T> {
        unsafe { Gc::from_ptr_with_kind(ptr) }
    }
}

impl<'gc, T: ?Sized, K> Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
    T: Unlock,
{
    /// Shorthand for [`Gc::write`]`(mc, self).`[`unlock()`](Write::unlock).
    #[inline]
    pub fn unlock(self, mc: &Mutation<'gc>) -> &'gc <T as Unlock>::Unlocked {
        Gc::write(mc, self).unlock()
    }
}

impl<'gc, T: ?Sized, K> Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
{
    /// Obtains a long-lived reference to the contents of this `Gc`.
    ///
    /// Unlike `AsRef` or `Deref`, the returned reference isn't bound to the `Gc` itself, and
    /// will stay valid for the entirety of the current arena callback.
    #[inline]
    pub fn as_ref(self: Gc<'gc, T, K>) -> &'gc T {
        // SAFETY: The returned reference cannot escape the current arena callback, as `&'gc T`
        // never implements `Collect` (unless `'gc` is `'static`, which is impossible here), and so
        // cannot be stored inside the GC root.
        unsafe { K::from_store(self.ptr).as_ref() }
    }

    /// Triggers a write barrier on this `Gc`, allowing for safe mutation.
    ///
    /// This triggers an unrestricted *backwards* write barrier on this pointer, meaning that it is
    /// guaranteed that this pointer can safely adopt *any* arbitrary child pointers (until the next
    /// time that collection is triggered).
    ///
    /// It returns a reference to the inner `T` wrapped in a `Write` marker to allow for
    /// unrestricted mutation on the held type or any of its directly held fields.
    #[inline]
    pub fn write(mc: &Mutation<'gc>, gc: Self) -> &'gc Write<T> {
        unsafe {
            mc.backward_barrier(Gc::erase(gc), None);
            // SAFETY: the write barrier stays valid until the end of the current callback.
            Write::assume(gc.as_ref())
        }
    }
}

impl<'gc, T: ?Sized, M, P> GcFat<'gc, T, M, P>
where
    M: 'static,
    P: PtrMeta<T, M>,
    P::Thin: 'gc,
{
    /// Convert a "fat" `Gc` to a "thin" one.
    ///
    /// From the outside, the resulting `Gc` behaves identically to the "fat" representation but is
    /// the size of a "thin" pointer instead.
    ///
    /// If called on a `Gc<'gc, T>` for a `T: Sized` type, then this has no practical effect (other
    /// than changing the type of the `Gc`).
    ///
    /// # Performance
    ///
    /// This representation of pointers has performance implications vs the "fat" one. The garbage
    /// collector *must* store pointer metadata next to the value in the allocated memory for
    /// garbage collection, so it is always possible to store a thin pointer and reconstruct a fat
    /// one by combining the metadata stored in the GC header with the thin representation.
    ///
    /// But, this means that on "thin" pointers, operations like finding the length of a slice must
    /// visit the memory that the `Gc` points to rather than this metadata being available next to
    /// the pointer itself.
    pub fn as_thin(gc: Self) -> GcThin<'gc, T, M, P> {
        unsafe { Gc::from_ptr_with_kind(Gc::as_ptr(gc)) }
    }
}

impl<'gc, T: ?Sized, M, P> GcThin<'gc, T, M, P>
where
    M: 'static,
    P: PtrMeta<T, M>,
    P::Thin: 'gc,
{
    /// Convert a "thin" `Gc` to a "fat" one.
    ///
    /// This is the reverse of [`Gc::as_thin`]
    pub fn as_fat(gc: Self) -> GcFat<'gc, T, M, P> {
        unsafe { Gc::from_ptr_with_kind(Gc::as_ptr(gc)) }
    }
}

impl<'gc, T: ?Sized, S, M, P> Gc<'gc, T, GcKind<S, M, P>>
where
    GcKind<S, M, P>: IsGcKind<'gc, T>,
{
    /// Retrieve the *per-type* metadata specified when a `Gc` was allocated.
    pub fn type_metadata(gc: Gc<'gc, T, GcKind<S, M, P>>) -> &'static M {
        unsafe { gc.ptr.type_metadata::<M>() }
    }
}

impl<'gc, T: ?Sized, K> PartialEq for Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(other.as_ref())
    }

    fn ne(&self, other: &Self) -> bool {
        self.as_ref().ne(other.as_ref())
    }
}

impl<'gc, T: ?Sized, K> Eq for Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
    T: Eq,
{
}

impl<'gc, T: ?Sized, K> PartialOrd for Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
    T: PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        self.as_ref().partial_cmp(other.as_ref())
    }

    #[inline]
    fn le(&self, other: &Self) -> bool {
        self.as_ref().le(other.as_ref())
    }

    #[inline]
    fn lt(&self, other: &Self) -> bool {
        self.as_ref().lt(other.as_ref())
    }

    #[inline]
    fn ge(&self, other: &Self) -> bool {
        self.as_ref().ge(other.as_ref())
    }

    #[inline]
    fn gt(&self, other: &Self) -> bool {
        self.as_ref().gt(other.as_ref())
    }
}

impl<'gc, T: ?Sized, K> Ord for Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
    T: Ord,
{
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl<'gc, T: ?Sized, K> Hash for Gc<'gc, T, K>
where
    K: IsGcKind<'gc, T>,
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

/// A type used for more advanced ways of allocating a [`Gc`].
pub struct GcBuilder<'gc, T: ?Sized, M = (), P = UnitPtrMeta> {
    ptr: GcPtr<T>,
    _marker: PhantomData<(Invariant<'gc>, M, P)>,
}

impl<'gc, T: ?Sized, M, P> Drop for GcBuilder<'gc, T, M, P> {
    fn drop(&mut self) {
        unsafe {
            self.ptr.dealloc();
        }
    }
}

impl<'gc, T: Collect<'gc>> GcBuilder<'gc, T> {
    /// Create a new `GcBuilder` suitable for building a `Gc` pointer to a *sized* `T`.
    #[inline]
    pub fn new() -> Self {
        GcBuilder::new_with_type_meta::<UnitTypeMeta>()
    }
}

impl<'gc, T, M> GcBuilder<'gc, T, M, UnitPtrMeta>
where
    T: Collect<'gc>,
{
    /// Create a new `GcBuilder` suitable for building a `Gc` pointer to a *sized* `T` with the
    /// *per-type* metadata from `TM`.
    ///
    /// The `TM::METADATA` pointer will be stored in the static *per-type* vtable so there is no
    /// per-allocation cost, but there is one vtable per `T` <-> `TM` pair.
    #[inline]
    pub fn new_with_type_meta<TM: TypeMeta<TypeMetadata = M>>() -> Self {
        unsafe { Self::new_with_type_and_ptr_meta::<TM>(()) }
    }
}

impl<'gc, T: ?Sized, M, P> GcBuilder<'gc, T, M, P>
where
    T: Collect<'gc>,
    P: AllocMeta<T, M>,
{
    /// Create a new `GcBuilder` suitable for building a `Gc` pointing to an *unsized* `T`with the
    /// given `ptr_meta` per-value metadata and per-type metadata from `TM`.
    ///
    /// # Safety
    ///
    /// Using this method requires asserting that `P: AllocMeta` is correctly implemented for the
    /// `TM::METADATA` being used to create the `Gc`.
    #[inline]
    pub unsafe fn new_with_type_and_ptr_meta<TM: TypeMeta<TypeMetadata = M>>(
        ptr_meta: P::PtrMetadata,
    ) -> Self {
        let ptr = GcPtr::<T>::alloc::<TM, P>(ptr_meta);
        ptr.header().set_needs_trace(T::NEEDS_TRACE);

        GcBuilder {
            ptr,
            _marker: PhantomData,
        }
    }
}

impl<'gc, T: ?Sized, M> GcBuilder<'gc, Static<T>, M, UnitPtrMeta> {
    /// Safely unwrap a `GcBuilder<'gc, Static<T>>` into a `GcBuilder<'gc, T>`.
    ///
    /// This is always safe to do since a `Static<T>` has the same representation as `T`.
    ///
    /// This can be used to allocate a type wrapped in [`Static`] as a bare value.
    #[inline]
    pub fn unwrap_static(self) -> GcBuilder<'gc, T, M, UnitPtrMeta> {
        unsafe { GcBuilder::from_raw(self.into_raw() as *mut T) }
    }
}

impl<'gc, T: ?Sized, M, P> GcBuilder<'gc, Static<T>, M, StaticPtrMeta<P>> {
    /// Safely unwrap a `GcBuilder<'gc, Static<T>, StaticPtrMeta<P>>` into a `GcBuilder<'gc, T, P>`.
    ///
    /// This is always safe to do since a `Static<T>` has the same representation as `T` and
    /// `StaticPtrMeta<P>` always wraps the `P: PtrMeta` without modification.
    ///
    /// This can be used to allocate a type wrapped in [`Static`] as a bare value.
    #[inline]
    pub fn unwrap_static(self) -> GcBuilder<'gc, T, M, P> {
        unsafe { GcBuilder::from_raw(self.into_raw() as *mut T) }
    }
}

impl<'gc, T: ?Sized, M, P> GcBuilder<'gc, T, M, P> {
    /// Returns a pointer to the (possibly uninitialized) value being built.
    ///
    /// The pointer will always point to valid, aligned memory for the type `T` and may be written
    /// to to initialize the value.
    pub fn as_ptr(&mut self) -> *mut T {
        self.ptr.as_ptr()
    }

    /// Convert this `GcBuilder` into a bare pointer.
    pub fn into_raw(self) -> *mut T {
        let ptr = self.ptr;
        mem::forget(self);
        ptr.as_ptr()
    }

    /// Retrieve a `GcBuilder` from a pointer obtained from [`GcBuilder::into_raw`].
    ///
    /// # Safety
    ///
    /// The types of `T`, `P`, and `M` must be compatible with the originally constructed
    /// `GcBuilder`.
    pub unsafe fn from_raw(ptr: *mut T) -> GcBuilder<'gc, T, M, P> {
        unsafe {
            Self {
                ptr: GcPtr::from_ptr(ptr),
                _marker: PhantomData,
            }
        }
    }

    /// Finish constructing a `Gc<T>` by unsafely assuming that the held memory is properly
    /// initialized.
    pub unsafe fn assume_init(self, mc: &Mutation<'gc>) -> GcFat<'gc, T, M, P> {
        let ptr = self.ptr;
        mem::forget(self);

        ptr.header().set_live(true);
        mc.link(ptr.erase());
        Gc {
            ptr,
            _marker: PhantomData,
        }
    }
}

impl<'gc, T, M, P> GcBuilder<'gc, T, M, P> {
    /// Finish constructing a `Gc<T>` by initializing the (sized) value with `val`.
    #[inline]
    pub fn write(mut self, mc: &Mutation<'gc>, val: T) -> GcFat<'gc, T, M, P> {
        unsafe {
            self.as_ptr().write(val);
            self.assume_init(mc)
        }
    }
}
