use core::{
    borrow::Borrow,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    ops::Deref,
};

use crate::{
    barrier::{Unlock, Write},
    collect::{Collect, Trace},
    context::{Finalization, Mutation},
    gc_ptr::GcPtr,
    gc_weak::GcWeak,
    meta::{AllocMeta, DefaultPtrKind, Fat, PtrMeta, SizedPtrMeta, Thin, TypeMeta, UnitTypeMeta},
    static_wrapper::{Static, StaticPtrMeta},
    types::{GcColor, Invariant},
};

/// A garbage collected pointer to a type T.
///
/// This type is `Copy` and is implemented as a plain machine pointer to `T`.
///
/// You can only allocate `Gc` pointers through a `&Mutation<'gc>` inside an arena type, and through
/// "generativity" such `Gc` pointers may not escape the arena they were born in or be stored inside
/// TLS. This, combined with correct `Collect` implementations, means that `Gc` pointers will never
/// be dangling and are always safe to access.
pub struct Gc<'gc, T: ?Sized + 'gc, K = DefaultPtrKind, M = ()>
where
    K: GcStore<'gc, T>,
{
    pub(crate) ptr: GcPtr<K::Store>,
    _marker: PhantomData<(Invariant<'gc>, K, M)>,
}

#[allow(private_interfaces)]
pub unsafe trait GcStore<'gc, T: ?Sized> {
    type Store: ?Sized + 'gc;

    fn from_store(store_ptr: GcPtr<Self::Store>) -> GcPtr<T>;
    fn to_store(ptr: GcPtr<T>) -> GcPtr<Self::Store>;
}

#[allow(private_interfaces)]
unsafe impl<'gc, T: ?Sized + 'gc, P> GcStore<'gc, T> for Fat<P> {
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

#[allow(private_interfaces)]
unsafe impl<'gc, T: ?Sized + 'gc, P> GcStore<'gc, T> for Thin<P>
where
    P: PtrMeta<T>,
    P::Thin: 'gc,
{
    type Store = P::Thin;

    #[inline(always)]
    fn from_store(store_ptr: GcPtr<Self::Store>) -> GcPtr<T> {
        unsafe { store_ptr.fat_ptr::<T, P>() }
    }

    #[inline(always)]
    fn to_store(ptr: GcPtr<T>) -> GcPtr<Self::Store> {
        unsafe { ptr.thin_ptr::<P>() }
    }
}

impl<'gc, T: ?Sized, K, M> fmt::Pointer for Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Pointer::fmt(&Gc::as_ptr(*self), fmt)
    }
}

impl<'gc, T: ?Sized, K, M> fmt::Debug for Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
    T: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self.as_ref(), fmt)
    }
}

impl<'gc, T: ?Sized, K, M> fmt::Display for Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
    T: fmt::Display,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self.as_ref(), fmt)
    }
}

impl<'gc, T: ?Sized, K, M> Copy for Gc<'gc, T, K, M> where K: GcStore<'gc, T> {}

impl<'gc, T: ?Sized, K, M> Clone for Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
{
    #[inline]
    fn clone(&self) -> Gc<'gc, T, K, M> {
        *self
    }
}

unsafe impl<'gc, T: ?Sized, K, M> Collect<'gc> for Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
{
    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        cc.trace_gc(Self::erase(*self))
    }
}

impl<'gc, T: ?Sized, K, M> AsRef<T> for Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
{
    #[inline]
    fn as_ref(&self) -> &T {
        unsafe { K::from_store(self.ptr).as_ref() }
    }
}

impl<'gc, T: ?Sized, K, M> Deref for Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
{
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<'gc, T: ?Sized, K, M> Borrow<T> for Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
{
    #[inline]
    fn borrow(&self) -> &T {
        self.as_ref()
    }
}

impl<'gc, T: ?Sized, P, M> Gc<'gc, T, Fat<P>, M>
where
    T: Collect<'gc>,
    P: AllocMeta<T>,
{
    #[inline]
    unsafe fn allocate<TM: TypeMeta<Metadata = M>>(
        mc: &Mutation<'gc>,
        ptr_meta: P::Metadata,
    ) -> Self {
        Gc {
            ptr: mc.allocate::<T, P, TM>(ptr_meta),
            _marker: PhantomData,
        }
    }
}

impl<'gc, T: Collect<'gc>> Gc<'gc, T> {
    #[inline]
    pub fn new(mc: &Mutation<'gc>, t: T) -> Gc<'gc, T> {
        GcBuilder::allocate(mc).write(t)
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
        GcBuilder::allocate(mc).unwrap_static().write(t)
    }
}

impl<'gc, T: ?Sized, K, M> Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
{
    #[inline]
    pub fn downgrade(this: Gc<'gc, T, K, M>) -> GcWeak<'gc, T, K, M> {
        GcWeak { inner: this }
    }

    /// Returns true when a pointer is *dead* during finalization. This is equivalent to
    /// `GcWeak::is_dead` for strong pointers.
    ///
    /// Any strong pointer reachable from the root will never be dead, BUT there can be strong
    /// pointers reachable only through other weak pointers that can be dead.
    #[inline]
    pub fn is_dead(_: &Finalization<'gc>, gc: Gc<'gc, T, K, M>) -> bool {
        matches!(gc.ptr.header().color(), GcColor::White | GcColor::WhiteWeak)
    }

    /// Manually marks a dead `Gc` pointer as reachable and keeps it alive.
    ///
    /// Equivalent to `GcWeak::resurrect` for strong pointers. Manually marks this pointer and
    /// all transitively held pointers as reachable, thus keeping them from being dropped this
    /// collection cycle.
    #[inline]
    pub fn resurrect(fc: &Finalization<'gc>, gc: Gc<'gc, T, K, M>) {
        fc.resurrect(gc.ptr.erase());
    }

    #[inline]
    pub fn as_ptr(gc: Gc<'gc, T, K, M>) -> *const T {
        K::from_store(gc.ptr).as_ptr()
    }

    #[inline]
    pub unsafe fn from_ptr_cast_meta(ptr: *const T) -> Gc<'gc, T, K, M> {
        unsafe {
            Gc {
                ptr: K::to_store(GcPtr::from_ptr(ptr)),
                _marker: PhantomData,
            }
        }
    }

    /// Returns true if two `Gc`s point to the same allocation.
    ///
    /// Similarly to `Rc::ptr_eq` and `Arc::ptr_eq`, this function ignores the metadata of `dyn`
    /// pointers.
    #[inline]
    pub fn ptr_eq(this: Gc<'gc, T, K, M>, other: Gc<'gc, T, K, M>) -> bool {
        this.ptr.addr_eq(other.ptr)
    }

    /// Cast a `Gc` to the unit type.
    ///
    /// This is exactly the same as `unsafe { Gc::cast::<()>(this) }`, but we can provide this
    /// method safely because it is always safe to dereference a `*mut ()` that has come from
    /// casting a `*mut T`.
    #[inline]
    pub fn erase(this: Gc<'gc, T, K, M>) -> Gc<'gc, ()> {
        Gc::<'gc, ()> {
            ptr: unsafe { GcPtr::from_ptr(GcPtr::as_ptr(this.ptr) as *const ()) },
            _marker: PhantomData,
        }
    }
}

impl<'gc, T: ?Sized, M> Gc<'gc, T, DefaultPtrKind, M> {
    /// Cast a `Gc` pointer to a different type.
    ///
    /// # Safety
    ///
    /// It must be valid to dereference a `*mut U` that has come from casting a `*mut T`.
    #[inline]
    pub unsafe fn cast<U: 'gc>(
        this: Gc<'gc, T, DefaultPtrKind, M>,
    ) -> Gc<'gc, U, DefaultPtrKind, M> {
        Gc::<'gc, U, DefaultPtrKind, M> {
            ptr: unsafe { GcPtr::from_ptr(GcPtr::as_ptr(this.ptr) as *const U) },
            _marker: PhantomData,
        }
    }
}

impl<'gc, T: ?Sized> Gc<'gc, T> {
    /// Retrieve a `Gc` from a raw pointer obtained from [`Gc::as_ptr`].
    ///
    /// # Safety
    ///
    /// The provided pointer must have been obtained from `Gc::as_ptr`, must not have been collected
    /// yet, and must be dereferencable as its current type.
    #[inline]
    pub unsafe fn from_ptr(ptr: *const T) -> Gc<'gc, T> {
        unsafe {
            Gc {
                ptr: GcPtr::from_ptr(ptr),
                _marker: PhantomData,
            }
        }
    }
}

impl<'gc, T: ?Sized, K, M> Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
    T: Unlock,
{
    /// Shorthand for [`Gc::write`]`(mc, self).`[`unlock()`](Write::unlock).
    #[inline]
    pub fn unlock(self, mc: &Mutation<'gc>) -> &'gc <T as Unlock>::Unlocked {
        Gc::write(mc, self).unlock()
    }
}

impl<'gc, T: ?Sized, K, M> Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
{
    /// Obtains a long-lived reference to the contents of this `Gc`.
    ///
    /// Unlike `AsRef` or `Deref`, the returned reference isn't bound to the `Gc` itself, and
    /// will stay valid for the entirety of the current arena callback.
    #[inline]
    pub fn as_ref(self: Gc<'gc, T, K, M>) -> &'gc T {
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

impl<'gc, T: ?Sized, P, M> Gc<'gc, T, Fat<P>, M>
where
    P: PtrMeta<T>,
    P::Thin: 'static,
{
    pub fn as_thin(gc: Self) -> Gc<'gc, T, Thin<P>, M> {
        unsafe { Gc::from_ptr_cast_meta(gc.ptr.as_ptr()) }
    }
}

impl<'gc, T: ?Sized, K, M> Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
{
    pub fn type_metadata(gc: Gc<'gc, T, K, M>) -> &'static M {
        unsafe { gc.ptr.type_metadata::<M>() }
    }
}

impl<'gc, T: ?Sized, K, M> PartialEq for Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(other.as_ref())
    }

    fn ne(&self, other: &Self) -> bool {
        self.as_ref().ne(other.as_ref())
    }
}

impl<'gc, T: ?Sized, K, M> Eq for Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
    T: Eq,
{
}

impl<'gc, T: ?Sized, K, M> PartialOrd for Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
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

impl<'gc, T: ?Sized, K, M> Ord for Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
    T: Ord,
{
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl<'gc, T: ?Sized, K, M> Hash for Gc<'gc, T, K, M>
where
    K: GcStore<'gc, T>,
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

pub struct GcBuilder<'gc, T: ?Sized, P = SizedPtrMeta, M = ()> {
    gc: Gc<'gc, T, Fat<P>, M>,
}

impl<'gc, T: Collect<'gc>> GcBuilder<'gc, T> {
    #[inline]
    pub fn allocate(mc: &Mutation<'gc>) -> Self {
        GcBuilder::allocate_with_type_meta::<UnitTypeMeta>(mc)
    }
}

impl<'gc, T, M> GcBuilder<'gc, T, SizedPtrMeta, M>
where
    T: Collect<'gc>,
{
    #[inline]
    pub fn allocate_with_type_meta<TM: TypeMeta<Metadata = M>>(mc: &Mutation<'gc>) -> Self {
        Self::allocate_with_all_meta::<TM>(mc, ())
    }
}

impl<'gc, T: ?Sized, P, M> GcBuilder<'gc, T, P, M>
where
    T: Collect<'gc>,
    P: AllocMeta<T>,
{
    #[inline]
    pub fn allocate_with_all_meta<TM: TypeMeta<Metadata = M>>(
        mc: &Mutation<'gc>,
        ptr_meta: P::Metadata,
    ) -> Self {
        GcBuilder {
            gc: unsafe { Gc::allocate::<TM>(mc, ptr_meta) },
        }
    }
}

impl<'gc, T: ?Sized, M> GcBuilder<'gc, Static<T>, SizedPtrMeta, M> {
    #[inline]
    pub fn unwrap_static(self) -> GcBuilder<'gc, T, SizedPtrMeta, M> {
        unsafe { GcBuilder::from_raw(self.into_raw() as *mut T) }
    }
}

impl<'gc, T: ?Sized, P, M> GcBuilder<'gc, Static<T>, StaticPtrMeta<P>, M> {
    #[inline]
    pub fn unwrap_static(self) -> GcBuilder<'gc, T, P, M> {
        unsafe { GcBuilder::from_raw(self.into_raw() as *mut T) }
    }
}

impl<'gc, T: ?Sized, P, M> GcBuilder<'gc, T, P, M> {
    pub fn as_ptr(&self) -> *const T {
        Gc::as_ptr(self.gc)
    }

    pub fn as_mut_ptr(&mut self) -> *mut T {
        Gc::as_ptr(self.gc).cast_mut()
    }

    pub fn into_raw(self) -> *mut T {
        Gc::as_ptr(self.gc).cast_mut()
    }

    pub unsafe fn from_raw(ptr: *mut T) -> GcBuilder<'gc, T, P, M> {
        unsafe {
            Self {
                gc: Gc::from_ptr_cast_meta(ptr),
            }
        }
    }

    pub unsafe fn assume_init(self) -> Gc<'gc, T, Fat<P>, M> {
        self.gc.ptr.header().set_live(true);
        self.gc
    }
}

impl<'gc, T, P, M> GcBuilder<'gc, T, P, M> {
    #[inline]
    pub fn write(mut self, val: T) -> Gc<'gc, T, Fat<P>, M> {
        unsafe {
            self.as_mut_ptr().write(val);
            self.assume_init()
        }
    }
}
