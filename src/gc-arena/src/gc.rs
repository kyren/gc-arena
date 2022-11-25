use core::alloc::Layout;
use core::cell::UnsafeCell;
use core::fmt::{self, Debug, Display, Pointer};
use core::marker::PhantomData;
use core::mem::align_of;
use core::ops::Deref;
use core::ptr::{self, NonNull};

use crate::collect::Collect;
use crate::context::{CollectionContext, MutationContext};
use crate::gc_weak::GcWeak;
use crate::layout_polyfill;
use crate::types::{GcBox, Invariant};

/// A garbage collected pointer to a type T.  Implements Copy, and is implemented as a plain machine
/// pointer.  You can only allocate `Gc` pointers through an `Allocator` inside an arena type, and
/// through "generativity" such `Gc` pointers may not escape the arena they were born in or be
/// stored inside TLS.  This, combined with correct `Collect` implementations, means that `Gc`
/// pointers will never be dangling and are always safe to access.
pub struct Gc<'gc, T: 'gc + Collect> {
    pub(crate) ptr: NonNull<GcBox<T>>,
    _invariant: Invariant<'gc>,
}

impl<'gc, T: 'gc + Collect + Debug> Debug for Gc<'gc, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&**self, fmt)
    }
}

impl<'gc, T: 'gc + Collect> Pointer for Gc<'gc, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Pointer::fmt(&Gc::as_ptr(*self), fmt)
    }
}

impl<'gc, T: 'gc + Collect + Display> Display for Gc<'gc, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&**self, fmt)
    }
}

impl<'gc, T: Collect + 'gc> Copy for Gc<'gc, T> {}

impl<'gc, T: Collect + 'gc> Clone for Gc<'gc, T> {
    fn clone(&self) -> Gc<'gc, T> {
        *self
    }
}

unsafe impl<'gc, T: 'gc + Collect> Collect for Gc<'gc, T> {
    fn trace(&self, cc: CollectionContext) {
        unsafe {
            cc.trace(self.ptr);
        }
    }
}

impl<'gc, T: Collect + 'gc> Deref for Gc<'gc, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.ptr.as_ref().value.get() }
    }
}

impl<'gc, T: 'gc + Collect> Gc<'gc, T> {
    pub fn allocate(mc: MutationContext<'gc, '_>, t: T) -> Gc<'gc, T> {
        unsafe { Self::from_inner(mc.allocate(t)) }
    }

    pub fn downgrade(this: Gc<'gc, T>) -> GcWeak<'gc, T> {
        GcWeak { inner: this }
    }

    /// When implementing `Collect` on types with internal mutability containing `Gc` pointers, this
    /// method must be used to ensure safe mutability.  Safe to call, but only necessary from unsafe
    /// code.
    pub fn write_barrier(mc: MutationContext<'gc, '_>, gc: Self) {
        unsafe {
            mc.write_barrier(gc.ptr);
        }
    }

    pub fn ptr_eq(this: Gc<'gc, T>, other: Gc<'gc, T>) -> bool {
        Gc::as_ptr(this) == Gc::as_ptr(other)
    }

    pub fn as_ptr(gc: Gc<'gc, T>) -> *const T {
        let ptr: *mut GcBox<T> = NonNull::as_ptr(gc.ptr);

        // SAFETY: This cannot go through Deref::deref or create temporary references
        // because stacked borrows requires that any new (temporary) shared ref
        // that gets created from a raw pointer invalidates any previous
        // tag associated with that pointer. So, this gets a pointer to the
        // underlying value without creating any temporary references.
        unsafe { UnsafeCell::raw_get(ptr::addr_of!((*ptr).value) as _) }
    }

    unsafe fn from_inner(ptr: NonNull<GcBox<T>>) -> Self {
        Self {
            ptr,
            _invariant: PhantomData,
        }
    }

    unsafe fn from_ptr(ptr: *mut GcBox<T>) -> Self {
        unsafe { Self::from_inner(NonNull::new_unchecked(ptr)) }
    }

    /// Construct a [Gc] from a raw pointer
    ///
    /// # Safety
    ///
    /// The raw pointer passed in must come from a call to [Gc::as_ptr]
    pub unsafe fn from_raw(ptr: *const T) -> Self {
        let offset = data_offset::<T>();

        let raw = ptr.cast::<u8>();

        // Reverse the offset to find the original GcBox.
        let gc_ptr = unsafe { raw.sub(offset) as *mut GcBox<T> };

        unsafe { Self::from_ptr(gc_ptr) }
    }
}

/// Get the offset within a `GcBox` for the payload behind a pointer.
fn data_offset<T>() -> usize {
    // Calculates the aligned offset of the value contained within a GcBox.
    // Because GcBox is repr(C), the contained value will always be the last field in memory.
    let layout = Layout::new::<GcBox<()>>();
    layout.size() + layout_polyfill::layout_padding_needed_for(&layout, align_of::<T>())
}
