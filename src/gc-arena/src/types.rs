use core::cell::Cell;
use core::marker::PhantomData;
use core::mem;
use core::ptr::NonNull;

use alloc::boxed::Box;

use crate::collect::Collect;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub(crate) enum GcColor {
    /// An object that has not yet been reached by tracing (if we're in a tracing phase).
    ///
    /// During `Phase::Sweep`, we will free all white objects
    /// that existed *before* the start of the current `Phase::Sweep`.
    /// Objects allocated during `Phase::Sweep` will be white, but will
    /// not be freed.
    White,
    /// An object reachable from a Black object, but that has not
    /// yet been traced using `Collect::trace`. We also mark black
    /// objects as gray during `Phase::Propagate` in response to a
    /// `write_barrier` call, so that we re-trace and find any objects
    /// newly reachable from the mutated object.
    Gray,
    /// An object that was reached during tracing. It will not be freed
    /// during `Phase::Sweep`. At the end of `Phase::Sweep`, all black
    /// objects will be reset to white.
    Black,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct GcBoxPtr(NonNull<GcBox<dyn Collect>>);

impl GcBoxPtr {
    #[inline(always)]
    pub(crate) unsafe fn erase<'gc, T: Collect + 'gc>(ptr: NonNull<GcBox<T>>) -> Self {
        let erased: *mut GcBox<dyn Collect + 'gc> = ptr.as_ptr();
        let erased: *mut GcBox<dyn Collect + 'static> = mem::transmute(erased);
        Self(NonNull::new_unchecked(erased))
    }

    #[inline(always)]
    pub(crate) fn flags(&self) -> &GcFlags {
        unsafe { &self.0.as_ref().flags }
    }

    #[inline(always)]
    pub(crate) fn next(&self) -> &Cell<Option<GcBoxPtr>> {
        unsafe { &self.0.as_ref().next }
    }

    #[inline(always)]
    pub(crate) fn size_of_box(&self) -> usize {
        let erased = unsafe { self.0.as_ref() };
        mem::size_of_val(erased)
    }

    #[inline(always)]
    pub(crate) unsafe fn trace_value(&self, cc: crate::CollectionContext) {
        self.0.as_ref().value().trace(cc)
    }

    #[inline(always)]
    pub(crate) unsafe fn drop_in_place(&mut self) {
        // We get interior mutability "for free" thanks to the raw
        // pointer indirection.
        let value = &mut self.0.as_mut().value;
        mem::ManuallyDrop::drop(value);
    }

    #[inline(always)]
    pub(crate) unsafe fn dealloc(self) {
        let _ = Box::from_raw(self.0.as_ptr());
    }
}

pub(crate) struct GcBox<T: Collect + ?Sized> {
    flags: GcFlags,
    next: Cell<Option<GcBoxPtr>>,
    value: mem::ManuallyDrop<T>,
}

impl<T: Collect + ?Sized> GcBox<T> {
    #[inline(always)]
    pub(crate) fn new(flags: GcFlags, next: Option<GcBoxPtr>, t: T) -> Self
    where
        T: Sized,
    {
        Self {
            flags,
            next: Cell::new(next),
            value: mem::ManuallyDrop::new(t),
        }
    }

    #[inline(always)]
    pub(crate) fn value(&self) -> &T {
        &*self.value
    }
}

pub(crate) struct GcFlags(Cell<u8>);

impl GcFlags {
    #[inline]
    pub(crate) fn new() -> GcFlags {
        GcFlags(Cell::new(0))
    }

    #[inline]
    pub(crate) fn color(&self) -> GcColor {
        match self.0.get() & 0x3 {
            0x0 => GcColor::White,
            0x1 => GcColor::Gray,
            0x2 => GcColor::Black,
            // this is needed for the compiler to codegen a simple AND.
            // SAFETY: only possible extra value is 0x3,
            // and the only place where we set these bits is in set_color
            _ => unsafe { core::hint::unreachable_unchecked() },
        }
    }

    #[inline]
    pub(crate) fn set_color(&self, color: GcColor) {
        self.0.set(
            (self.0.get() & !0x3)
                | match color {
                    GcColor::White => 0x0,
                    GcColor::Gray => 0x1,
                    GcColor::Black => 0x2,
                },
        )
    }

    #[inline]
    pub(crate) fn needs_trace(&self) -> bool {
        self.0.get() & 0x4 != 0x0
    }

    /// This is `true` if we've traced a weak pointer during to this `GcBox`
    /// during the most recent `Phase::Propagate`. This is reset back to
    /// `false` during `Phase::Sweep`.
    #[inline]
    pub(crate) fn traced_weak_ref(&self) -> bool {
        self.0.get() & 0x8 != 0x0
    }

    /// Determines whether or not we've dropped the `dyn Collect` value
    /// stored in `GcBox.value`
    /// When we garbage-collect a `GcBox` that still has outstanding weak pointers,
    /// we set `alive` to false. When there are no more weak pointers remaining,
    /// we will deallocate the `GcBox`, but skip dropping the `dyn Collect` value
    /// (since we've already done it).
    #[inline]
    pub(crate) fn is_live(&self) -> bool {
        self.0.get() & 0x10 != 0x0
    }

    #[inline]
    pub(crate) fn set_needs_trace(&self, needs_trace: bool) {
        self.0
            .set((self.0.get() & !0x4) | if needs_trace { 0x4 } else { 0x0 });
    }

    #[inline]
    pub(crate) fn set_traced_weak_ref(&self, traced_weak_ref: bool) {
        self.0
            .set((self.0.get() & !0x8) | if traced_weak_ref { 0x8 } else { 0x0 });
    }

    #[inline]
    pub(crate) fn set_live(&self, alive: bool) {
        self.0
            .set((self.0.get() & !0x10) | if alive { 0x10 } else { 0x0 });
    }
}

// Phantom type that holds a lifetime and ensures that it is invariant.
pub(crate) type Invariant<'a> = PhantomData<Cell<&'a ()>>;
