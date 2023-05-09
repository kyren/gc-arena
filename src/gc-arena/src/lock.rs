//! GC-aware interior mutability types.

use core::{
    cell::{BorrowError, BorrowMutError, Cell, Ref, RefCell, RefMut},
    cmp::Ordering,
    fmt,
};

use crate::{Collect, CollectionContext, Gc, MutationContext};

/// Types that support additional operations (typically, mutation) when behind a write barrier.
pub trait Unlock {
    /// This will typically be a cell-like type providing some sort of interior mutability.
    type Unlocked: ?Sized;

    /// Provides unsafe access to the unlocked type, *without* triggering a write barrier.
    ///
    /// # Safety
    ///
    /// In order to maintain the invariants of the garbage collector, no new `Gc` pointers
    /// may be adopted by as a result of the interior mutability afforded by the unlocked value,
    /// unless the write barrier for the containing `Gc` pointer is invoked manually before
    /// collection is triggered.
    unsafe fn unlock_unchecked(&self) -> &Self::Unlocked;
}

/// A wrapper around a `Cell` that implements `Collect`.
///
/// Only provides safe read access to the `Cell`, full write access requires unsafety.
///
/// If the `Lock` is directly held in a `Gc` pointer, safe mutable access is provided, since methods
/// on `Gc` can ensure that the write barrier is called.
#[repr(transparent)]
#[derive(Default)]
pub struct Lock<T: ?Sized> {
    cell: Cell<T>,
}

impl<T: Copy + fmt::Debug> fmt::Debug for Lock<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Lock").field(&self.cell).finish()
    }
}

impl<T> Lock<T> {
    pub fn new(t: T) -> Lock<T> {
        Self { cell: Cell::new(t) }
    }
}

impl<T: ?Sized> Lock<T> {
    pub fn as_ptr(&self) -> *mut T {
        self.cell.as_ptr()
    }

    /// Access the inner `Cell` value.
    ///
    /// SAFETY: In order to maintain the invariants of the garbage collector, no new `Gc` pointers
    /// may be adopted by this type as a result of the interior mutability afforded by directly
    /// accessing the `Cell`, unless the write barrier for the containing `Gc` pointer is invoked
    /// manually before collection is triggered.
    pub unsafe fn as_cell(&self) -> &Cell<T> {
        &self.cell
    }
}

impl<T: Copy> Lock<T> {
    pub fn get(&self) -> T {
        self.cell.get()
    }
}

impl<'gc, T: Copy + 'gc> Gc<'gc, Lock<T>> {
    pub fn get(&self) -> T {
        self.cell.get()
    }

    pub fn set(&self, mc: MutationContext<'gc, '_>, t: T) {
        self.unlock(mc).set(t);
    }
}

impl<T: ?Sized> Unlock for Lock<T> {
    type Unlocked = Cell<T>;

    unsafe fn unlock_unchecked(&self) -> &Self::Unlocked {
        &self.cell
    }
}

unsafe impl<'gc, T: Collect + Copy + 'gc> Collect for Lock<T> {
    fn trace(&self, cc: CollectionContext) {
        // Okay, so this calls `T::trace` on a *copy* of `T`.
        //
        // This is theoretically a correctness issue, because technically `T` could have interior
        // mutability and modify the copy, and this modification would be lost.
        //
        // However, currently there is not a type in rust that allows for interior mutability that
        // is also `Copy`, so this *currently* impossible to even observe.
        //
        // I am assured that this requirement is technially "only" a lint, and could be relaxed in
        // the future. If this requirement is ever relaxed in some way, fixing this is relatively
        // easy, by setting the value of the cell to the copy we make, after tracing (via a drop
        // guard in case of panics). Additionally, this is not a safety issue, only a correctness
        // issue, the changes will "just" be lost after this call returns.
        //
        // It could be fixed now, but since it is not even testable because it is currently
        // *impossible*, I did not bother. One day this may need to be implemented!
        T::trace(&self.get(), cc);
    }
}

impl<T> From<T> for Lock<T> {
    fn from(t: T) -> Self {
        Self::new(t)
    }
}

impl<T> From<Cell<T>> for Lock<T> {
    fn from(cell: Cell<T>) -> Self {
        Self { cell }
    }
}

// Can't use `#[derive]` because of the non-standard bounds.
impl<T: Copy> Clone for Lock<T> {
    fn clone(&self) -> Self {
        Self::new(self.get())
    }
}

// Can't use `#[derive]` because of the non-standard bounds.
impl<T: PartialEq + Copy> PartialEq for Lock<T> {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}

// Can't use `#[derive]` because of the non-standard bounds.
impl<T: Eq + Copy> Eq for Lock<T> {}

// Can't use `#[derive]` because of the non-standard bounds.
impl<T: PartialOrd + Copy> PartialOrd for Lock<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.get().partial_cmp(&other.get())
    }
}

// Can't use `#[derive]` because of the non-standard bounds.
impl<T: Ord + Copy> Ord for Lock<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.get().cmp(&other.get())
    }
}

/// A wrapper around a `RefCell` that implements `Collect`.
///
/// Only provides safe read access to the `RefCell`, full write access requires unsafety.
///
/// If the `RefLock` is directly held in a `Gc` pointer, safe mutable access is provided, since methods
/// on `Gc` can ensure that the write barrier is called.
#[repr(transparent)]
#[derive(Clone, Default, Eq, PartialEq, Ord, PartialOrd)]
pub struct RefLock<T: ?Sized> {
    cell: RefCell<T>,
}

impl<T: fmt::Debug + ?Sized> fmt::Debug for RefLock<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut fmt = fmt.debug_tuple("RefLock");
        match self.try_borrow() {
            Ok(borrow) => fmt.field(&borrow),
            Err(_) => {
                // The RefLock is mutably borrowed so we can't look at its value
                // here. Show a placeholder instead.
                struct BorrowedPlaceholder;

                impl fmt::Debug for BorrowedPlaceholder {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        f.write_str("<borrowed>")
                    }
                }

                fmt.field(&BorrowedPlaceholder)
            }
        }
        .finish()
    }
}

impl<T> RefLock<T> {
    pub fn new(t: T) -> Self {
        Self {
            cell: RefCell::new(t),
        }
    }
}

impl<T: ?Sized> RefLock<T> {
    pub fn as_ptr(&self) -> *mut T {
        self.cell.as_ptr()
    }

    #[track_caller]
    pub fn borrow<'a>(&'a self) -> Ref<'a, T> {
        self.cell.borrow()
    }

    pub fn try_borrow<'a>(&'a self) -> Result<Ref<'a, T>, BorrowError> {
        self.cell.try_borrow()
    }

    /// Access the inner `RefCell` type.
    ///
    /// SAFETY: In order to maintain the invariants of the garbage collector, no new `Gc` pointers
    /// may be adopted by this type as a result of the interior mutability afforded by directly
    /// accessing the `RefCell`, unless the write barrier for the containing `Gc` pointer is invoked
    /// manually before collection is triggered.
    pub unsafe fn as_ref_cell(&self) -> &RefCell<T> {
        &self.cell
    }
}

impl<'gc, T: ?Sized + 'gc> Gc<'gc, RefLock<T>> {
    #[track_caller]
    pub fn borrow<'a>(&'a self) -> Ref<'a, T> {
        RefLock::borrow(self)
    }

    pub fn try_borrow<'a>(&'a self) -> Result<Ref<'a, T>, BorrowError> {
        RefLock::try_borrow(self)
    }

    #[track_caller]
    pub fn borrow_mut<'a>(&'a self, mc: MutationContext<'gc, '_>) -> RefMut<'a, T> {
        self.unlock(mc).borrow_mut()
    }

    pub fn try_borrow_mut<'a>(
        &'a self,
        mc: MutationContext<'gc, '_>,
    ) -> Result<RefMut<'a, T>, BorrowMutError> {
        self.unlock(mc).try_borrow_mut()
    }
}

impl<T> From<T> for RefLock<T> {
    fn from(t: T) -> Self {
        Self::new(t)
    }
}

impl<T> From<RefCell<T>> for RefLock<T> {
    fn from(cell: RefCell<T>) -> Self {
        Self { cell }
    }
}

impl<T: ?Sized> Unlock for RefLock<T> {
    type Unlocked = RefCell<T>;

    unsafe fn unlock_unchecked(&self) -> &Self::Unlocked {
        &self.cell
    }
}

unsafe impl<'gc, T: Collect + ?Sized + 'gc> Collect for RefLock<T> {
    fn trace(&self, cc: CollectionContext) {
        self.cell.borrow().trace(cc);
    }
}
