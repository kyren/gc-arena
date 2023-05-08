use core::{
    cell::{BorrowError, BorrowMutError, Cell, Ref, RefCell, RefMut},
    fmt::{self, Debug},
};

use crate::{Collect, CollectionContext};

/// A `Cell` type that is `Collect`.
///
/// This does not trigger a write barrier on writes, so the only methods that allow writing to the
/// inner type are unsafe.
///
/// SAFETY: In order to maintain the invariants of the garbage collector, no new `Gc` pointers
/// may be adopted by this type as a result of any method that affords interior mutability, unless
/// the write barrier for the containing `Gc` pointer is invoked manually before collection is
/// triggered.
pub struct CollectCell<T: ?Sized> {
    cell: Cell<T>,
}

impl<T: Copy + Debug> fmt::Debug for CollectCell<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("CollectCell").field(&self.cell).finish()
    }
}

impl<T: Copy> Clone for CollectCell<T> {
    fn clone(&self) -> Self {
        Self {
            cell: self.cell.clone(),
        }
    }
}

impl<T: ?Sized> CollectCell<T> {
    pub fn as_ptr(&self) -> *mut T {
        self.cell.as_ptr()
    }
}

impl<T> CollectCell<T> {
    pub fn new(t: T) -> CollectCell<T> {
        Self { cell: Cell::new(t) }
    }

    /// Call `Cell::set` on the inner `Cell`.
    ///
    /// SAFETY: See [`CollectCell`]
    pub unsafe fn set(&self, t: T) {
        self.cell.set(t);
    }

    /// Call `Cell::replace` on the inner `Cell`.
    ///
    /// SAFETY: See [`CollectCell`]
    pub unsafe fn replace(&self, t: T) -> T {
        self.cell.replace(t)
    }
}

impl<T: Copy> CollectCell<T> {
    pub fn get(&self) -> T {
        self.cell.get()
    }
}

unsafe impl<'gc, T: Collect + Copy + 'gc> Collect for CollectCell<T> {
    fn trace(&self, cc: CollectionContext) {
        self.cell.get().trace(cc);
    }
}

/// A `RefCell` type that is `Collect`.
///
/// This does not trigger a write barrier on writes, so the only methods that allow writing to the
/// inner type are unsafe.
///
/// SAFETY: In order to maintain the invariants of the garbage collector, no new `Gc` pointers
/// may be adopted by this type as a result of any method that affords interior mutability, unless
/// the write barrier for the containing `Gc` pointer is invoked manually before collection is
/// triggered.
#[derive(Clone)]
pub struct CollectRefCell<T: ?Sized> {
    cell: RefCell<T>,
}

impl<T: Debug + ?Sized> Debug for CollectRefCell<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.try_borrow() {
            Ok(borrow) => fmt.debug_tuple("CollectRefCell").field(&borrow).finish(),
            Err(_) => {
                // The GcCell is mutably borrowed so we can't look at its value
                // here. Show a placeholder instead.
                struct BorrowedPlaceholder;

                impl Debug for BorrowedPlaceholder {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        f.write_str("<borrowed>")
                    }
                }

                fmt.debug_tuple("GcCell")
                    .field(&BorrowedPlaceholder)
                    .finish()
            }
        }
    }
}

impl<T> CollectRefCell<T> {
    pub fn new(t: T) -> Self {
        Self {
            cell: RefCell::new(t),
        }
    }
}

impl<T: ?Sized> CollectRefCell<T> {
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

    /// Call `RefCell::borrow_mut` on the inner `RefCell`.
    ///
    /// SAFETY: See [`CollectRefCell`]
    #[track_caller]
    pub unsafe fn borrow_mut<'a>(&'a self) -> RefMut<'a, T> {
        self.cell.borrow_mut()
    }

    /// Call `RefCell::try_borrow_mut` on the inner `RefCell` *without* the write barrier.
    ///
    /// SAFETY: See [`CollectRefCell`]
    pub unsafe fn try_borrow_mut<'a>(&'a self) -> Result<RefMut<'a, T>, BorrowMutError> {
        self.cell.try_borrow_mut()
    }
}

unsafe impl<'gc, T: Collect + ?Sized + 'gc> Collect for CollectRefCell<T> {
    fn trace(&self, cc: CollectionContext) {
        self.cell.borrow().trace(cc);
    }
}
