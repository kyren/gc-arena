use core::cell::{BorrowError, BorrowMutError, Ref, RefCell, RefMut};
use core::fmt::{self, Debug, Pointer};

use crate::collect::Collect;
use crate::context::{CollectionContext, MutationContext};
use crate::gc::Gc;
use crate::GcWeakCell;

/// A garbage collected pointer to a type T that may be safely mutated.  When a type that may hold
/// `Gc` pointers is mutated, it may adopt new `Gc` pointers, and in order for this to be safe this
/// must be accompanied by a call to `Gc::write_barrier`.  This type wraps the given `T` in a
/// `RefCell` in such a way that writing to the `RefCell` is always accompanied by a call to
/// `Gc::write_barrier`.
pub struct GcCell<'gc, T: 'gc + Collect>(pub(crate) Gc<'gc, GcRefCell<T>>);

impl<'gc, T: Collect + 'gc> Copy for GcCell<'gc, T> {}

impl<'gc, T: Collect + 'gc> Clone for GcCell<'gc, T> {
    fn clone(&self) -> GcCell<'gc, T> {
        *self
    }
}

impl<'gc, T: 'gc + Collect + Debug> Debug for GcCell<'gc, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.try_read() {
            Ok(borrow) => fmt.debug_struct("GcCell").field("value", &borrow).finish(),
            Err(_) => {
                // The GcCell is mutably borrowed so we can't look at its value
                // here. Show a placeholder instead.
                struct BorrowedPlaceholder;

                impl Debug for BorrowedPlaceholder {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        f.write_str("<borrowed>")
                    }
                }

                fmt.debug_struct("GcCell")
                    .field("value", &BorrowedPlaceholder)
                    .finish()
            }
        }
    }
}

impl<'gc, T: 'gc + Collect> Pointer for GcCell<'gc, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Pointer::fmt(&self.as_ptr(), fmt)
    }
}

unsafe impl<'gc, T: 'gc + Collect> Collect for GcCell<'gc, T> {
    fn trace(&self, cc: CollectionContext) {
        self.0.trace(cc)
    }
}

impl<'gc, T: 'gc + Collect> GcCell<'gc, T> {
    pub fn allocate(mc: MutationContext<'gc, '_>, t: T) -> GcCell<'gc, T> {
        GcCell(Gc::allocate(
            mc,
            GcRefCell {
                cell: RefCell::new(t),
            },
        ))
    }

    pub fn downgrade(this: GcCell<'gc, T>) -> GcWeakCell<'gc, T> {
        GcWeakCell { inner: this }
    }

    pub fn ptr_eq(this: GcCell<'gc, T>, other: GcCell<'gc, T>) -> bool {
        this.as_ptr() == other.as_ptr()
    }

    pub fn as_ptr(self) -> *mut T {
        self.0.cell.as_ptr()
    }

    #[track_caller]
    pub fn read<'a>(&'a self) -> Ref<'a, T> {
        self.0.cell.borrow()
    }

    pub fn try_read<'a>(&'a self) -> Result<Ref<'a, T>, BorrowError> {
        self.0.cell.try_borrow()
    }

    #[track_caller]
    pub fn write<'a>(&'a self, mc: MutationContext<'gc, '_>) -> RefMut<'a, T> {
        let b = self.0.cell.borrow_mut();
        Gc::write_barrier(mc, self.0);
        b
    }

    pub fn try_write<'a>(
        &'a self,
        mc: MutationContext<'gc, '_>,
    ) -> Result<RefMut<'a, T>, BorrowMutError> {
        let mb = self.0.cell.try_borrow_mut()?;
        Gc::write_barrier(mc, self.0);
        Ok(mb)
    }
}

pub(crate) struct GcRefCell<T: Collect> {
    cell: RefCell<T>,
}

unsafe impl<'gc, T: Collect + 'gc> Collect for GcRefCell<T> {
    fn trace(&self, cc: CollectionContext) {
        self.cell.borrow().trace(cc);
    }
}
