use core::cell::{BorrowError, BorrowMutError, Ref, RefCell, RefMut};
use core::fmt::{self, Debug};
use core::ptr::NonNull;

use crate::collect::Collect;
use crate::context::{CollectionContext, MutationContext};
use crate::gc::Gc;
use crate::types::GcBox;

/// A garbage collected pointer to a type `T` that may be safely mutated.
pub type GcCell<'gc, T> = Gc<'gc, GcRefCell<'gc, T>>;

/// The `Gc` equivalent of a `RefCell`.
///
/// When a type that may hold `Gc` pointers is mutated, it may adopt new `Gc` pointers, and in
/// order for this to be safe this must be accompanied by a call to `MutationContext::write_barrier`.
/// This type wraps the given `T` in a `RefCell` in such a way that writing to the `RefCell`
/// is always accompanied by a call to `MutationContext::write_barrier`.
pub struct GcRefCell<'gc, T: Collect + 'gc> {
    /// `Gc` allocation this `GcRefCell`` belongs to
    allocation: NonNull<GcBox<dyn Collect + 'gc>>,
    cell: RefCell<T>,
}

impl<'gc, T: 'gc + Collect + Debug> Debug for GcRefCell<'gc, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.try_read() {
            Ok(borrow) => fmt
                .debug_struct("GcRefCell")
                .field("value", &borrow)
                .finish(),
            Err(_) => {
                // The GcRefCell is mutably borrowed so we can't look at its value
                // here. Show a placeholder instead.
                struct BorrowedPlaceholder;

                impl Debug for BorrowedPlaceholder {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        f.write_str("<borrowed>")
                    }
                }

                fmt.debug_struct("GcRefCell")
                    .field("value", &BorrowedPlaceholder)
                    .finish()
            }
        }
    }
}

impl<T: Collect> GcRefCell<'_, T> {
    /// Creates a new `GcRefCell` containing `t`.
    ///
    /// # Safety
    ///
    /// The caller should initialize the allocation this `GcRefCell` belongs to,
    /// as this function initializes it dangling.
    unsafe fn new(t: T) -> Self {
        GcRefCell {
            allocation: NonNull::<GcBox<()>>::dangling(),
            cell: RefCell::new(t),
        }
    }
}

impl<'gc, T: Collect + 'gc> GcRefCell<'gc, T> {
    /// Allocates a new `GcRefCell` containing `t`.
    pub fn allocate(mc: MutationContext<'gc, '_>, t: T) -> GcCell<'gc, T> {
        unsafe {
            let mut cell = mc.allocate(GcRefCell::new(t));

            cell.as_mut().value.get_mut().allocation = cell;
            Gc::from_inner(cell)
        }
    }

    #[track_caller]
    pub fn read(&self) -> Ref<'_, T> {
        self.cell.borrow()
    }

    pub fn try_read(&self) -> Result<Ref<'_, T>, BorrowError> {
        self.cell.try_borrow()
    }

    #[track_caller]
    pub fn write<'a>(&'a self, mc: MutationContext<'gc, '_>) -> RefMut<'a, T> {
        let b = self.cell.borrow_mut();
        unsafe { mc.write_barrier(self.allocation) };
        b
    }

    pub fn try_write<'a>(
        &'a self,
        mc: MutationContext<'gc, '_>,
    ) -> Result<RefMut<'a, T>, BorrowMutError> {
        let mb = self.cell.try_borrow_mut()?;
        unsafe { mc.write_barrier(self.allocation) };
        Ok(mb)
    }

    /// Returns a pointer to the wrapped `T`
    pub fn as_ptr(&self) -> *mut T {
        self.cell.as_ptr()
    }
}

unsafe impl<'gc, T: Collect + 'gc> Collect for GcRefCell<'gc, T> {
    fn trace(&self, cc: CollectionContext) {
        self.read().trace(cc);
    }
}
