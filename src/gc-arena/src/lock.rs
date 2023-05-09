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

// Helper macro to factor out the common parts of locks types.
macro_rules! make_lock_wrapper {
    (
        $(#[$meta:meta])*
        locked = $locked_type:ident as $gc_locked_type:ident;
        unlocked = $unlocked_type:ident unsafe $unsafe_unlock_method:ident;
        impl Sized { $($sized_items:tt)* }
        impl ?Sized { $($unsized_items:tt)* }
    ) => {
        /// A wrapper around a [`
        #[doc = stringify!($unlocked_type)]
        /// `] that implements [`Collect`].
        ///
        /// Only provides safe read access to the wrapped [`
        #[doc = stringify!($unlocked_type)]
        /// `], full write access requires unsafety.
        ///
        /// If the `
        #[doc = stringify!($locked_type)]
        /// ` is directly held in a [`Gc`] pointer, safe mutable access is provided,
        /// since methods on [`Gc`] can ensure that the write barrier is called.
        $(#[$meta])*
        #[repr(transparent)]
        pub struct $locked_type<T: ?Sized> {
            cell: $unlocked_type<T>,
        }

        #[doc = concat!("An alias for `Gc<'gc, ", stringify!($locked_type), "<T>>`.")]
        pub type $gc_locked_type<'gc, T> = Gc<'gc, $locked_type<T>>;

        impl<T> $locked_type<T> {
            pub fn new(t: T) -> $locked_type<T> {
                Self { cell: $unlocked_type::new(t) }
            }

            pub fn into_inner(self) -> T {
                self.cell.into_inner()
            }

            $($sized_items)*
        }

        impl<T: ?Sized> $locked_type<T> {
            pub fn as_ptr(&self) -> *mut T {
                self.cell.as_ptr()
            }

            $($unsized_items)*

            #[doc = concat!("Access the wrapped [`", stringify!($unlocked_type), "`].")]
            ///
            /// # Safety
            /// In order to maintain the invariants of the garbage collector, no new [`Gc`]
            /// pointers may be adopted by this type as a result of the interior mutability
            /// afforded by directly accessing the inner [`
            #[doc = stringify!($unlocked_type)]
            /// `], unless the write barrier for the containing [`Gc`] pointer is invoked manuall
            /// before collection is triggered.
            pub unsafe fn $unsafe_unlock_method(&self) -> &$unlocked_type<T> {
                &self.cell
            }

            pub fn get_mut(&mut self) -> &mut T {
                self.cell.get_mut()
            }
        }

        impl<T: ?Sized> Unlock for $locked_type<T> {
            type Unlocked = $unlocked_type<T>;

            unsafe fn unlock_unchecked(&self) -> &Self::Unlocked {
                &self.cell
            }
        }

        impl<T> From<T> for $locked_type<T> {
            fn from(t: T) -> Self {
                Self::new(t)
            }
        }

        impl<T> From<$unlocked_type<T>> for $locked_type<T> {
            fn from(cell: $unlocked_type<T>) -> Self {
                Self { cell }
            }
        }
    };
}

make_lock_wrapper!(
    #[derive(Default)]
    locked = Lock as GcLock;
    unlocked = Cell unsafe as_cell;
    impl Sized {
        pub fn get(&self) -> T where T: Copy {
            self.cell.get()
        }

        pub fn take(&self) -> T where T: Default {
            // Despite mutating the contained value, this doesn't need a write barrier, as,
            // thanks to lifetime parametricity, the result of `Default::default()` cannot
            // ever obtain safely a `MutationContext` or other external `Gc` pointers.
            self.cell.take()
        }
    }
    impl ?Sized {}
);

impl<T: Copy + fmt::Debug> fmt::Debug for Lock<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Lock").field(&self.cell).finish()
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

make_lock_wrapper!(
    #[derive(Clone, Default, Eq, PartialEq, Ord, PartialOrd)]
    locked = RefLock as GcRefLock;
    unlocked = RefCell unsafe as_ref_cell;
    impl Sized {
        pub fn take(&self) -> T where T: Default {
            // Despite mutating the contained value, this doesn't need a write barrier,
            // as, thanks to lifetime parametricity, a `Default::default()` cannot ever
            // safely obtain a `MutationContext` or other external `Gc` pointers.
            self.cell.take()
        }
    }
    impl ?Sized {
        #[track_caller]
        pub fn borrow<'a>(&'a self) -> Ref<'a, T> {
            self.cell.borrow()
        }

        pub fn try_borrow<'a>(&'a self) -> Result<Ref<'a, T>, BorrowError> {
            self.cell.try_borrow()
        }
    }
);

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

unsafe impl<'gc, T: Collect + ?Sized + 'gc> Collect for RefLock<T> {
    fn trace(&self, cc: CollectionContext) {
        self.cell.borrow().trace(cc);
    }
}
