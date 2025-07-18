//! GC-aware interior mutability types.

use core::{
    cell::{BorrowError, BorrowMutError, Cell, OnceCell, Ref, RefCell, RefMut},
    cmp::Ordering,
    fmt,
};

use crate::{
    Gc, Mutation,
    barrier::Unlock,
    collect::{Collect, Trace},
};

// Helper macro to factor out the common parts of locks types.
macro_rules! make_lock_wrapper {
    (
        $(#[$meta:meta])*
        locked = $locked_type:ident as $gc_locked_type:ident;
        unlocked = $unlocked_type:ident unsafe $unsafe_unlock_method:ident;
        $(
            impl $Sized:ident { $($sized_items:tt)* }
            impl ?Sized { $($unsized_items:tt)* }
        )?
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
        pub struct $locked_type<T $(: ?$Sized)?> {
            cell: $unlocked_type<T>,
        }

        #[doc = concat!("An alias for `Gc<'gc, ", stringify!($locked_type), "<T>>`.")]
        pub type $gc_locked_type<'gc, T> = Gc<'gc, $locked_type<T>>;

        $(
        impl<T> $locked_type<T> {
            #[inline]
            pub fn new(t: T) -> $locked_type<T> {
                Self { cell: $unlocked_type::new(t) }
            }

            #[inline]
            pub fn into_inner(self) -> T {
                self.cell.into_inner()
            }

            $($sized_items)*
        }

        impl<T: ?$Sized> $locked_type<T> {
            #[inline]
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
            /// `], unless the write barrier for the containing [`Gc`] pointer is invoked manually
            /// before collection is triggered.
            #[inline]
            pub unsafe fn $unsafe_unlock_method(&self) -> &$unlocked_type<T> {
                &self.cell
            }

            #[inline]
            pub fn get_mut(&mut self) -> &mut T {
                self.cell.get_mut()
            }
        }
        )?

        impl<T $(: ?$Sized)?> Unlock for $locked_type<T> {
            type Unlocked = $unlocked_type<T>;

            #[inline]
            unsafe fn unlock_unchecked(&self) -> &Self::Unlocked {
                &self.cell
            }
        }

        impl<T> From<T> for $locked_type<T> {
            #[inline]
            fn from(t: T) -> Self {
                Self { cell: t.into() }
            }
        }

        impl<T> From<$unlocked_type<T>> for $locked_type<T> {
            #[inline]
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
        #[inline]
        pub fn get(&self) -> T where T: Copy {
            self.cell.get()
        }

        #[inline]
        pub fn take(&self) -> T where T: Default {
            // Despite mutating the contained value, this doesn't need a write barrier, as
            // the return value of `Default::default` can never contain (non-leaked) `Gc` pointers.
            //
            // The reason for this is somewhat subtle, and boils down to lifetime parametricity.
            // Because Rust doesn't allow naming concrete lifetimes, and because `Default` doesn't
            // have any lifetime parameters, any potential `'gc` lifetime in `T` must be
            // existentially quantified. As such, a `Default` implementation that tries to smuggle
            // a branded `Gc` pointer or `Mutation` through external state (e.g. thread
            // locals) must use unsafe code and cannot be sound in the first place, as it has no
            // way to ensure that the smuggled data has the correct `'gc` brand.
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
    #[inline]
    pub fn get(self) -> T {
        self.cell.get()
    }

    #[inline]
    pub fn set(self, mc: &Mutation<'gc>, t: T) {
        self.unlock(mc).set(t);
    }
}

unsafe impl<'gc, T: Collect<'gc> + Copy + 'gc> Collect<'gc> for Lock<T> {
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
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
        cc.trace(&self.get());
    }
}

// Can't use `#[derive]` because of the non-standard bounds.
impl<T: Copy> Clone for Lock<T> {
    #[inline]
    fn clone(&self) -> Self {
        Self::new(self.get())
    }
}

// Can't use `#[derive]` because of the non-standard bounds.
impl<T: PartialEq + Copy> PartialEq for Lock<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}

// Can't use `#[derive]` because of the non-standard bounds.
impl<T: Eq + Copy> Eq for Lock<T> {}

// Can't use `#[derive]` because of the non-standard bounds.
impl<T: PartialOrd + Copy> PartialOrd for Lock<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.get().partial_cmp(&other.get())
    }
}

// Can't use `#[derive]` because of the non-standard bounds.
impl<T: Ord + Copy> Ord for Lock<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.get().cmp(&other.get())
    }
}

make_lock_wrapper!(
    #[derive(Clone, Default, Eq, PartialEq, Ord, PartialOrd)]
    locked = RefLock as GcRefLock;
    unlocked = RefCell unsafe as_ref_cell;
    impl Sized {
        #[inline]
        pub fn take(&self) -> T where T: Default {
            // See comment in `Lock::take`.
            self.cell.take()
        }
    }
    impl ?Sized {
        #[track_caller]
        #[inline]
        pub fn borrow<'a>(&'a self) -> Ref<'a, T> {
            self.cell.borrow()
        }

        #[inline]
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
                fmt.field(&format_args!("<borrowed>"))
            }
        }
        .finish()
    }
}

impl<'gc, T: ?Sized + 'gc> Gc<'gc, RefLock<T>> {
    #[track_caller]
    #[inline]
    pub fn borrow(self) -> Ref<'gc, T> {
        RefLock::borrow(self.as_ref())
    }

    #[inline]
    pub fn try_borrow(self) -> Result<Ref<'gc, T>, BorrowError> {
        RefLock::try_borrow(self.as_ref())
    }

    #[track_caller]
    #[inline]
    pub fn borrow_mut(self, mc: &Mutation<'gc>) -> RefMut<'gc, T> {
        self.unlock(mc).borrow_mut()
    }

    #[inline]
    pub fn try_borrow_mut(self, mc: &Mutation<'gc>) -> Result<RefMut<'gc, T>, BorrowMutError> {
        self.unlock(mc).try_borrow_mut()
    }
}

unsafe impl<'gc, T: Collect<'gc> + 'gc + ?Sized> Collect<'gc> for RefLock<T> {
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        cc.trace(&*self.borrow());
    }
}

make_lock_wrapper!(
    #[derive(Clone, Eq, PartialEq)]
    locked = OnceLock as GcOnceLock;
    unlocked = OnceCell unsafe as_once_cell;
);

// Can't use `#[derive]` because of the non-standard bounds.
impl<T> Default for OnceLock<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: fmt::Debug> fmt::Debug for OnceLock<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut d = fmt.debug_tuple("OnceLock");
        match self.get() {
            Some(v) => d.field(v),
            None => d.field(&format_args!("<uninit>")),
        };
        d.finish()
    }
}

impl<T> OnceLock<T> {
    #[inline]
    pub fn new() -> Self {
        Self {
            cell: OnceCell::new(),
        }
    }

    #[inline]
    pub fn get(&self) -> Option<&T> {
        self.cell.get()
    }

    #[inline]
    pub fn get_mut(&mut self) -> Option<&mut T> {
        self.cell.get_mut()
    }
}

unsafe impl<'gc, T: Collect<'gc>> Collect<'gc> for OnceLock<T> {
    const NEEDS_TRACE: bool = T::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        if let Some(val) = self.get() {
            cc.trace(val);
        }
    }
}

impl<'gc, T: 'gc> Gc<'gc, OnceLock<T>> {
    #[inline]
    pub fn get(self) -> Option<&'gc T> {
        self.as_ref().get()
    }

    #[inline]
    pub fn set(self, mc: &Mutation<'gc>, value: T) -> Result<(), T> {
        // SAFETY: we emit a write barrier if (and only if) the value is modified.
        let result = self.cell.set(value);
        if result.is_ok() {
            mc.backward_barrier(Gc::erase(self), None);
        }
        result
    }

    #[inline]
    pub fn get_or_init<F>(self, mc: &Mutation<'gc>, f: F) -> &'gc T
    where
        F: FnOnce() -> T,
    {
        // SAFETY: we emit a write barrier if (and only if) the value is modified.
        self.as_ref().cell.get_or_init(|| {
            mc.backward_barrier(Gc::erase(self), None);
            f()
        })
    }
}
