#[cfg(feature = "allocator-api2")]
mod inner {
    use core::hash::{BuildHasher, Hash};

    use allocator_api2::alloc::Allocator;

    use crate::barrier::IndexWrite;
    use crate::collect::{Collect, Trace};

    unsafe impl<'gc, K, V, S, A> Collect<'gc> for hashbrown::HashMap<K, V, S, A>
    where
        K: Collect<'gc>,
        V: Collect<'gc>,
        S: 'static,
        A: Allocator + Clone + Collect<'gc>,
    {
        const NEEDS_TRACE: bool = K::NEEDS_TRACE || V::NEEDS_TRACE || A::NEEDS_TRACE;

        #[inline]
        fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
            for (k, v) in self {
                cc.trace(k);
                cc.trace(v);
            }
            cc.trace(self.allocator());
        }
    }

    unsafe impl<'gc, T, S, A> Collect<'gc> for hashbrown::HashSet<T, S, A>
    where
        T: Collect<'gc>,
        S: 'static,
        A: Allocator + Clone + Collect<'gc>,
    {
        const NEEDS_TRACE: bool = T::NEEDS_TRACE || A::NEEDS_TRACE;

        #[inline]
        fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
            for v in self {
                cc.trace(v);
            }
            cc.trace(self.allocator());
        }
    }

    unsafe impl<'gc, T, A> Collect<'gc> for hashbrown::HashTable<T, A>
    where
        T: Collect<'gc>,
        A: Allocator + Clone + Collect<'gc>,
    {
        const NEEDS_TRACE: bool = T::NEEDS_TRACE || A::NEEDS_TRACE;

        #[inline]
        fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
            for v in self {
                cc.trace(v);
            }
            cc.trace(self.allocator());
        }
    }

    unsafe impl<K, V, S, A, Q> IndexWrite<&Q> for hashbrown::HashMap<K, V, S, A>
    where
        K: Eq + Hash,
        Q: Hash + hashbrown::Equivalent<K> + ?Sized,
        S: BuildHasher,
        A: Allocator,
    {
    }
}

#[cfg(not(feature = "allocator-api2"))]
mod inner {
    use core::hash::{BuildHasher, Hash};

    use crate::barrier::IndexWrite;
    use crate::collect::{Collect, Trace};

    unsafe impl<'gc, K, V, S> Collect<'gc> for hashbrown::HashMap<K, V, S>
    where
        K: Collect<'gc>,
        V: Collect<'gc>,
        S: 'static,
    {
        const NEEDS_TRACE: bool = K::NEEDS_TRACE || V::NEEDS_TRACE;

        #[inline]
        fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
            for (k, v) in self {
                cc.trace(k);
                cc.trace(v);
            }
        }
    }

    unsafe impl<'gc, T, S> Collect<'gc> for hashbrown::HashSet<T, S>
    where
        T: Collect<'gc>,
        S: 'static,
    {
        const NEEDS_TRACE: bool = T::NEEDS_TRACE;

        #[inline]
        fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
            for v in self {
                cc.trace(v);
            }
        }
    }

    unsafe impl<'gc, T> Collect<'gc> for hashbrown::HashTable<T>
    where
        T: Collect<'gc>,
    {
        const NEEDS_TRACE: bool = T::NEEDS_TRACE;

        #[inline]
        fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
            for v in self {
                cc.trace(v);
            }
        }
    }

    unsafe impl<K, V, S, Q> IndexWrite<&Q> for hashbrown::HashMap<K, V, S>
    where
        K: Eq + Hash,
        Q: Hash + hashbrown::Equivalent<K> + ?Sized,
        S: BuildHasher,
    {
    }
}
