#[cfg(feature = "allocator-api2")]
mod inner {
    use allocator_api2::alloc::Allocator;

    use crate::{collect::Collect, context::Collection};

    unsafe impl<K, V, S, A> Collect for hashbrown::HashMap<K, V, S, A>
    where
        K: Collect,
        V: Collect,
        S: 'static,
        A: Allocator + Clone + Collect,
    {
        const NEEDS_TRACE: bool = K::NEEDS_TRACE || V::NEEDS_TRACE || A::NEEDS_TRACE;

        #[inline]
        fn trace(&self, mut cc: Collection<'_>) {
            for (k, v) in self {
                cc.trace(k);
                cc.trace(v);
            }
            self.allocator().trace(cc);
        }
    }

    unsafe impl<T, S, A> Collect for hashbrown::HashSet<T, S, A>
    where
        T: Collect,
        S: 'static,
        A: Allocator + Clone + Collect,
    {
        const NEEDS_TRACE: bool = T::NEEDS_TRACE || A::NEEDS_TRACE;

        #[inline]
        fn trace(&self, mut cc: Collection<'_>) {
            for v in self {
                cc.trace(v);
            }
            self.allocator().trace(cc);
        }
    }

    unsafe impl<T, A> Collect for hashbrown::HashTable<T, A>
    where
        T: Collect,
        A: Allocator + Clone + Collect,
    {
        const NEEDS_TRACE: bool = T::NEEDS_TRACE || A::NEEDS_TRACE;

        #[inline]
        fn trace(&self, mut cc: Collection<'_>) {
            for v in self {
                cc.trace(v);
            }
            self.allocator().trace(cc);
        }
    }
}

#[cfg(not(feature = "allocator-api2"))]
mod inner {
    use crate::{collect::Collect, context::Collection};

    unsafe impl<K, V, S> Collect for hashbrown::HashMap<K, V, S>
    where
        K: Collect,
        V: Collect,
        S: 'static,
    {
        const NEEDS_TRACE: bool = K::NEEDS_TRACE || V::NEEDS_TRACE;

        #[inline]
        fn trace(&self, mut cc: Collection<'_>) {
            for (k, v) in self {
                cc.trace(k);
                cc.trace(v);
            }
        }
    }

    unsafe impl<T, S> Collect for hashbrown::HashSet<T, S>
    where
        T: Collect,
        S: 'static,
    {
        const NEEDS_TRACE: bool = T::NEEDS_TRACE;

        #[inline]
        fn trace(&self, mut cc: Collection<'_>) {
            for v in self {
                cc.trace(v);
            }
        }
    }

    unsafe impl<T> Collect for hashbrown::HashTable<T>
    where
        T: Collect,
    {
        const NEEDS_TRACE: bool = T::NEEDS_TRACE;

        #[inline]
        fn trace(&self, mut cc: Collection<'_>) {
            for v in self {
                cc.trace(v);
            }
        }
    }
}
