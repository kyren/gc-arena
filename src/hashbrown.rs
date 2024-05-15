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
        #[inline]
        fn needs_trace() -> bool {
            K::needs_trace() || V::needs_trace() || A::needs_trace()
        }

        #[inline]
        fn trace(&self, cc: &Collection) {
            self.allocator().trace(cc);
            for (k, v) in self {
                k.trace(cc);
                v.trace(cc);
            }
        }
    }

    unsafe impl<T, S, A> Collect for hashbrown::HashSet<T, S, A>
    where
        T: Collect,
        S: 'static,
        A: Allocator + Clone + Collect,
    {
        #[inline]
        fn needs_trace() -> bool {
            T::needs_trace() || A::needs_trace()
        }

        #[inline]
        fn trace(&self, cc: &Collection) {
            self.allocator().trace(cc);
            for v in self {
                v.trace(cc);
            }
        }
    }

    unsafe impl<T, A> Collect for hashbrown::HashTable<T, A>
    where
        T: Collect,
        A: Allocator + Clone + Collect,
    {
        #[inline]
        fn needs_trace() -> bool {
            T::needs_trace() || A::needs_trace()
        }

        #[inline]
        fn trace(&self, cc: &Collection) {
            self.allocator().trace(cc);
            for v in self {
                v.trace(cc);
            }
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
        #[inline]
        fn needs_trace() -> bool {
            K::needs_trace() || V::needs_trace()
        }

        #[inline]
        fn trace(&self, cc: &Collection) {
            for (k, v) in self {
                k.trace(cc);
                v.trace(cc);
            }
        }
    }

    unsafe impl<T, S> Collect for hashbrown::HashSet<T, S>
    where
        T: Collect,
        S: 'static,
    {
        #[inline]
        fn needs_trace() -> bool {
            T::needs_trace()
        }

        #[inline]
        fn trace(&self, cc: &Collection) {
            for v in self {
                v.trace(cc);
            }
        }
    }

    unsafe impl<T> Collect for hashbrown::HashTable<T>
    where
        T: Collect,
    {
        #[inline]
        fn needs_trace() -> bool {
            T::needs_trace()
        }

        #[inline]
        fn trace(&self, cc: &Collection) {
            for v in self {
                v.trace(cc);
            }
        }
    }
}
