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
