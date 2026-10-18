use enum_map::{Enum, EnumMap};

use crate::collect::{Collect, Trace};

// SAFETY: `EnumMap<K, V>` only contains values of type `V`. We trace all values, satisfying the
// requirement that all held `Gc` pointers are traced. `EnumMap` does not have a custom `Drop` and
// does not provide interior mutability, so requirements 2 and 3 of `Collect` are upheld by `V`.
unsafe impl<'gc, K: Enum, V: Collect<'gc>> Collect<'gc> for EnumMap<K, V> {
    const NEEDS_TRACE: bool = V::NEEDS_TRACE;

    #[inline]
    fn trace<C: Trace<'gc>>(&self, cc: &mut C) {
        if V::NEEDS_TRACE {
            for v in self.values() {
                cc.trace(v);
            }
        }
    }
}
