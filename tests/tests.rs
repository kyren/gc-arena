use core::{cell::Cell, mem};
#[cfg(feature = "std")]
use std::{array, collections::HashMap, rc::Rc};

#[cfg(feature = "std")]
use rand::distr::Distribution;

use gc_arena::{
    Arena, Collect, DynamicRootSet, Gc, GcBuilder, GcWeak, Lock, RefLock, Rootable,
    arena::CollectionPhase,
    collect::{DynCollect, dyn_collect},
    metrics::Pacing,
    static_collect, unsize,
};

#[test]
fn simple_allocation() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        test: Gc<'gc, i32>,
    }

    let arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| TestRoot {
        test: Gc::new(mc, 42),
    });

    arena.mutate(|_mc, root| {
        assert_eq!(*((*root).test), 42);
    });
}

#[test]
fn weak_allocation() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        test: Gc<'gc, RefLock<Option<Gc<'gc, i32>>>>,
        weak: GcWeak<'gc, i32>,
    }

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| {
        let test = Gc::new(mc, 42);
        let weak = Gc::downgrade(test);
        assert!(weak.upgrade(mc).is_some());
        TestRoot {
            test: Gc::new(mc, RefLock::new(Some(test))),
            weak,
        }
    });
    arena.finish_cycle();
    arena.mutate(|mc, root| {
        assert!(
            root.weak
                .upgrade(mc)
                .map(|gc| Gc::ptr_eq(gc, root.test.borrow().unwrap()))
                .unwrap_or(false)
        );

        *root.test.unlock(mc).borrow_mut() = None;
    });
    let mut done = false;
    while !done {
        arena.mutate(|mc, root| {
            // keep allocating objects to ensure the GC is triggered
            Gc::new(mc, 0);
            if let Some(gc) = root.weak.upgrade(mc) {
                assert_eq!(*gc, 42);
            } else {
                done = true;
            }
        });
        arena.collect_debt();
    }
}

#[cfg(feature = "std")]
#[test]
fn dyn_sized_allocation() {
    #[derive(Clone)]
    struct RefCounter(Rc<()>);
    static_collect!(RefCounter);

    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        slice: Gc<'gc, [Gc<'gc, RefCounter>]>,
    }

    const SIZE: usize = 10;

    let counter = RefCounter(Rc::new(()));

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| {
        let array: [_; SIZE] = core::array::from_fn(|_| Gc::new(mc, counter.clone()));
        let slice = unsize!(Gc::new(mc, array) => [_]);
        TestRoot { slice }
    });

    arena.finish_cycle();

    // Check that no counter was dropped.
    assert_eq!(Rc::strong_count(&counter.0), SIZE + 1);

    // Drop all the RefCounters.
    arena.mutate_root(|mc, root| {
        root.slice = unsize!(Gc::new(mc, []) => [_]);
    });
    arena.finish_cycle();

    // Check that all counters were dropped.
    assert_eq!(Rc::strong_count(&counter.0), 1);
}

#[cfg(feature = "std")]
#[test]
fn repeated_allocation_deallocation() {
    #[derive(Clone)]
    struct RefCounter(Rc<()>);
    static_collect!(RefCounter);

    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc>(Gc<'gc, RefLock<HashMap<i32, Gc<'gc, (i32, RefCounter)>>>>);

    let r = RefCounter(Rc::new(()));

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| {
        TestRoot(Gc::new(mc, RefLock::new(HashMap::new())))
    });

    let key_range = rand::distr::Uniform::try_from(0..200).unwrap();
    let mut rng = rand::rng();

    for _ in 0..20 {
        arena.mutate(|mc, root| {
            let mut map = root.0.unlock(mc).borrow_mut();
            for _ in 0..20 {
                let i = key_range.sample(&mut rng);
                if let Some(old) = map.insert(i, Gc::new(mc, (i, r.clone()))) {
                    assert_eq!(old.0, i);
                }
            }

            for _ in 0..20 {
                let i = key_range.sample(&mut rng);
                if let Some(old) = map.remove(&i) {
                    assert_eq!(old.0, i);
                }
            }
        });

        arena.collect_debt();
    }

    arena.finish_cycle();
    arena.finish_cycle();

    let live_size = arena.mutate(|_, root| root.0.borrow().len());
    assert_eq!(Rc::strong_count(&r.0), live_size + 1);
}

#[test]
fn all_dropped() {
    #[derive(Clone)]
    struct RefCounter(Rc<()>);
    static_collect!(RefCounter);

    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc>(Gc<'gc, RefLock<Vec<Gc<'gc, RefCounter>>>>);

    let r = RefCounter(Rc::new(()));

    let arena =
        Arena::<Rootable![TestRoot<'_>]>::new(|mc| TestRoot(Gc::new(mc, RefLock::new(Vec::new()))));

    arena.mutate(|mc, root| {
        let mut v = root.0.unlock(mc).borrow_mut();
        for _ in 0..100 {
            v.push(Gc::new(mc, r.clone()));
        }
    });
    drop(arena);
    assert_eq!(Rc::strong_count(&r.0), 1);
}

#[test]
fn all_garbage_collected() {
    #[derive(Clone)]
    struct RefCounter(Rc<()>);
    static_collect!(RefCounter);

    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc>(Gc<'gc, RefLock<Vec<Gc<'gc, RefCounter>>>>);

    let r = RefCounter(Rc::new(()));

    let mut arena =
        Arena::<Rootable![TestRoot<'_>]>::new(|mc| TestRoot(Gc::new(mc, RefLock::new(Vec::new()))));

    arena.mutate(|mc, root| {
        let mut v = root.0.unlock(mc).borrow_mut();
        for _ in 0..100 {
            v.push(Gc::new(mc, r.clone()));
        }
    });
    arena.mutate(|mc, root| {
        root.0.unlock(mc).borrow_mut().clear();
    });
    arena.finish_cycle();
    assert_eq!(Rc::strong_count(&r.0), 1);
}

#[test]
fn test_layouts() {
    use core::sync::atomic::{AtomicPtr, Ordering};

    static PTR: AtomicPtr<()> = AtomicPtr::new(core::ptr::null_mut());

    #[derive(Collect)]
    #[collect(require_static)]
    struct Wrapper<T: 'static>(T);

    impl<T> Drop for Wrapper<T> {
        fn drop(&mut self) {
            let ptr = self as *const Self;
            PTR.store(ptr as *mut (), Ordering::SeqCst);
        }
    }

    macro_rules! test_layout {
        (size=$size:literal, align=$align:literal) => {{
            #[repr(align($align))]
            struct Aligned([u8; $size]);

            let array: [u8; $size] = core::array::from_fn(|i| i as u8);

            let ptr = gc_arena::arena::rootless_mutate(|mc| {
                let gc = Gc::new(mc, Wrapper(Aligned(array)));
                assert_eq!(array, gc.0.0);
                Gc::as_ptr(gc) as *mut ()
            });

            let dropped_ptr = PTR.load(Ordering::SeqCst);
            assert_eq!(ptr, dropped_ptr, "size={}, align={}", $size, $align);
        }};
    }

    test_layout!(size = 0, align = 1);
    test_layout!(size = 1, align = 1);
    test_layout!(size = 2, align = 1);

    test_layout!(size = 0, align = 2);
    test_layout!(size = 2, align = 2);
    test_layout!(size = 4, align = 2);

    test_layout!(size = 0, align = 4);
    test_layout!(size = 4, align = 4);
    test_layout!(size = 8, align = 4);

    test_layout!(size = 0, align = 8);
    test_layout!(size = 8, align = 8);
    test_layout!(size = 16, align = 8);

    test_layout!(size = 0, align = 16);
    test_layout!(size = 16, align = 16);
    test_layout!(size = 32, align = 16);

    test_layout!(size = 0, align = 32);
    test_layout!(size = 32, align = 32);
    test_layout!(size = 64, align = 32);
}

#[test]
fn derive_collect() {
    #[allow(unused)]
    #[derive(Collect)]
    #[collect(no_drop)]
    struct Test1<'gc> {
        a: i32,
        b: Gc<'gc, i32>,
    }

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(no_drop)]
    struct Test2 {
        a: i32,
        b: i32,
    }

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(no_drop)]
    enum Test3<'gc> {
        B(Gc<'gc, i32>),
        A(i32),
    }

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(no_drop)]
    enum Test4 {
        A(i32),
    }

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(no_drop, gc_lifetime = 'gc)]
    struct Test5<'gc, 'a>(Gc<'gc, &'a i32>);

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(no_drop)]
    struct Test6(i32);

    assert_eq!(Test1::NEEDS_TRACE, true);
    assert_eq!(Test2::NEEDS_TRACE, false);
    assert_eq!(Test3::NEEDS_TRACE, true);
    assert_eq!(Test4::NEEDS_TRACE, false);
    assert_eq!(Test5::NEEDS_TRACE, true);
    assert_eq!(Test6::NEEDS_TRACE, false);

    struct NoImpl;

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(no_drop)]
    struct Test7 {
        #[collect(require_static)]
        field: NoImpl,
    }

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(no_drop)]
    enum Test8 {
        First {
            #[collect(require_static)]
            field: NoImpl,
        },
    }

    assert_eq!(Test7::NEEDS_TRACE, false);
    assert_eq!(Test8::NEEDS_TRACE, false);

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(no_drop, bound = "where T: Collect<'gc>")]
    struct Test9<T>(T);

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(no_drop, bound = "")]
    struct Test10<'foo, T>(Gc<'foo, ()>, T)
    where
        T: Collect<'foo>;

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(no_drop, bound = "where T: Collect<'foo>")]
    struct Test11<'foo, T>(Gc<'foo, ()>, T);
}

#[test]
fn test_map() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct Root<'gc> {
        some_complex_state: Vec<Gc<'gc, i32>>,
    }

    let arena = Arena::<Rootable![Root<'_>]>::new(|mc| Root {
        some_complex_state: vec![Gc::new(mc, 42), Gc::new(mc, 69)],
    });

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Intermediate<'gc> {
        root: Root<'gc>,
        state: Gc<'gc, i32>,
    }

    let arena = arena.map_root::<Rootable![Intermediate<'_>]>(|_, root| {
        let state = root.some_complex_state[0];
        Intermediate { root, state }
    });

    arena.mutate(|_, root| {
        // A complex operation that does some allocations
        assert_eq!(*root.state, 42);
    });

    let arena = arena
        .try_map_root::<Rootable![Intermediate<'_>], ()>(|_, intermediate| {
            let state = intermediate.root.some_complex_state[1];
            Ok(Intermediate {
                root: intermediate.root,
                state,
            })
        })
        .unwrap();

    arena.mutate(|_, root| {
        // Another complex operation that does some allocations
        assert_eq!(*root.state, 69);
    });
}

#[cfg(feature = "std")]
#[test]
fn test_dynamic_roots() {
    let rc_a = Rc::new(12);
    let rc_b = Rc::new("hello".to_owned());

    let mut arena = Arena::<Rootable![DynamicRootSet<'_>]>::new(|mc| DynamicRootSet::new(mc));

    let root_a = arena
        .mutate(|mc, root_set| root_set.stash::<Rootable![Rc<i32>]>(mc, Gc::new(mc, rc_a.clone())));

    let root_b = arena.mutate(|mc, root_set| {
        root_set.stash::<Rootable![Rc<String>]>(mc, Gc::new(mc, rc_b.clone()))
    });

    assert_eq!(Rc::strong_count(&rc_a), 2);
    assert_eq!(Rc::strong_count(&rc_b), 2);

    arena.finish_cycle();

    assert_eq!(Rc::strong_count(&rc_a), 2);
    assert_eq!(Rc::strong_count(&rc_b), 2);

    let mut root_b_dup = None;

    arena.mutate(|mc, root_set| {
        let root_a = root_set.fetch(&root_a);
        assert_eq!(**root_a, 12);

        let root_b = root_set.fetch(&root_b);
        assert_eq!(root_b.as_str(), "hello");
        root_b_dup = Some(root_set.stash::<Rootable![Rc<String>]>(mc, root_b));
    });

    let root_a_clone = root_a.clone();
    let root_b_clone = root_b.clone();

    drop(root_a);
    drop(root_b);

    arena.finish_cycle();

    assert_eq!(Rc::strong_count(&rc_a), 2);
    assert_eq!(Rc::strong_count(&rc_b), 2);

    drop(root_a_clone);
    drop(root_b_clone);

    arena.finish_cycle();

    assert_eq!(Rc::strong_count(&rc_a), 1);
    // There is still `root_b_dup` which should point to the same object.
    assert_eq!(Rc::strong_count(&rc_b), 2);

    drop(root_b_dup);

    arena.finish_cycle();

    assert_eq!(Rc::strong_count(&rc_b), 1);
}

#[test]
#[should_panic]
fn test_dynamic_bad_set() {
    let arena1 = Arena::<Rootable![DynamicRootSet<'_>]>::new(|mc| DynamicRootSet::new(mc));
    let arena2 = Arena::<Rootable![DynamicRootSet<'_>]>::new(|mc| DynamicRootSet::new(mc));

    let dyn_root = arena1.mutate(|mc, root| root.stash::<Rootable![i32]>(mc, Gc::new(mc, 44)));

    arena2.mutate(|_, root| {
        root.fetch(&dyn_root);
    });
}

#[test]
fn test_unsize() {
    use std::fmt::Display;

    gc_arena::arena::rootless_mutate(|mc| {
        let gc: Gc<'_, String> = Gc::new(mc, "Hello world!".into());
        let gc_weak = Gc::downgrade(gc);

        let dyn_gc = unsize!(gc => dyn Display);
        let dyn_weak = unsize!(gc_weak => dyn Display);
        assert_eq!(dyn_gc.to_string(), "Hello world!");
        assert_eq!(dyn_weak.upgrade(mc).unwrap().to_string(), "Hello world!");

        let gc: Gc<'_, RefLock<i32>> = Gc::new(mc, RefLock::new(12345));
        let gc_weak = Gc::downgrade(gc);

        let dyn_gc = unsize!(gc => RefLock<dyn Display>);
        let dyn_weak = unsize!(gc_weak => RefLock<dyn Display>);
        assert_eq!(dyn_gc.borrow().to_string(), "12345");
        assert_eq!(dyn_weak.upgrade(mc).unwrap().borrow().to_string(), "12345");
    })
}

#[test]
fn test_collection_bounded() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        test: [Gc<'gc, u8>; 32],
    }

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| TestRoot {
        test: array::from_fn(|_| Gc::new(mc, 0)),
    });

    arena.metrics().set_pacing(Pacing {
        sleep_factor: 1.0,
        min_sleep: 64,
        ..Default::default()
    });

    // Finish the current collection cycle so that the new min_sleep is used. We should be asleep
    // for exactly min_sleep, since the sleep factor is 1.0 and 32 less than 64.
    arena.finish_cycle();

    for _ in 0..32 {
        for _ in 0..4 {
            arena.mutate(|mc, _| {
                for _ in 0..32 {
                    let _ = Gc::new(mc, 0u8);
                }
            });
        }
        assert!(arena.metrics().total_gc_count() < 512);
        assert!(arena.metrics().allocation_debt() < 512.0);
        arena.collect_debt();
    }

    for _ in 0..32 {
        for _ in 0..4 {
            arena.mutate_root(|mc, root| {
                let _ = mem::replace(&mut root.test, array::from_fn(|_| Gc::new(mc, 0)));
            });
        }
        assert!(arena.metrics().total_gc_count() < 512);
        assert!(arena.metrics().allocation_debt() < 512.0);
        arena.collect_debt();
    }
}

#[test]
fn cast() {
    #[derive(Collect)]
    #[collect(require_static)]
    #[repr(C)]
    struct A {
        header: Cell<u8>,
        footer: u8,
    }

    #[derive(Collect)]
    #[collect(require_static)]
    #[repr(C)]
    struct B {
        header: Cell<u8>,
    }

    gc_arena::arena::rootless_mutate(|mc| {
        let a = Gc::new(
            mc,
            A {
                header: Cell::new(0b01010101),
                footer: 0b10101010,
            },
        );

        unsafe {
            let b = Gc::cast::<B>(a);
            assert_eq!(b.header.get(), 0b01010101);
            b.header.set(0b11111111);
        }

        assert_eq!(a.header.get(), 0b11111111);
        assert_eq!(a.footer, 0b10101010);
    });
}

#[test]
fn ptr_magic() {
    gc_arena::arena::rootless_mutate(|mc| {
        #[derive(Debug, Eq, PartialEq, Collect)]
        #[collect(require_static)]
        struct S(u8, u32, u64);

        let a = Gc::new(mc, S(3, 4, 5));

        let aptr = Gc::as_ptr(a);

        unsafe {
            assert_eq!(*aptr, S(3, 4, 5));

            let b = Gc::from_ptr(aptr);
            assert_eq!(*b, S(3, 4, 5));
        }
    });
}

#[cfg(feature = "std")]
#[test]
fn okay_panic() {
    use std::panic::{AssertUnwindSafe, catch_unwind};

    use gc_arena::collect::Trace;

    struct Test<'gc> {
        data: Gc<'gc, [u8; 256]>,
        panic_count: Cell<u8>,
        trace_finished: Cell<bool>,
    }

    unsafe impl<'gc> Collect<'gc> for Test<'gc> {
        fn trace<T: Trace<'gc>>(&self, cc: &mut T) {
            let panics = self.panic_count.get();
            if panics > 0 {
                self.panic_count.set(panics - 1);
                panic!("test panic");
            }
            self.data.trace(cc);
            self.trace_finished.set(true);
        }
    }

    let mut arena = Arena::<Rootable![Gc<'_, Test<'_>>]>::new(|mc| {
        Gc::new(
            mc,
            Test {
                data: Gc::new(mc, [5; 256]),
                panic_count: Cell::new(5),
                trace_finished: Cell::new(false),
            },
        )
    });

    for _ in 0..10 {
        if let Err(err) = catch_unwind(AssertUnwindSafe(|| {
            arena.finish_cycle();
        })) {
            assert_eq!(*err.downcast::<&'static str>().unwrap(), "test panic");
        } else {
            break;
        }
    }

    arena.mutate(|_, root| {
        assert_eq!(root.panic_count.get(), 0);
        assert!(root.trace_finished.get());
    });
}

#[test]
fn field_locks() {
    use gc_arena::barrier::{field, unlock};

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Nested<'gc> {
        bar: Lock<Option<Gc<'gc, Test<'gc>>>>,
    }

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Test<'gc> {
        foo: RefLock<Gc<'gc, i32>>,
        nested: Nested<'gc>,
    }

    let arena = Arena::<Rootable![Gc<'_, Test<'_>>]>::new(|mc| {
        Gc::new(
            mc,
            Test {
                foo: RefLock::new(Gc::new(mc, 10)),
                nested: Nested {
                    bar: Lock::new(None),
                },
            },
        )
    });

    arena.mutate(|mc, root| {
        let this = Gc::write(mc, *root);
        assert_eq!(**this.foo.borrow(), 10);
        assert!(this.nested.bar.get().is_none());

        *unlock!(this, Test, foo).borrow_mut() = Gc::new(mc, 42);
        unlock!(field!(this, Test, nested), Nested, bar).set(Some(*root));

        assert_eq!(**this.foo.borrow(), 42);
        assert!(Gc::ptr_eq(this.nested.bar.get().unwrap(), *root));
    });
}

#[test]
fn gc_sleep_actually_sleeps() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        test: [Gc<'gc, u8>; 32],
    }

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| TestRoot {
        test: array::from_fn(|i| Gc::new(mc, i as u8)),
    });

    arena.metrics().set_pacing(Pacing {
        sleep_factor: 1.0,
        min_sleep: 100,
        ..Default::default()
    });

    // Finish the current collection cycle so that the new min_sleep is used. We should be asleep
    // for exactly min_sleep, since the sleep factor is 1.0 and 32 less than 100.
    arena.finish_cycle();

    // We should be asleep, aka the debt should be zero.
    assert!(arena.metrics().allocation_debt() == 0.0);

    for _ in 0..80 {
        arena.mutate(|mc, _| {
            let _ = Gc::new(mc, 0u8);
        });
    }

    // We should still be asleep after allocating 80 GCs.
    assert!(arena.metrics().allocation_debt() == 0.0);

    for _ in 0..30 {
        arena.mutate(|mc, _| {
            let _ = Gc::new(mc, 0u8);
        });
    }

    // We should *not* be asleep after allocating 30 more GCs, because 80 + 30 is greater than 100.
    assert!(arena.metrics().allocation_debt() > 0.0);
}

#[test]
fn stop_the_world_works() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        vec: Vec<Gc<'gc, u8>>,
    }

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|_| TestRoot { vec: Vec::new() });

    arena.metrics().set_pacing(Pacing {
        min_sleep: 100,
        sleep_factor: 1.5,
        ..Pacing::STOP_THE_WORLD
    });

    // Finish the current collection cycle so that the new min_sleep is used.
    arena.finish_cycle();

    // We should be asleep, aka the debt should be zero.
    assert!(arena.metrics().allocation_debt() == 0.0);

    for _ in 0..80 {
        arena.mutate_root(|mc, root| {
            root.vec.push(Gc::new(mc, 0));
        });
    }

    // We should still be asleep after allocating 80 Gcs.
    assert!(arena.metrics().allocation_debt() == 0.0);

    for _ in 0..30 {
        arena.mutate_root(|mc, root| {
            root.vec.push(Gc::new(mc, 0));
        });
    }

    // Our debt should now be positive, since we've definitely allocated more than 100 Gcs.
    assert!(arena.metrics().allocation_debt() > 0.0);

    // This should do a full collection.
    arena.collect_debt();

    // And we should be back asleep.
    assert_eq!(arena.collection_phase(), CollectionPhase::Sleeping);

    // The total remembered allocations after the last full collection was at least 110 Gcs, so
    // allocating 150 Gcs (which is less than 110 * 1.5) should not wake the collector.
    for _ in 0..150 {
        arena.mutate(|mc, _| {
            Gc::new(mc, 0);
        })
    }
    assert!(arena.metrics().allocation_debt() == 0.0);
}

#[test]
fn basic_finalization() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        a: Gc<'gc, u8>,
        b: Gc<'gc, u8>,
        c: GcWeak<'gc, u8>,
        d: GcWeak<'gc, u8>,
    }

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| {
        let a = Gc::new(mc, 1);
        let b = Gc::new(mc, 2);
        TestRoot {
            a,
            b,
            c: Gc::downgrade(a),
            d: Gc::downgrade(b),
        }
    });

    arena.mutate_root(|mc, root| {
        root.a = Gc::new(mc, 3);
    });

    arena.finish_marking().unwrap().finalize(|fc, root| {
        assert!(root.c.upgrade(&fc).is_some());
        assert!(root.c.is_dead(fc));
        assert!(!root.d.is_dead(fc));
        root.c.resurrect(fc);
    });

    arena
        .finish_marking()
        .unwrap()
        .finalize(|fc, root| root.c.resurrect(fc).is_some());

    arena.finish_cycle();

    arena.finish_marking().unwrap().finalize(|fc, root| {
        assert!(root.c.upgrade(&fc).is_some());
        assert!(root.c.is_dead(fc));
        assert!(!root.d.is_dead(fc));
    });

    arena.finish_cycle();

    arena.finish_marking().unwrap().finalize(|fc, root| {
        assert!(root.c.upgrade(&fc).is_none());
        assert!(root.c.is_dead(fc));
        assert!(!root.d.is_dead(fc));
    });
}

#[test]
fn transitive_death() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        a: Option<Gc<'gc, Gc<'gc, u8>>>,
        b: GcWeak<'gc, Gc<'gc, u8>>,
    }

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| {
        let a = Gc::new(mc, Gc::new(mc, 1));
        let b = Gc::downgrade(a);
        TestRoot { a: Some(a), b }
    });

    arena.finish_marking().unwrap().finalize(|fc, root| {
        assert!(!root.b.is_dead(fc));
        assert!(!Gc::is_dead(fc, *root.b.upgrade(&fc).unwrap()));
    });

    arena.finish_cycle();

    arena.mutate_root(|_, root| {
        root.a = None;
    });

    arena.finish_marking().unwrap().finalize(|fc, root| {
        assert!(root.b.is_dead(fc));
        assert!(Gc::is_dead(fc, *root.b.upgrade(&fc).unwrap()));
    });
}

#[test]
fn test_phases() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        test: Gc<'gc, u8>,
    }

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| {
        let test = Gc::new(mc, 0);
        TestRoot { test }
    });
    arena.finish_cycle();

    // The collector must start out in the sleeping phase.
    assert_eq!(arena.collection_phase(), CollectionPhase::Sleeping);

    while arena.collection_phase() == CollectionPhase::Sleeping {
        // Keep accumulating debt to keep the collector moving.
        arena.mutate(|mc, _| {
            Gc::new(mc, 0u8);
        });
        // This cannot move past the Marked phase.
        arena.mark_debt();
    }

    // Assert that the collector has woken up into the Marking / Marked phase.
    assert!(matches!(
        arena.collection_phase(),
        CollectionPhase::Marking | CollectionPhase::Marked
    ));

    loop {
        // Keep accumulating debt to keep the collector moving.
        arena.mutate(|mc, _| {
            Gc::new(mc, 0u8);
        });

        if let Some(marked) = arena.mark_debt() {
            // Manually transition to the Sweeping phase.
            marked.start_sweeping();
            assert!(arena.collection_phase() == CollectionPhase::Sweeping);
            break;
        }
    }

    assert_eq!(arena.collection_phase(), CollectionPhase::Sweeping);

    // Assert that mark_debt() and finish_marking() do nothing while in the Sweeping phase.
    assert!(arena.mark_debt().is_none());
    assert!(arena.finish_marking().is_none());

    assert_eq!(arena.collection_phase(), CollectionPhase::Sweeping);

    // This should not move from Sweeping to Marking in one call, it must pass through Sleeping.
    arena.finish_cycle();

    // We must end back up at Sleeping.
    assert!(arena.collection_phase() == CollectionPhase::Sleeping);
}

#[test]
fn barriers() {
    use std::cell::Cell;

    use gc_arena::collect::Trace;

    #[derive(Default)]
    struct Node<'gc, S, W> {
        strong_child: Cell<Option<Gc<'gc, S>>>,
        weak_child: Cell<Option<GcWeak<'gc, W>>>,
    }

    unsafe impl<'gc, S: Collect<'gc>, W: Collect<'gc>> Collect<'gc> for Node<'gc, S, W> {
        const NEEDS_TRACE: bool = true;

        fn trace<T: Trace<'gc>>(&self, cc: &mut T) {
            cc.trace(&self.strong_child.get());
            cc.trace(&self.weak_child.get());
        }
    }

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Root<'gc> {
        node: Gc<'gc, Node<'gc, i32, i32>>,
    }

    let mut arena = Arena::<Rootable![Root<'_>]>::new(|mc| Root {
        node: Gc::new(mc, Node::default()),
    });

    // Will finish marking and `node` should be black.
    arena.finish_marking();

    // Make `node` adopt a white child pointer with a backwards barrier.
    arena.mutate(|mc, root| {
        let p = Gc::new(mc, 17);
        mc.backward_barrier(Gc::erase(root.node), Some(Gc::erase(p)));
        root.node.strong_child.set(Some(p));
    });

    // Finish collection, if the barrier didn't work this would delete the allocated pointer.
    arena.finish_cycle();

    arena.mutate(|_, root| {
        assert_eq!(*root.node.strong_child.get().unwrap(), 17);
    });

    // Will finish marking and `node` should be black.
    arena.finish_marking();

    // Make `node` adopt a white child weak pointer with a backwards barrier.
    arena.mutate(|mc, root| {
        let w = Gc::downgrade(Gc::new(mc, 13));
        mc.backward_barrier_weak(Gc::erase(root.node), GcWeak::erase(w));
        root.node.weak_child.set(Some(w));
    });

    // Finish collection, if the barrier didn't work this would delete the allocated pointer.
    arena.finish_cycle();

    arena.mutate(|_, root| {
        assert!(root.node.weak_child.get().unwrap().is_dropped());
    });

    // Reset
    arena.mutate(|_, root| {
        root.node.strong_child.set(None);
        root.node.weak_child.set(None);
    });

    // Will finish marking and `node` should be black.
    arena.finish_marking();

    // Make `node` adopt a white child pointer with a forwards barrier.
    arena.mutate(|mc, root| {
        let p = Gc::new(mc, 17);
        mc.forward_barrier(Some(Gc::erase(root.node)), Gc::erase(p));
        root.node.strong_child.set(Some(p));
    });

    // Finish collection, if the barrier didn't work this would delete the allocated pointer.
    arena.finish_cycle();

    arena.mutate(|_, root| {
        assert_eq!(*root.node.strong_child.get().unwrap(), 17);
    });

    // Will finish marking and `node` should be black.
    arena.finish_marking();

    // Make `node` adopt a white child weak pointer with a forwards barrier.
    arena.mutate(|mc, root| {
        let w = Gc::downgrade(Gc::new(mc, 13));
        mc.forward_barrier_weak(Some(Gc::erase(root.node)), GcWeak::erase(w));
        root.node.weak_child.set(Some(w));
    });

    // Finish collection, if the barrier didn't work this would delete the allocated pointer.
    arena.finish_cycle();

    arena.mutate(|_, root| {
        assert!(root.node.weak_child.get().unwrap().is_dropped());
    });
}

#[test]
fn cycle_debt_stops() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        test: Gc<'gc, u8>,
    }

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| {
        let test = Gc::new(mc, 0);
        TestRoot { test }
    });
    arena.metrics().set_pacing(Pacing {
        min_sleep: 100,
        ..Pacing::DEFAULT
    });

    arena.finish_cycle();

    assert_eq!(arena.collection_phase(), CollectionPhase::Sleeping);

    loop {
        for i in 0..4u8 {
            arena.mutate(|mc, _| {
                Gc::new(mc, i);
            });
        }
        if let Some(marked_arena) = arena.mark_debt() {
            marked_arena.start_sweeping();
            break;
        }
    }

    assert_eq!(arena.collection_phase(), CollectionPhase::Sweeping);

    loop {
        for i in 0..4u8 {
            arena.mutate(|mc, _| {
                Gc::new(mc, i);
            });
        }
        arena.cycle_debt();
        match arena.collection_phase() {
            CollectionPhase::Sweeping => {}
            CollectionPhase::Sleeping => break,
            CollectionPhase::Marking | CollectionPhase::Marked => {
                panic!("`Arena::cycle_debt` should not transition past sleeping in one call!")
            }
        }
    }
}

#[test]
fn dyn_collect() {
    trait MyTrait<'gc>: 'gc + DynCollect<'gc> {}
    dyn_collect!(dyn MyTrait<'gc>);

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(no_drop)]
    struct Test<'gc> {
        field: Box<dyn MyTrait<'gc> + 'gc>,
    }

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Impl<'gc>(Gc<'gc, ()>);

    impl<'gc> MyTrait<'gc> for Impl<'gc> {}

    #[allow(unused)]
    fn test_fn<'gc>(p: impl MyTrait<'gc> + Collect<'gc> + 'gc) -> Box<dyn MyTrait<'gc> + 'gc> {
        Box::new(p)
    }

    gc_arena::arena::rootless_mutate(|mc| {
        let _test = Gc::new(
            mc,
            Test {
                field: Box::new(Impl(Gc::new(mc, ()))),
            },
        );
    });

    trait MyTrait2<'gc, T>: DynCollect<'gc>
    where
        T: Clone,
    {
    }

    dyn_collect!(<T> dyn MyTrait2<'gc, T> where T: Clone);

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(no_drop)]
    struct Test2<'gc> {
        field: Box<dyn MyTrait2<'gc, ()>>,
    }
}

#[test]
fn static_collect_with_param() {
    #[allow(unused)]
    #[derive(Clone)]
    struct Test<T>(Rc<T>);
    static_collect!(<T> Test<T>);
}

#[test]
fn zst_cache() {
    use gc_arena::zst_cache::ZstCache;

    #[repr(align(8))]
    struct Aligned8;

    #[repr(align(16))]
    struct Aligned16;

    #[repr(align(32))]
    struct Aligned32;

    gc_arena::arena::rootless_mutate(|mc| {
        let zst_cache = ZstCache::<16>::new(mc);

        let aligned8_ptr1 = zst_cache.alloc_static(mc, Aligned8);
        let _ = *aligned8_ptr1;

        let aligned8_ptr2 = zst_cache.alloc_static(mc, Aligned8);
        let _ = *aligned8_ptr2;

        let aligned16_ptr = zst_cache.alloc_static(mc, Aligned16);
        let _ = *aligned16_ptr;

        assert!(zst_cache.is_cached(aligned8_ptr1));
        assert!(zst_cache.is_cached(aligned8_ptr2));
        assert!(zst_cache.is_cached(aligned16_ptr));

        let aligned32_ptr = zst_cache.alloc_static(mc, Aligned32);
        let _ = *aligned32_ptr;

        assert!(!zst_cache.is_cached(aligned32_ptr));
    });
}

#[test]
fn lesser_aligned_prefix_has_same_ptr() {
    #[derive(Collect)]
    #[collect(require_static)]
    #[repr(align(1))]
    struct UnderAligned(u8);

    #[derive(Collect)]
    #[collect(require_static)]
    #[repr(C, align(1024))]
    struct OverAligned {
        header: UnderAligned,
        value: u8,
    }

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Root<'gc> {
        header_gc: Gc<'gc, UnderAligned>,
    }

    let mut arena = Arena::<Rootable![Root<'_>]>::new(|mc| {
        let gc = Gc::new(
            mc,
            OverAligned {
                header: UnderAligned(7),
                value: 13,
            },
        );

        // SAFETY: This is allowed because `OverAligned` is `#[repr(C)]`
        let header_gc = unsafe { Gc::from_ptr(Gc::as_ptr(gc) as *const UnderAligned) };

        Root { header_gc }
    });

    arena.finish_cycle();

    arena.mutate(|_, root| {
        assert_eq!(root.header_gc.0, 7);
    });
}

#[test]
fn test_alloc_slice_with_header() {
    use std::array;

    macro_rules! test_slice {
        (
            header_size = $header_size:literal,
            header_align = $header_align:literal,
            element_size = $element_size:literal,
            element_align = $element_align:literal,
            len = $len:literal $(,)?
        ) => {{
            #[repr(align($header_align))]
            struct Header([u8; $header_size]);
            static_collect!(Header);

            #[repr(align($element_align))]
            struct Element([u8; $element_size]);
            static_collect!(Element);

            fn expected_array<const LEN: usize>(offset: usize) -> [u8; LEN] {
                array::from_fn(|i| (i + offset) as u8)
            }

            gc_arena::arena::rootless_mutate(|mc| {
                let gc_ptr = gc_arena::GcSliceWithHeaderBuilder::new($len)
                    .write_header(Header(expected_array(0)))
                    .write_slice_with(mc, |i| Element(expected_array(i + 1)));

                assert!(gc_ptr.header.0 == expected_array(0));

                for i in 0..$len {
                    assert!(gc_ptr.slice[i].0 == expected_array(1 + i));
                }
            });
        }};
    }

    test_slice!(
        header_size = 0,
        header_align = 1,
        element_size = 0,
        element_align = 1,
        len = 5
    );
    test_slice!(
        header_size = 1,
        header_align = 1,
        element_size = 1,
        element_align = 1,
        len = 5
    );
    test_slice!(
        header_size = 8,
        header_align = 1,
        element_size = 8,
        element_align = 1,
        len = 5
    );
    test_slice!(
        header_size = 32,
        header_align = 1,
        element_size = 32,
        element_align = 1,
        len = 5
    );

    test_slice!(
        header_size = 0,
        header_align = 8,
        element_size = 0,
        element_align = 1,
        len = 5
    );
    test_slice!(
        header_size = 1,
        header_align = 8,
        element_size = 1,
        element_align = 1,
        len = 5
    );
    test_slice!(
        header_size = 8,
        header_align = 8,
        element_size = 8,
        element_align = 1,
        len = 5
    );
    test_slice!(
        header_size = 32,
        header_align = 8,
        element_size = 32,
        element_align = 1,
        len = 5
    );

    test_slice!(
        header_size = 0,
        header_align = 1,
        element_size = 0,
        element_align = 8,
        len = 5
    );
    test_slice!(
        header_size = 1,
        header_align = 1,
        element_size = 1,
        element_align = 8,
        len = 5
    );
    test_slice!(
        header_size = 8,
        header_align = 1,
        element_size = 8,
        element_align = 8,
        len = 5
    );
    test_slice!(
        header_size = 32,
        header_align = 1,
        element_size = 32,
        element_align = 8,
        len = 5
    );

    test_slice!(
        header_size = 0,
        header_align = 8,
        element_size = 0,
        element_align = 32,
        len = 5
    );
    test_slice!(
        header_size = 1,
        header_align = 8,
        element_size = 1,
        element_align = 32,
        len = 5
    );
    test_slice!(
        header_size = 8,
        header_align = 8,
        element_size = 8,
        element_align = 32,
        len = 5
    );
    test_slice!(
        header_size = 32,
        header_align = 8,
        element_size = 32,
        element_align = 32,
        len = 5
    );

    test_slice!(
        header_size = 0,
        header_align = 32,
        element_size = 0,
        element_align = 8,
        len = 5
    );
    test_slice!(
        header_size = 1,
        header_align = 32,
        element_size = 1,
        element_align = 8,
        len = 5
    );
    test_slice!(
        header_size = 8,
        header_align = 32,
        element_size = 8,
        element_align = 8,
        len = 5
    );
    test_slice!(
        header_size = 32,
        header_align = 32,
        element_size = 32,
        element_align = 8,
        len = 5
    );
}

#[test]
fn test_slice_with_header_drop() {
    let rc = Rc::new(());

    gc_arena::arena::rootless_mutate(|mc| {
        let _ = gc_arena::GcSliceWithHeaderBuilder::new(10)
            .write_header(rc.clone())
            .write_slice_with(mc, |_| rc.clone());
    });

    assert_eq!(Rc::strong_count(&rc), 1);
}

#[test]
fn test_panicking_slice_with_header_drop() {
    use std::panic::catch_unwind;

    let rc = Rc::new(());

    let Err(err) = catch_unwind(|| {
        gc_arena::arena::rootless_mutate(|mc| {
            let _ = gc_arena::GcSliceWithHeaderBuilder::new(10)
                .write_header(rc.clone())
                .write_slice_with(mc, |i| {
                    if i < 5 {
                        rc.clone()
                    } else {
                        panic!("test panic")
                    }
                });
        });
    }) else {
        unreachable!()
    };
    assert_eq!(*err.downcast::<&'static str>().unwrap(), "test panic");

    assert_eq!(Rc::strong_count(&rc), 1);
}

#[test]
fn test_slice_with_header_copy() {
    gc_arena::arena::rootless_mutate(|mc| {
        let ptr = gc_arena::GcSliceWithHeaderBuilder::new(5)
            .write_header(47)
            .copy_slice(mc, &[5, 6, 7, 8, 9]);

        assert_eq!(ptr.header, 47);
        assert_eq!(ptr.slice, [5, 6, 7, 8, 9]);
    });
}

#[test]
fn test_thin_slice_with_header() {
    use gc_arena::{GcSliceWithHeader, GcSliceWithHeaderBuilder, GcThinSliceWithHeader};

    assert!(mem::size_of::<GcSliceWithHeader<i32, i32>>() > mem::size_of::<Gc<()>>());
    assert!(mem::size_of::<GcThinSliceWithHeader<i32, i32>>() == mem::size_of::<Gc<()>>());

    gc_arena::arena::rootless_mutate(|mc| {
        let ptr = GcSliceWithHeaderBuilder::new(5)
            .write_header(47)
            .copy_slice(mc, &[5, 6, 7, 8, 9]);

        let thin_ptr: GcThinSliceWithHeader<i32, i32> = Gc::as_thin(ptr);
        assert_eq!(*Gc::as_thin_ref(thin_ptr), 47);
        assert_eq!(thin_ptr.header, 47);
        assert_eq!(thin_ptr.slice, [5, 6, 7, 8, 9]);

        let fat_ptr: GcSliceWithHeader<i32, i32> = Gc::as_fat(thin_ptr);
        assert_eq!(fat_ptr.header, 47);
        assert_eq!(fat_ptr.slice, [5, 6, 7, 8, 9]);
    });
}

#[test]
fn test_thin_slice() {
    use gc_arena::{GcSlice, GcThinSlice};

    assert!(mem::size_of::<GcSlice<i32>>() > mem::size_of::<Gc<()>>());
    assert!(mem::size_of::<GcThinSlice<i32>>() == mem::size_of::<Gc<()>>());

    gc_arena::arena::rootless_mutate(|mc| {
        let ptr = Gc::new_slice(mc, &[5, 6, 7, 8, 9]);

        let thin_ptr: GcThinSlice<i32> = Gc::as_thin(ptr);
        assert_eq!(*thin_ptr, [5, 6, 7, 8, 9]);

        let fat_ptr: GcSlice<i32> = Gc::as_fat(thin_ptr);
        assert_eq!(*fat_ptr, [5, 6, 7, 8, 9]);
    });
}

#[test]
fn test_thin_str() {
    use gc_arena::{GcStr, GcThinStr};

    assert!(mem::size_of::<GcStr>() > mem::size_of::<Gc<()>>());
    assert!(mem::size_of::<GcThinStr>() == mem::size_of::<Gc<()>>());

    gc_arena::arena::rootless_mutate(|mc| {
        let s = Gc::new_str(mc, "foo");

        let thin_s: GcThinStr = Gc::as_thin(s);
        assert_eq!(thin_s.as_ref(), "foo");

        let fat_s: GcStr = Gc::as_fat(thin_s);
        assert_eq!(fat_s.as_ref(), "foo");
    });
}

#[test]
fn test_type_metadata() {
    gc_arena::arena::rootless_mutate(|mc| {
        #[derive(Collect)]
        #[collect(require_static)]
        struct TypeA(u32);

        #[derive(Collect)]
        #[collect(require_static)]
        struct TypeB(u32);

        impl gc_arena::meta::TypeMeta for TypeA {
            type TypeMetadata = u32;

            const TYPE_METADATA: u32 = 7;

            gc_arena::meta::type_meta_const_promotion!();
        }

        impl gc_arena::meta::TypeMeta for TypeB {
            type TypeMetadata = u32;

            const TYPE_METADATA: u32 = 8;

            gc_arena::meta::type_meta_const_promotion!();
        }

        let a = GcBuilder::new_with_type_meta::<TypeA>().write(mc, TypeA(7));
        let b = GcBuilder::new_with_type_meta::<TypeB>().write(mc, TypeB(8));

        assert_eq!(a.0, 7);
        assert_eq!(Gc::type_metadata(a), 7);
        assert_eq!(b.0, 8);
        assert_eq!(Gc::type_metadata(b), 8);
    });
}

#[test]
fn test_type_meta_same_types() {
    gc_arena::arena::rootless_mutate(|mc| {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct Object(HashMap<String, i32>);

        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        struct IsLocked(bool);

        struct Locked;

        impl gc_arena::meta::TypeMeta for Locked {
            type TypeMetadata = IsLocked;

            const TYPE_METADATA: &'static IsLocked = &IsLocked(true);
        }

        struct Unlocked;

        impl gc_arena::meta::TypeMeta for Unlocked {
            type TypeMetadata = IsLocked;

            const TYPE_METADATA: &'static IsLocked = &IsLocked(false);
        }

        let obj_a = GcBuilder::new_with_type_meta::<Locked>().write(mc, Object(HashMap::new()));
        let obj_b = GcBuilder::new_with_type_meta::<Unlocked>().write(mc, Object(HashMap::new()));

        assert_eq!(*Gc::type_metadata(obj_a), IsLocked(true));
        assert_eq!(*Gc::type_metadata(obj_b), IsLocked(false));

        let mut _obj_c = obj_a;
        _obj_c = obj_b;
    })
}

#[test]
fn test_builder_drop() {
    gc_arena::arena::rootless_mutate(|mc| {
        for i in 0..10 {
            GcBuilder::<i32>::new().write(mc, i);
        }
    });
}

#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/*.rs");
}
