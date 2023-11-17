use core::{cell::Cell, mem};
use gc_arena::finalize_queue::FinalizeQueue;
#[cfg(feature = "std")]
use rand::distributions::Distribution;
#[cfg(feature = "std")]
use std::{collections::HashMap, rc::Rc};

use gc_arena::{
    lock::RefLock, metrics::Pacing, unsafe_empty_collect, unsize, Arena, Collect, DynamicRootSet,
    Gc, GcWeak, Rootable,
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
    arena.collect_all();
    arena.mutate(|mc, root| {
        assert!(root
            .weak
            .upgrade(mc)
            .map(|gc| Gc::ptr_eq(gc, root.test.borrow().unwrap()))
            .unwrap_or(false));

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
    unsafe_empty_collect!(RefCounter);

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

    arena.collect_all();

    // Check that no counter was dropped.
    assert_eq!(Rc::strong_count(&counter.0), SIZE + 1);

    // Drop all the RefCounters.
    arena.mutate_root(|mc, root| {
        root.slice = unsize!(Gc::new(mc, []) => [_]);
    });
    arena.collect_all();

    // Check that all counters were dropped.
    assert_eq!(Rc::strong_count(&counter.0), 1);
}

#[cfg(feature = "std")]
#[test]
fn repeated_allocation_deallocation() {
    #[derive(Clone)]
    struct RefCounter(Rc<()>);
    unsafe_empty_collect!(RefCounter);

    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc>(Gc<'gc, RefLock<HashMap<i32, Gc<'gc, (i32, RefCounter)>>>>);

    let r = RefCounter(Rc::new(()));

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| {
        TestRoot(Gc::new(mc, RefLock::new(HashMap::new())))
    });

    let key_range = rand::distributions::Uniform::from(0..10000);
    let mut rng = rand::thread_rng();

    for _ in 0..40 {
        arena.mutate(|mc, root| {
            let mut map = root.0.unlock(mc).borrow_mut();
            for _ in 0..50 {
                let i = key_range.sample(&mut rng);
                if let Some(old) = map.insert(i, Gc::new(mc, (i, r.clone()))) {
                    assert_eq!(old.0, i);
                }
            }

            for _ in 0..50 {
                let i = key_range.sample(&mut rng);
                if let Some(old) = map.remove(&i) {
                    assert_eq!(old.0, i);
                }
            }
        });

        arena.collect_debt();
    }

    arena.collect_all();
    arena.collect_all();

    let live_size = arena.mutate(|_, root| root.0.borrow().len());
    assert_eq!(Rc::strong_count(&r.0), live_size + 1);
}

#[test]
fn all_dropped() {
    #[derive(Clone)]
    struct RefCounter(Rc<()>);
    unsafe_empty_collect!(RefCounter);

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
    unsafe_empty_collect!(RefCounter);

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
    arena.collect_all();
    arena.collect_all();
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

            let ptr = gc_arena::rootless_arena(|mc| {
                let gc = Gc::new(mc, Wrapper(Aligned(array)));
                assert_eq!(array, gc.0 .0);
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
    #[collect(no_drop)]
    struct Test5(Gc<'static, i32>);

    #[allow(unused)]
    #[derive(Collect)]
    #[collect(no_drop)]
    struct Test6(i32);

    assert_eq!(Test1::needs_trace(), true);
    assert_eq!(Test2::needs_trace(), false);
    assert_eq!(Test3::needs_trace(), true);
    assert_eq!(Test4::needs_trace(), false);
    assert_eq!(Test5::needs_trace(), true);
    assert_eq!(Test6::needs_trace(), false);

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

    assert_eq!(Test7::needs_trace(), false);
    assert_eq!(Test8::needs_trace(), false);
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

    let mut arena: Arena<Rootable![DynamicRootSet<'_>]> = Arena::new(|mc| DynamicRootSet::new(mc));

    let root_a = arena.mutate(|mc, root_set| {
        root_set.stash::<Rootable![Gc<'_, Rc<i32>>]>(mc, Gc::new(mc, rc_a.clone()))
    });

    let root_b = arena.mutate(|mc, root_set| {
        root_set.stash::<Rootable![Gc<'_, Rc<String>>]>(mc, Gc::new(mc, rc_b.clone()))
    });

    assert_eq!(Rc::strong_count(&rc_a), 2);
    assert_eq!(Rc::strong_count(&rc_b), 2);

    arena.collect_all();
    arena.collect_all();

    assert_eq!(Rc::strong_count(&rc_a), 2);
    assert_eq!(Rc::strong_count(&rc_b), 2);

    arena.mutate(|_, root_set| {
        let root_a = *root_set.fetch(&root_a);
        assert_eq!(**root_a, 12);

        let root_b = root_set.fetch(&root_b);
        assert_eq!(root_b.as_str(), "hello");
    });

    drop(root_a);
    drop(root_b);

    arena.collect_all();
    arena.collect_all();

    assert_eq!(Rc::strong_count(&rc_a), 1);
    assert_eq!(Rc::strong_count(&rc_b), 1);
}

#[test]
#[should_panic]
fn test_dynamic_bad_set() {
    let arena1: Arena<Rootable![DynamicRootSet<'_>]> = Arena::new(|mc| DynamicRootSet::new(mc));

    let arena2: Arena<Rootable![DynamicRootSet<'_>]> = Arena::new(|mc| DynamicRootSet::new(mc));

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Root<'gc>(Gc<'gc, i32>);

    let dyn_root =
        arena1.mutate(|mc, root| root.stash::<Rootable![Root<'_>]>(mc, Root(Gc::new(mc, 44))));

    arena2.mutate(|_, root| {
        root.fetch(&dyn_root);
    });
}

#[test]
fn test_unsize() {
    use std::fmt::Display;

    gc_arena::rootless_arena(|mc| {
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
        test: Gc<'gc, [u8; 256]>,
    }

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| TestRoot {
        test: Gc::new(mc, [0; 256]),
    });

    arena.metrics().set_pacing(
        Pacing::default()
            .with_pause_factor(1.0)
            .with_timing_factor(1.0)
            .with_min_sleep(256),
    );
    // Finish the current collection cycle so that the new min_sleep is used.
    arena.collect_all();

    for _ in 0..1024 {
        for _ in 0..4 {
            arena.mutate(|mc, _| {
                let _ = Gc::new(mc, [0u8; 256]);
            });
        }
        assert!(arena.metrics().total_allocation() < 4096);
        assert!(arena.metrics().allocation_debt() < 4096.0);
        arena.collect_debt();
    }

    for _ in 0..1024 {
        for _ in 0..4 {
            arena.mutate_root(|mc, root| {
                let _ = mem::replace(&mut root.test, Gc::new(mc, [0u8; 256]));
            });
        }
        assert!(arena.metrics().total_allocation() < 4096);
        assert!(arena.metrics().allocation_debt() < 4096.0);
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

    gc_arena::rootless_arena(|mc| {
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
    gc_arena::rootless_arena(|mc| {
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
    use std::panic::{catch_unwind, AssertUnwindSafe};

    struct Test<'gc> {
        data: Gc<'gc, [u8; 256]>,
        panic_count: Cell<u8>,
        trace_finished: Cell<bool>,
    }

    unsafe impl<'gc> Collect for Test<'gc> {
        fn trace(&self, cc: &gc_arena::Collection) {
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
            arena.collect_all();
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
    use gc_arena::lock::{Lock, RefLock};

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
fn gc_pause_actually_pauses() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        test: Gc<'gc, [u8; 256]>,
    }

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| TestRoot {
        test: Gc::new(mc, [0; 256]),
    });

    arena.metrics().set_pacing(
        Pacing::default()
            .with_pause_factor(1.0)
            .with_timing_factor(1.0)
            .with_min_sleep(1024),
    );
    // Finish the current collection cycle so that the new min_sleep is used. We should be asleep
    // for exactly min_sleep, since the pause factor is 1.0 and 2x 256 bytes is 512 which is less
    // than 1024.
    arena.collect_all();

    // We should be asleep, aka the debt should be zero.
    assert!(arena.metrics().allocation_debt() == 0.0);

    for _ in 0..8 {
        arena.mutate(|mc, _| {
            let _ = Gc::new(mc, [0u8; 100]);
        });
    }

    // We should still be asleep after allocating 800 bytes (assumes that the overhead is less than
    // 224 bytes).
    assert!(arena.metrics().allocation_debt() == 0.0);

    for _ in 0..3 {
        arena.mutate(|mc, _| {
            let _ = Gc::new(mc, [0u8; 100]);
        });
    }

    // We should *not* be asleep after allocating 300 more bytes, because this is greater than 1024.
    assert!(arena.metrics().allocation_debt() > 0.0);
}

#[test]
fn gc_external_allocation_affects_timing() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        test: Gc<'gc, [u8; 256]>,
    }

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| TestRoot {
        test: Gc::new(mc, [0; 256]),
    });

    arena.metrics().set_pacing(
        Pacing::default()
            .with_pause_factor(1.0)
            .with_timing_factor(1.0)
            .with_min_sleep(1024),
    );
    // Finish the current collection cycle so that the new min_sleep is used. We should be asleep
    // for exactly min_sleep, since the pause factor is 1.0 and 2x 256 bytes is 512 which is less
    // than 1024.
    arena.collect_all();

    // We should be asleep, aka the debt should be zero.
    assert!(arena.metrics().allocation_debt() == 0.0);

    for _ in 0..8 {
        arena.metrics().mark_external_allocation(100);
    }

    // We should still be asleep after allocating 800 bytes (assumes that the overhead is less than
    // 224 bytes).
    assert!(arena.metrics().allocation_debt() == 0.0);

    for _ in 0..3 {
        arena.metrics().mark_external_allocation(100);
    }

    let debt_high_mark = arena.metrics().allocation_debt();

    // We should *not* be asleep after allocating 300 more bytes, because this is greater than 1024.
    assert!(debt_high_mark > 0.0);

    // Free all of the external data we just pretended to allocate
    for _ in 0..11 {
        arena.metrics().mark_external_deallocation(100);
    }

    // This should have payed off some debt.
    assert!(arena.metrics().allocation_debt() < debt_high_mark);
}

#[test]
fn finalize_queue() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        finalize_queue: FinalizeQueue<'gc, u8>,
        test: Gc<'gc, u8>,
    }

    let mut arena = Arena::<Rootable![TestRoot<'_>]>::new(|mc| TestRoot {
        finalize_queue: FinalizeQueue::new(mc),
        test: Gc::new(mc, 4),
    });

    arena.mutate(|mc, root| {
        root.finalize_queue.register(Gc::new(mc, 1));
        root.finalize_queue.register(Gc::new(mc, 2));
        root.finalize_queue.register(Gc::new(mc, 3));
        root.finalize_queue.register(root.test);
        root.finalize_queue.register(Gc::new(mc, 5));
        root.finalize_queue.register(Gc::new(mc, 6));
    });

    arena.collect_all();

    arena.mutate_root(|mc, root| {
        let mut v = Vec::new();
        while let Some(p) = root.finalize_queue.poll() {
            v.push(*p);
        }
        v.sort();
        assert_eq!(v, &[1, 2, 3, 5, 6]);
        root.test = Gc::new(mc, 7);
        root.finalize_queue.register(root.test);
    });

    arena.collect_all();

    arena.mutate_root(|_, root| {
        let mut v = Vec::new();
        while let Some(p) = root.finalize_queue.poll() {
            v.push(*p);
        }
        v.sort();
        assert_eq!(v, &[4]);
    });
}

#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/*.rs");
}
