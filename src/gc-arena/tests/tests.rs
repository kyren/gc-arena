#[cfg(feature = "std")]
use rand::distributions::Distribution;
#[cfg(feature = "std")]
use std::{collections::HashMap, rc::Rc};

use gc_arena::lock::RefLock;
use gc_arena::{
    unsafe_empty_collect, unsize, Arena, ArenaParameters, Collect, DynamicRootSet, Gc, GcWeak,
    MutationContext, Rootable,
};

#[test]
fn simple_allocation() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        test: Gc<'gc, i32>,
    }

    let arena = Arena::<Rootable![TestRoot<'gc>]>::new(ArenaParameters::default(), |mc| TestRoot {
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

    let mut arena = Arena::<Rootable![TestRoot<'gc>]>::new(ArenaParameters::default(), |mc| {
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

    let mut arena = Arena::<Rootable![TestRoot<'gc>]>::new(ArenaParameters::default(), |mc| {
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

    let mut arena = Arena::<Rootable![TestRoot<'gc>]>::new(ArenaParameters::default(), |mc| {
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

    let arena = Arena::<Rootable![TestRoot<'gc>]>::new(ArenaParameters::default(), |mc| {
        TestRoot(Gc::new(mc, RefLock::new(Vec::new())))
    });

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

    let mut arena = Arena::<Rootable![TestRoot<'gc>]>::new(ArenaParameters::default(), |mc| {
        TestRoot(Gc::new(mc, RefLock::new(Vec::new())))
    });

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

    let arena = Arena::<Rootable![Root<'gc>]>::new(ArenaParameters::default(), |mc| Root {
        some_complex_state: vec![Gc::new(mc, 42), Gc::new(mc, 69)],
    });

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Intermediate<'gc> {
        root: Root<'gc>,
        state: Gc<'gc, i32>,
    }

    let arena = arena.map_root::<Rootable![Intermediate<'gc>]>(|_, root| {
        let state = root.some_complex_state[0];
        Intermediate { root, state }
    });

    arena.mutate(|_, root| {
        // A complex operation that does some allocations
        assert_eq!(*root.state, 42);
    });

    let arena = arena
        .try_map_root::<Rootable![Intermediate<'gc>], ()>(|_, intermediate| {
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

#[test]
fn test_dynamic_roots() {
    let mut arena: Arena<Rootable![DynamicRootSet<'gc>]> =
        Arena::new(ArenaParameters::default(), |mc| DynamicRootSet::new(mc));

    let initial_size = arena.total_allocated();

    let root1 =
        arena.mutate(|mc, root_set| root_set.stash::<Rootable![Gc<'gc, i32>]>(mc, Gc::new(mc, 12)));

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Root2<'gc>(Gc<'gc, i32>, Gc<'gc, bool>);

    let root2 = arena.mutate(|mc, root_set| {
        root_set.stash::<Rootable![Root2<'gc>]>(mc, Root2(Gc::new(mc, 27), Gc::new(mc, true)))
    });

    arena.collect_all();
    arena.collect_all();

    assert!(arena.total_allocated() > initial_size);

    arena.mutate(|_, root_set| {
        let root1 = *root_set.fetch(&root1);
        assert_eq!(*root1, 12);

        let root2 = root_set.fetch(&root2);
        assert_eq!(*root2.0, 27);
        assert_eq!(*root2.1, true);
    });

    drop(root1);
    drop(root2);

    arena.collect_all();
    arena.collect_all();

    assert!(arena.total_allocated() == initial_size);
}

#[test]
#[should_panic]
fn test_dynamic_bad_set() {
    let arena1: Arena<Rootable![DynamicRootSet<'gc>]> =
        Arena::new(ArenaParameters::default(), |mc| DynamicRootSet::new(mc));

    let arena2: Arena<Rootable![DynamicRootSet<'gc>]> =
        Arena::new(ArenaParameters::default(), |mc| DynamicRootSet::new(mc));

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Root<'gc>(Gc<'gc, i32>);

    let dyn_root =
        arena1.mutate(|mc, root| root.stash::<Rootable![Root<'gc>]>(mc, Root(Gc::new(mc, 44))));

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
fn test_collect_overflow() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        test: Gc<'gc, [u8; 256]>,
    }

    let mut arena =
        Arena::<Rootable![TestRoot<'gc>]>::new(ArenaParameters::default(), |mc| TestRoot {
            test: Gc::new(mc, [0; 256]),
        });

    for _ in 0..1024 {
        arena.collect_all();
        assert!(arena.total_allocated() < 1024); // these should all stay bounded
        assert!(arena.remembered_size() < 1024);
        assert!(arena.allocation_debt() < 1024.0);
    }
}

#[test]
fn cast() {
    use core::cell::Cell;

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

#[test]
fn test_send() {
    fn assert_send<S: Send>() {}

    assert_send::<Arena<Rootable![Gc<'gc, RefLock<()>>]>>();

    trait TestTrait<'gc> {}

    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestStruct<'gc>(Gc<'gc, i32>);

    impl<'gc> TestTrait<'gc> for TestStruct<'gc> {}

    fn test<'gc>(_mc: MutationContext<'gc, '_>, test: Gc<'gc, TestStruct<'gc>>) {
        let _p: Gc<'gc, dyn TestTrait<'gc> + Send> = unsize!(
            test,
            Rootable!['a => Gc<'a, TestStruct<'a>>] =>
            Rootable!['a => Gc<'a, dyn TestTrait<'a> + Send>]
        );
    }

    gc_arena::rootless_arena(|mc| test(mc, Gc::new(mc, TestStruct(Gc::new(mc, 4)))));
}

#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/*.rs");
}
