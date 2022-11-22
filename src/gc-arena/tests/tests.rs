#[cfg(feature = "std")]
use rand::distributions::Distribution;
#[cfg(feature = "std")]
use std::collections::HashMap;
use std::rc::Rc;

use gc_arena::{make_arena, unsafe_empty_collect, ArenaParameters, Collect, Gc, GcCell, GcWeak};

#[test]
fn simple_allocation() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        test: Gc<'gc, i32>,
    }

    make_arena!(TestArena, TestRoot);

    let arena = TestArena::new(ArenaParameters::default(), |mc| TestRoot {
        test: Gc::allocate(mc, 42),
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
        test: GcCell<'gc, Option<Gc<'gc, i32>>>,
        weak: GcWeak<'gc, i32>,
    }

    make_arena!(TestArena, TestRoot);

    let mut arena = TestArena::new(ArenaParameters::default(), |mc| {
        let test = Gc::allocate(mc, 42);
        let weak = Gc::downgrade(test);
        assert!(weak.upgrade(mc).is_some());
        TestRoot {
            test: GcCell::allocate(mc, Some(test)),
            weak,
        }
    });
    arena.collect_all();
    arena.mutate(|mc, root| {
        assert!(root
            .weak
            .upgrade(mc)
            .map(|gc| Gc::ptr_eq(gc, root.test.read().unwrap()))
            .unwrap_or(false));

        *root.test.write(mc) = None;
    });
    let mut done = false;
    while !done {
        arena.mutate(|mc, root| {
            // keep allocating objects to ensure the GC is triggered
            Gc::allocate(mc, 0);
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
fn repeated_allocation_deallocation() {
    #[derive(Clone)]
    struct RefCounter(Rc<()>);
    unsafe_empty_collect!(RefCounter);

    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc>(GcCell<'gc, HashMap<i32, Gc<'gc, (i32, RefCounter)>>>);
    make_arena!(TestArena, TestRoot);

    let r = RefCounter(Rc::new(()));

    let mut arena = TestArena::new(ArenaParameters::default(), |mc| {
        TestRoot(GcCell::allocate(mc, HashMap::new()))
    });

    let key_range = rand::distributions::Uniform::from(0..10000);
    let mut rng = rand::thread_rng();

    for _ in 0..200 {
        arena.mutate(|mc, root| {
            let mut map = root.0.write(mc);
            for _ in 0..100 {
                let i = key_range.sample(&mut rng);
                if let Some(old) = map.insert(i, Gc::allocate(mc, (i, r.clone()))) {
                    assert_eq!(old.0, i);
                }
            }

            for _ in 0..100 {
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

    let live_size = arena.mutate(|_, root| root.0.read().len());
    assert_eq!(Rc::strong_count(&r.0), live_size + 1);
}

#[test]
fn all_dropped() {
    #[derive(Clone)]
    struct RefCounter(Rc<()>);
    unsafe_empty_collect!(RefCounter);

    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc>(GcCell<'gc, Vec<Gc<'gc, RefCounter>>>);
    make_arena!(TestArena, TestRoot);

    let r = RefCounter(Rc::new(()));

    let arena = TestArena::new(ArenaParameters::default(), |mc| {
        TestRoot(GcCell::allocate(mc, Vec::new()))
    });

    arena.mutate(|mc, root| {
        let mut v = root.0.write(mc);
        for _ in 0..100 {
            v.push(Gc::allocate(mc, r.clone()));
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
    struct TestRoot<'gc>(GcCell<'gc, Vec<Gc<'gc, RefCounter>>>);
    make_arena!(TestArena, TestRoot);

    let r = RefCounter(Rc::new(()));

    let mut arena = TestArena::new(ArenaParameters::default(), |mc| {
        TestRoot(GcCell::allocate(mc, Vec::new()))
    });

    arena.mutate(|mc, root| {
        let mut v = root.0.write(mc);
        for _ in 0..100 {
            v.push(Gc::allocate(mc, r.clone()));
        }
    });
    arena.mutate(|mc, root| {
        root.0.write(mc).clear();
    });
    arena.collect_all();
    arena.collect_all();
    assert_eq!(Rc::strong_count(&r.0), 1);
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
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/*.rs");
}

#[test]
fn recursive_struct_indirect_gc() {
    #[derive(Collect)]
    #[collect(no_drop)]
    pub enum Value<'gc, S: 'gc> {
        #[allow(dead_code)]
        Closure(Gc<'gc, Closure<'gc, S>>),
    }

    #[derive(Collect)]
    #[collect(no_drop)]
    pub struct Closure<'gc, S: 'gc> {
        pub vars: Vec<Value<'gc, S>>,
    }
}

#[test]
fn recursive_struct_indirect_gccell() {
    #[derive(Collect)]
    #[collect(no_drop)]
    pub enum Value<'gc, S: 'gc> {
        #[allow(dead_code)]
        Closure(GcCell<'gc, Closure<'gc, S>>),
    }

    #[derive(Collect)]
    #[collect(no_drop)]
    pub struct Closure<'gc, S: 'gc> {
        pub vars: Vec<Value<'gc, S>>,
    }
}
