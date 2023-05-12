use gc_arena::{Arena, ArenaParameters, Collect, Gc, Rootable};

fn main() {
    #[derive(Collect)]
    #[collect(no_drop)]
    struct TestRoot<'gc> {
        test: Gc<'gc, u8>,
    }

    let arena = Arena::<Rootable![TestRoot<'gc>]>::new(ArenaParameters::default(), |mc| TestRoot {
        test: Gc::new(mc, 4),
    });

    let arena = Box::leak(Box::new(arena));
    let _test: Gc<'static, _> = arena.mutate(|_, root| root.test);
}
