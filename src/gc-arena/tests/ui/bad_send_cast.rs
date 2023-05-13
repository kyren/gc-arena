use std::rc::Rc;

use gc_arena::{unsize, Collect, Gc, MutationContext, Rootable};

trait TestTrait<'gc> {}

#[derive(Collect)]
#[collect(no_drop)]
struct TestStruct<'gc>(Gc<'gc, Rc<i32>>);

impl<'gc> TestTrait<'gc> for TestStruct<'gc> {}

fn test<'gc>(_mc: MutationContext<'gc, '_>, test: Gc<'gc, TestStruct<'gc>>) {
    let _p: Gc<'gc, dyn TestTrait<'gc> + Send> = unsize!(
        test,
        Rootable!['a => Gc<'a, TestStruct<'a>>] =>
        Rootable!['a => Gc<'a, dyn TestTrait<'a> + Send>]
    );
}

fn main() {}
