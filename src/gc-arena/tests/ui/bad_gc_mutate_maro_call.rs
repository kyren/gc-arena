use gc_arena::{Collect, Gc, GcCell};

#[derive(Collect)]
#[collect(no_drop)]
struct Root<'gc>(GcCell<'gc, Data<'gc>>);

#[derive(Collect)]
#[collect(no_drop)]
struct Data<'gc> {
    gc: Gc<'gc, bool>,
}

trait MyTrait {
    type MyGat<'a>;
}

impl<'gc> MyTrait for Data<'gc> {
    type MyGat<'a> = Gc<'gc, bool>;
}

fn exploit<'gc>(data: GcCell<'gc, Data<'gc>>) {
    // Tests that we can only provide an ident, not a path.
    // See the comments in `reroot!` for why this is important
    // for soundness.
    gc_arena::reroot!(<Data<'gc> as MyTrait>::MyGat, root.0, |mut data| {})
}

fn main() {}