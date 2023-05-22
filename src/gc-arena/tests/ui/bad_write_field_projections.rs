

use gc_arena::Gc;
use gc_arena::barrier::{Write, field};

struct Foo<'gc> {
    foo: i32,
    bar: Gc<'gc, u32>,
    baz: f64,
}

fn projection_prederef<'a, 'gc>(v: &'a Write<Gc<'gc, Foo<'gc>>>) -> &'a Write<i32> {
    field!(v, Foo, foo)
}

fn projection_postderef<'a, 'gc>(v: &'a Write<Foo<'gc>>) -> &'a Write<u32> {
    field!(v, Foo, bar)
}

fn projection_wrong_type<'a, 'gc>(v: &'a Write<Foo<'gc>>) -> &'a Write<i64> {
    field!(v, Foo, baz)
}

fn main() {}
