

use gc_arena::Gc;
use gc_arena::barrier::{Write, field};

struct Foo<'gc> {
    foo: i32,
    bar: f64,
    baz: &'static u32,
    quux: Gc<'gc, u32>,
}

fn projection_prederef1<'a, 'gc>(v: &'a Write<&'a Foo<'gc>>) -> &'a Write<i32> {
    field!(v, Foo, foo)
}

fn projection_prederef2<'a, 'gc>(v: &'a Write<Gc<'gc, Foo<'gc>>>) -> &'a Write<i32> {
    field!(v, Foo, foo)
}

fn projection_postderef1<'a, 'gc>(v: &'a Write<Foo<'gc>>) -> &'a Write<u32> {
    field!(v, Foo, baz)
}

fn projection_postderef2<'a, 'gc>(v: &'a Write<Foo<'gc>>) -> &'a Write<u32> {
    field!(v, Foo, quux)
}

fn projection_wrong_type<'a, 'gc>(v: &'a Write<Foo<'gc>>) -> &'a Write<i64> {
    field!(v, Foo, bar)
}

fn main() {}
