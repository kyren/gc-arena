#![allow(unused)]

use gc_arena::cell::Mutable;

#[derive(Mutable)]
#[mutable(name = "Dup1", name = "Dup2")]
struct Duplicates {
    #[mutable]
    #[mutable]
    a: i32,
}

#[derive(Mutable)]
#[mutable]
struct InvalidOptions {
    #[mutable(name = "a")]
    a: i32,
    #[mutable = "invalid"]
    b: i32,
}

#[derive(Mutable)]
struct NoMutables(i32);

#[derive(Mutable)]
enum MissingMutName {
    Foo,
    Bar(#[mutable] i32),
}

#[derive(Mutable)]
union Union {
    a: i32,
    #[mutable]
    b: u32,
}

fn main() {}
