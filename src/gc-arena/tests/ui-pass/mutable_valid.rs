#![allow(unused)]

use gc_arena::cell::Mutable;

mod nested {
    use super::Mutable;

    #[derive(Mutable)]
    pub struct Named {
        #[mutable]
        pub a: i32,
        b: u32,
        c: String,
    }
    
    #[derive(Mutable)]
    #[mutable(name = "TupleMut")]
    pub struct Tuple<T>(i32, #[mutable] T, u32);
    
    #[derive(Mutable)]
    #[mutable(name = "EnumMut")]
    pub enum Enum {
        Foo,
        Bar(String),
        Baz {
            #[mutable]
            a: i32,
            b: u32,
        },
        Quux(char, #[mutable] i32),
    }
}

// Assert that types that should be public are.
use nested::{Named, Tuple, TupleMut, Enum, EnumMut};

fn assert_pub_field(named: &Mutable<Named>) -> &Mutable<i32> {
    Named::project_mut(named).a
}

fn match_enum_mut(e: &Mutable<Enum>) -> Option<&Mutable<i32>> {
    match Enum::project_mut(e) {
        EnumMut::Foo => None,
        EnumMut::Bar(_) => None,
        EnumMut::Baz { a, b: _ } => Some(a),
        EnumMut::Quux(_, a) => Some(a),
    }
}

fn main() {}
