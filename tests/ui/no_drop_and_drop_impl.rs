use gc_arena::Collect;

#[derive(Collect)]
#[collect(no_drop)]
struct Foo {
}

impl Drop for Foo {
    fn drop(&mut self) {}
}

fn main() {}
