use gc_arena::Collect;

#[derive(Collect)]
#[collect(no_drop)]
struct MyStruct {
    #[collect(require_static)]
    #[collect(require_static)]
    field: bool
}

fn main() {}
