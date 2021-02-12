use gc_arena::Collect;

#[derive(Collect)]
#[collect(no_drop)]
struct MyStruct {
    #[collect(invalid_arg)] field: u8
}

fn main() {}
