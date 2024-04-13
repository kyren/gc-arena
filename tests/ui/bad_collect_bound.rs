use gc_arena::Collect;

struct NotCollect;

#[derive(Collect)]
#[collect(no_drop)]
struct MyStruct {
    field: NotCollect
}

fn main() {}
