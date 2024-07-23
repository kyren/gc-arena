use gc_arena::Collect;

struct NoCollectImpl<'a>(&'a bool);

#[derive(Collect)]
#[collect(no_drop)]
struct MyStruct<'a> {
    #[collect(require_static)]
    field: NoCollectImpl<'a>
}

fn assert_my_struct_collect<'a>() {
    let _ = MyStruct::<'a>::NEEDS_TRACE;
}

fn main() {}
