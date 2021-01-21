use gc_arena::Collect;

#[derive(Collect)]
#[collect(no_drop)]
enum MyEnum {
    #[collect(require_static)]
    First {
        field: u8
    }
}

fn main() {}
