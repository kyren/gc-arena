use std::rc::Rc;

use gc_arena::{
    static_send::{static_send, StaticSend},
    Gc, MutationContext, Rootable,
};

fn make_send<'gc>(mc: MutationContext<'gc, '_>) -> StaticSend<Gc<'gc, Rc<i32>>> {
    let ptr = Gc::new(mc, Rc::new(3));
    static_send::<Rootable!['a => Gc<'a, Rc<i32>>]>(ptr)
}

fn main() {}
