use std::rc::Rc;

use gc_arena::{Arena, Gc, Rootable};

fn assert_send<S: Send>() {}

fn main() {
    assert_send::<Arena<Rootable![Gc<'gc, ()>]>>();
    assert_send::<Arena<Rootable![Gc<'gc, Rc<()>>]>>();
}
