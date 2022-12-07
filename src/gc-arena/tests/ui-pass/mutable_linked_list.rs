use gc_arena::cell::{Cell, Mutable};
use gc_arena::{Collect, Gc, MutationContext};

#[derive(Collect, Mutable)]
#[collect(no_drop)]
struct Node<'gc, T: Collect + 'gc> {
    #[mutable]
    prev: Cell<Option<Gc<'gc, Node<'gc, T>>>>,
    #[mutable]
    next: Cell<Option<Gc<'gc, Node<'gc, T>>>>,
    value: Gc<'gc, T>,
}

#[allow(unused)]
fn detach_node<'gc, T: Collect>(mc: MutationContext<'gc, '_>, node: Gc<'gc, Node<'gc, T>>) -> Gc<'gc, T> {
    let node = Node::mutate_fields(mc, &node);
    let prev = node.prev.replace(None);
    let next = node.next.replace(None);

    if let Some(p) = prev {
        let p = Node::mutate_fields(mc, &p);
        p.next.set(next);
    }

    if let Some(n) = next {
        let p = Node::mutate_fields(mc, &n);
        p.prev.set(prev);
    }

    node.value
}

fn main() {}
