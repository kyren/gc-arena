use gc_arena::{Arena, Collect, Gc, Mutation, RefLock, Rootable};

// We define a node of a doubly-linked list data structure.
//
// `Collect` is derived procedurally, meaning that we can't mess up and forget
// to trace our inner `prev`, `next`, or `value`.
#[derive(Copy, Clone, Collect)]
// For safety, we agree to not implement `Drop`. We could also use
// `#[collect(unsafe_drop)]` or `#[collect(require_static)]` (if our type were
// 'static) here instead.
#[collect(no_drop)]
struct Node<'gc, T: 'gc> {
    // The representation of the `prev` and `next` fields is a plain machine
    // pointer that might be NULL.
    //
    // Thanks, niche optimization!
    prev: Option<NodePtr<'gc, T>>,
    next: Option<NodePtr<'gc, T>>,
    value: T,
}

// By default, `Collect` types (other than 'static types) cannot have interior
// mutability. In order to provide safe mutation, we need to use `gc-arena`
// specific types to provide it which guarantee that write barriers are invoked.
//
// We use `RefLock` here as an alternative to `RefCell`.
type NodePtr<'gc, T> = Gc<'gc, RefLock<Node<'gc, T>>>;

// Create a new `Node` and return a pointer to it.
//
// We need to pass the `&Mutation<'gc>` context here because we are mutating the
// object graph (by creating a new "object" with `Gc::new`).
fn new_node<'gc, T: Collect>(mc: &Mutation<'gc>, value: T) -> NodePtr<'gc, T> {
    Gc::new(
        mc,
        RefLock::new(Node {
            prev: None,
            next: None,
            value,
        }),
    )
}

// Join two nodes together, setting the `left` node's `next` field to `right`,
// and the `right` node's `prev` field to `left`.
//
// Again, we are mutating the object graph, so we must pass in the
// `&Mutation<'gc>` context.
fn node_join<'gc, T>(mc: &Mutation<'gc>, left: NodePtr<'gc, T>, right: NodePtr<'gc, T>) {
    // This is `Gc<RefLock<_>>::borrow_mut`, which takes the mutation context as
    // a parameter. Write barriers will always be invoked on the target pointer,
    // so we know it is safe to mutate the value behind the pointer.
    left.borrow_mut(mc).next = Some(right);
    right.borrow_mut(mc).prev = Some(left);
}

// Use a `NodePtr` as a cursor, move forward through a linked list by following
// `next` pointers.
//
// Returns `true` if there was a `next` pointer and the target node has been
// changed.
fn node_rotate_right<'gc, T>(node: &mut NodePtr<'gc, T>) -> bool {
    if let Some(next) = node.borrow().next {
        *node = next;
        true
    } else {
        false
    }
}

// Use a `NodePtr` as a cursor, move backward through a linked list by following
// `prev` pointers.
//
// Returns `true` if there was a `prev` pointer and the target node has been
// changed.
fn node_rotate_left<'gc, T>(node: &mut NodePtr<'gc, T>) -> bool {
    if let Some(prev) = node.borrow().prev {
        *node = prev;
        true
    } else {
        false
    }
}

fn main() {
    // Create a new arena with a single `NodePtr<'_, i32>` as the root type.
    //
    // We can't refer to some *particular* `NodePtr<'gc, i32>`, what we need to
    // be able to refer to is a set of `NodePtr<'_, i32>` for any possible '_
    // that we might pick. We use gc-arena's `Rootable!` macro for this.
    let mut arena = Arena::<Rootable![NodePtr<'_, i32>]>::new(|mc| {
        // Create a simple linked list with three links.
        //
        // 1 <-> 2 <-> 3 <-> 4

        let one = new_node(mc, 1);
        let two = new_node(mc, 2);
        let three = new_node(mc, 3);
        let four = new_node(mc, 4);

        node_join(mc, one, two);
        node_join(mc, two, three);
        node_join(mc, three, four);

        // We return the pointer to 1 as our root
        one
    });

    // Outside of a call to `Arena::new` or `Arena::mutate`, we have no access
    // to anything *inside* the arena. We have to *visit* the arena with one of
    // the mutation methods in order to access its interior.

    arena.mutate_root(|_, root| {
        // We can examine the root type and see that our linked list is still
        // [1, 2, 3, 4]
        for i in 1..=4 {
            assert_eq!(root.borrow().value, i);
            node_rotate_right(root);
        }
    });

    arena.mutate_root(|_, root| {
        // Also, all of the reverse links work too.
        for i in (1..=4).rev() {
            assert_eq!(root.borrow().value, i);
            node_rotate_left(root);
        }
    });

    arena.mutate(|mc, root| {
        // Make the list circular! We sever the connection to 4 and link 3 back
        // to 1 making a list like this...
        //
        // +-> 1 <-> 2 <-> 3 <-+
        // |                   |
        // +-------------------+
        let one = *root;
        let two = one.borrow().next.unwrap();
        let three = two.borrow().next.unwrap();
        node_join(mc, three, one);
    });

    // The node for 4 is now unreachable!
    //
    // It can be freed during collection, but collection does not happen
    // automatically. We have to trigger collection *outside* of a mutation
    // method.
    //
    // The `Arena::collect_all` finishes the current collection cycle, but this
    // is not the only way to trigger collection.
    //
    // `gc-arena` is an incremental collector, and so keeps track of "debt"
    // during the GC cycle, pacing the collector based on the rate and size of new
    // allocations.
    //
    // We can also call `Arena::collect_debt` to do a *bit* of collection at a
    // time, based on the current collector debt.
    //
    // Since the collector has not yet started its marking phase, calling this
    // will fully mark the arena and collect all the garbage, so this method
    // will always free the 4 node.
    arena.collect_all();

    arena.mutate_root(|_, root| {
        // Now we can see that if we rotate through our circular list, we will
        // get:
        //
        // 1 -> 2 -> 3 -> 1 -> 2 -> 3
        for _ in 0..2 {
            for i in 1..=3 {
                assert_eq!(root.borrow().value, i);
                node_rotate_right(root);
            }
        }
    });
}
