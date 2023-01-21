use gc_arena::{Collect, GcCell, Gc, Arena, Rootable, ArenaParameters};

#[derive(Collect)]
#[collect(no_drop)]
struct Root<'gc>(GcCell<'gc, Data<'gc>>);

#[derive(Collect)]
#[collect(no_drop)]
struct Data<'gc> {
    gc_ptr_1: Gc<'gc, bool>,
    gc_ptr_2: Gc<'gc, bool>,
    removable_pointer: Option<Gc<'gc, String>>,
    non_gc: Box<i32>,
}

fn main() {
    let arena = Arena::<Rootable![Root<'gc>]>::new(ArenaParameters::default(), |mc| {
        Root(GcCell::allocate(mc, Data {
            gc_ptr_1: Gc::allocate(mc, true),
            gc_ptr_2: Gc::allocate(mc, false),
            removable_pointer: Some(Gc::allocate(mc, String::from("My string"))),
            non_gc: Box::new(42),
        }))
    });

    arena.mutate(|mc, root| {
        // Test that we cannot store pointers from outside the closure in the GcCell
        let outer_ptr = root.0.read().gc_ptr_1;
        gc_arena::reroot!(Data, root.0, |mut data| {
            data.gc_ptr_2 = outer_ptr;
        }); 

        // Test that we cannot exfiltrate pointers from the GcCell
        // (this is not actually important for soundness, but we should know if this change)
        let _exfiltrated = gc_arena::reroot!(Data, root.0, |data| {
            data.removable_pointer
        }); 

        // Test that we cannot allocate new pointers with an existing MutationContext
        // and store them in the GcCell
        gc_arena::reroot!(Data, root.0, |mut data| {
            let new_gc_ptr = Gc::allocate(mc, true);
            data.gc_ptr_1 = new_gc_ptr;
        });
    });
    
}