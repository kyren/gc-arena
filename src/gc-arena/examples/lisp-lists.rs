/*!
LISP list demo<br/>
Bart Massey 2022

This toy app is a demo of `gc-arena` by way of LISP-style
lists.  A library could probably be built for this to good
effect, but this is maybe a better demo of the low-level use
of the `gc-arena` crate.

Thanks much to Manish Goregaokar for
[this article](https://manishearth.github.io/blog/2021/04/05/a-tour-of-safe-tracing-gc-designs-in-rust/)
that contained a nice tiny example of the use of
`gc-arena`. This was really helpful in understanding how to
use this crate.
*/

use gc_arena::*;

/// Start with a structure that will be treated as the
/// garbage collector "root".
///
/// Anything that might be mutated needs a [GcCell] rather than
/// a [Gc], so go with that.
///
/// Our root will be a `Vec` of top-level [List]s that the
/// GC will keep as long as they are referenced.
///
/// You will almost always want `no_drop` when deriving [Collect].
#[derive(Collect)]
#[collect(no_drop)]
struct MyRoot<'gc>(GcCell<'gc, Vec<List<'gc>>>);

impl<'gc> MyRoot<'gc> {
    /// Get a given list from the root. The `read()`
    /// primitive is necessary to ensure correctness of the
    /// read.
    fn get_list(&self, index: usize) -> List<'gc> {
        let lists: &Vec<List<'_>> = &self.0.read();
        lists[index]
    }

    /// Add a list to the root. The `write()` primitive is
    /// necessary to ensure correctness of the write, as it
    /// adds a "fence".
    fn push_list(&self, mc: MutationContext<'gc, '_>, list: List<'gc>) {
        self.0.write(mc).push(list);
    }
}

/// Define a `MyArena` type that knows about its roots.
type MyArena = Arena<Rootable![MyRoot<'gc>]>;

// This is how creating `MyArena` used to be done as of gc-arena 0.2.2.
// make_arena!(MyArena, MyRoot);

/// The fundamental list data structure of LISP is the "cons
/// cell".  A cons cell normally consists of a pair of references
/// to values: the "car" and "cdr". For a list,
/// the car will contain a list value and the cdr will reference
/// the rest of the list. LISP cons cells are sometimes used for
/// other things, though.
///
/// This definition thus is not the way things are normally
/// done in LISP, inasmuch as `Nil` wouldn't be part of a
/// normal definition and we are only doing homogenous lists
/// of `u32`s which gives us a simplification. This way
/// makes things easier in Rust.  I think.
///
/// (The terms car, cdr and cons are historical curiosities:
/// "cons" is short for "construct", and "car" and "cdr"
/// were the "contents of the address register" and
/// "contents of the decrement register" on some ancient
/// hardware.)
#[derive(Debug, Collect, Clone, Copy)]
#[collect(no_drop)]
enum ConsCell<'gc> {
    Nil,
    Cons { car: u32, cdr: List<'gc> },
}
use ConsCell::*;

/// The head of the [List] is wrapped so that the GC
/// understands that it is collectible. This is just a
/// convenience to avoid some really ugly types. There
/// should be no impact on the actual data structure, since
/// the compiler can elide the tag here.
///
/// Note that the `Copy` implementation here won't actually
/// copy anything but a pointer. This is the beauty of GC.
#[derive(Debug, Collect, Clone, Copy)]
#[collect(no_drop)]
struct List<'gc>(GcCell<'gc, ConsCell<'gc>>);

impl<'gc> List<'gc> {
    /// Get the cons cell out of the list. This
    /// requires a `read()`.
    fn cell(self) -> ConsCell<'gc> {
        *self.0.read()
    }

    /// Put a new cons cell into the list. This
    /// requires a `write()`.
    fn set_cell(self, mc: MutationContext<'gc, '_>, c: ConsCell<'gc>) {
        *self.0.write(mc) = c;
    }

    /// An empty [List] is just a pointer to a [Nil] cons cell
    /// in memory.
    ///
    /// For this GC to be able to collect it, it has to be
    /// aware of what arena it is in, etc. This is the role
    /// of `mc`.
    fn new(mc: MutationContext<'gc, '_>) -> Self {
        List(GcCell::allocate(mc, Nil))
    }

    /// To stick a value on the front of a [List], allocate
    /// a new [ConsCell] with the value and the rest of the list.
    ///
    /// Note that this allows "tail sharing": if you cons
    /// two different values onto the same list, you get two
    /// lists that share every cons cell but their first.
    /// Reference counting or garbage collection is needed
    /// to make sure that freeing the tail of one list does
    /// not free the other. Because LISPs typically allow mutating
    /// the cdr arbitrarily, GC is needed to deal with cycles
    /// that may result.
    fn cons(self, mc: MutationContext<'gc, '_>, car: u32) -> Self {
        List(GcCell::allocate(mc, Cons { car, cdr: self }))
    }

    /// Get the value at the head of the list, or panic if
    /// the list is empty.
    ///
    /// It would be more Rustic to return an `Option<u32>`,
    /// but this way is LISP tradition.
    fn car(self) -> u32 {
        if let Cons { car, .. } = self.cell() {
            car
        } else {
            panic!("car of nil")
        }
    }

    /// Get the "rest" of the list after the first element,
    /// or panic if the list is empty.
    ///
    /// It would be more Rustic to return an `Option<List>`,
    /// but this way is LISP tradition.
    fn cdr(self) -> Self {
        if let Cons { cdr, .. } = self.cell() {
            cdr
        } else {
            panic!("cdr of nil")
        }
    }

    /// Check whether a list is empty.
    fn is_nil(self) -> bool {
        matches!(self.cell(), Nil)
    }

    /// Print the list on one line in LISP format.
    ///
    /// This would loop forever if the list is cyclic.
    /// For demo purposes, we put an artificial limit on
    /// the loop count.
    fn print(mut self) {
        print!("(");
        let mut sep = "";
        let mut nsteps = 0;
        while !self.is_nil() {
            print!("{sep}");
            print!("{}", self.car());
            self = self.cdr();
            sep = " ";

            nsteps += 1;
            if nsteps >= 10 {
                println!(" ...");
                return;
            }
        }
        println!(")");
    }

    /// Destructively concatenate `other` onto the end of
    /// `self`.  This will affect any lists that tail-share
    /// with `self`, and thus should be used with extreme
    /// caution. `nconc` can be used to create cyclic lists,
    /// so also use caution there.
    fn nconc(mut self, mc: MutationContext<'gc, '_>, other: List<'gc>) {
        let mut prev = self;
        while !self.is_nil() {
            prev = self;
            self = self.cdr();
        }
        prev.cdr().set_cell(mc, other.cell());
    }
}

fn main() {
    // Set up an arena with default parameters.
    let params = ArenaParameters::default();
    let arena = MyArena::new(params, |mc| MyRoot(GcCell::allocate(mc, vec![])));

    // Create a couple of lists, with the tail of `l2` being
    // `l1`. Then store them as roots in the arena.
    arena.mutate(|mc, root| {
        let l1 = List::new(mc).cons(mc, 1);
        let l2 = l1.cons(mc, 2);
        root.push_list(mc, l1);
        root.push_list(mc, l2);
    });

    // Print the two root lists. Yep, they're still there
    // and look OK.
    //
    // Garbage collection may happen before this. Thus, it
    // is generally better to break up the operations into
    // small chunks so the GC gets a chance to operate.
    arena.mutate(|_, root| {
        root.get_list(0).print();
        root.get_list(1).print();
    });

    // `nconc` a third list onto the end of `l1`. This will
    // also affect `l2` due to tail-sharing.
    arena.mutate(|mc, root| {
        let l1 = root.get_list(0);
        let l2 = root.get_list(1);
        let l3 = List::new(mc).cons(mc, 0);
        l1.nconc(mc, l3);
        l1.print();
        l2.print();
    });

    // Use `nconc` to create a cyclic list. The good news is
    // that this will still be GC-ed when free: the GC deals
    // with cycles fine.
    arena.mutate(|mc, root| {
        let l2 = root.get_list(1);
        l2.nconc(mc, l2);
        l2.print();
    });
}
