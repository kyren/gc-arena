# gc-arena and gc-sequence
 
This repo is home to the `gc-arena` and `gc-sequence` crates, which provide Rust
with garbage collected arenas and a means of safely interacting with them.

These crates are still fairly experimental and WIP, but they do work and provide
genuinely safe garbage collected pointers.

## gc-arena

The `gc-arena` crate, along with its helper crate `gc-arena-derive`, provides a
safe system for simple garbage collected allocation arenas.  There are two
techniques at play that make this system sound:

* Garbage collected objects are traced using the `Collect` trait, which must be
  implemented correctly to ensure that all reachable objects are found.  This
  trait is therefore `unsafe`, but it *can* safely be implemented by procedural
  macro, and the `gc-arena-derive` provides such a safe procedural macro.

* In order for garbage collection to take place, the garbage collector must
  first have a list of "root" objects which are known to be reachable.  In our
  case, the user of `gc-arena` chooses a single root object for the arena, but
  this is not sufficient for safe garbage collection.  If garbage collection
  were to take place when there are garbage collected pointers anywhere on the
  Rust stack, such pointers would also need to be considered as "root" objects
  to prevent memory unsafety.  `gc-arena` solves this by strictly limiting where
  garbage collected pointers can be stored, and when they can be alive.  The
  arena can only be accessed through a single `mutate` method which takes a
  callback, and all garbage collected pointers inside this callback are branded
  with an invariant lifetime which is unique to that single callback call.
  Thus, when outside of this `mutate` method, the rust borrow checker ensures
  that it is not possible for garbage collected pointers to be alive anywhere on
  the stack, nor is it possible for them to have been smuggled outside of the
  arena's root object.  Since all pointers can be proven to be reachable from
  the single root object, safe garbage collection can take place.
  
In other words, the `gc-arena` crate does *not* retrofit Rust with a globally
accessible garbage collector, rather it *only* allows for limited garbage
collection in isolated garbage collected arenas.  All garbage collected pointers
must forever live inside only this arena, and pointers from different arenas are
prevented from being stored in the wrong arena.

## gc-sequence

The `gc-arena` crate, while still useful, is very limiting.  Since garbage
collection can only take place *between* calls to `mutate`, arbitrarily long
calls to `mutate` could prevent garbage collection for an arbitrary amount of
time.

`gc-sequence` attempts to ease this problem by supplying a `futures`-like API on
top of garbage collected arenas.  It allows users to build up `futures`-like
state machines, using combinators, which are driven forward by repeatedly
calling `Sequence::step`.  The resulting `Sequence` state machines themselves
implement `Collect`, so the provided arena wrapper can safely execute
arbitrarily long running `Sequence`s, garbage collecting in-between calls to
`Sequence::step`.  In this way, the `Sequence` trait becomes a way to express
garbage collector "safe points", at which collection can safely take place.

## Use cases

These crates were developed primarily as a means of writing VMs for garbage
collected languages in safe Rust, but there are probably many more uses than
just this.

## Current status and TODOs

Currently, these crates are WIP and experimental, but are basically usable and
safe.  Some notable current limitations:

* While they contain no *pathalogical* slowness, they are not highly optimized
  currently.  The garbage collector in the `gc-arena` crate is a basic
  incremental mark-and-sweep collector which by itself is not *terrible*, but
  there is still a lot of unnecessary space overhead per-allocation.
  Additionally, there is not currently a way to allocate DSTs in a Gc pointer,
  instead requiring very slow double indirection for any non-`Sized` type.  Both
  of these problems are very solvable, but this work hasn't been done yet.
  
* There is currently no system for object finalization.  This is not terribly
  difficult to implement, depending on the system, but it would require picking
  a particular set of edge-case finalization behavior.
  
* There are lots of missing `Sequence` combinators, and there is currently no
  way to conveniently constructing looping `Sequence`s.  This is easily
  solvable, but not currently done.
  
* A harder to solve limitation is that there is currently no system for
  multi-threaded allocation and collection.  The basic lifetime and safety
  techniques here would still work in an arena supprinting multi-threading, but
  these crates do not support this.
  
* Another limitiation is that the `Collect` trait does not provide a mechanism
  to move pointers once they are allocated, so this limits the types of
  collectors that could be written.
  
* The `gc-sequence` crate is inconvenient to use, as it requires combinators to
  build up `Sequence`s.  This could be made vastly easier with the help of Rust
  generators, BUT there would still be the limitation that generators would not
  automatically implement `Collect`.  Therefore, even with generator support, it
  would still require avoiding keeping garbage collected values kept inside the
  generators, limiting their usefulness.  If it were ever possible to implement
  `Collect` on generators automatically through some as-yet-undesigned rust
  feature, this would greatly improve the convenience and viability of the
  techniques in these crates in general.

## Prior Art

The ideas here are mostly not mine, much of this was heavily derived
