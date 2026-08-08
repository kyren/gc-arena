[![crates.io](https://img.shields.io/crates/v/gc-arena)](https://crates.io/crates/gc-arena)
[![docs.rs](https://docs.rs/gc-arena/badge.svg)](https://docs.rs/gc-arena)
[![Build Status](https://img.shields.io/circleci/project/github/kyren/gc-arena.svg)](https://circleci.com/gh/kyren/gc-arena)

## gc-arena

This repo is home to the `gc-arena` crate, which provides Rust with garbage
collected arenas and a means of safely interacting with them.

The `gc-arena` crate, along with its helper crate `gc-arena-derive`, provides
allocation with safe, incremental, exact, cycle-detecting garbage collection
within a closed "arena".

These garbage collected arenas are safe through the use of three main techniques:

1) The `Collect` trait is used to implement exact tracing. This trait is unsafe
   and implementing it incorrectly can lead to UB, but the trait can be (and
   almost always is) safely derived via proc-macro.

2) Pointers are branded with a unique generative `'gc` lifetime that ensures
   that they cannot escape mutation callbacks or be assigned to the wrong arena.

3) "Mutation xor collection" -- Either `Arena::mutate` is executing or
  `Arena::collect` is executing, but never both at the same time. This way,
  we know that there are no untraceable objects on the stack when we call
  `Arena::collect`.

See [this blog post](https://kyju.org/blog/rust-safe-garbage-collection/) for
an in-depth tour of the crate's design. It is quite dense, but it explains
everything necessary to fully understand the machinery used in the included
[linked list example](examples/linked_list.rs).

## Current status

Basically usable and safe! It is used by the Adobe Flash Player
emulator [Ruffle](https://github.com/ruffle-rs/ruffle) for its
ActionScript VM as well my own GameMaker compatible compiler / VM
[fabricator](https://github.com/kyren/fabricator) which is used by the game
[Fields of Mistria](https://www.fieldsofmistria.com/).

Collection is implemented using an incremental mark-and-sweep algorithm very
similar to the one in PUC-Rio Lua, and is optimized primarily for low pause
time. During mutation, allocation "debt" is accumulated, and this "debt"
determines the amount of work that the next call to `Arena::collect` will do.

The pointers held in arenas (spelled `Gc<'gc, T>`) are zero-cost newtypes around
`*const T` that implement `Copy`. No pointer bookkeeping at all is necessary as
`Gc`s are moved or copied during mutation.

There is robust support for allocating slices, `str`s, and other DSTs
(Dynamically Sized Types) directly in `Gc` pointers. `Gc` pointers to DSTs can
also have either a "fat" or "thin" representation, the second of which reads
pointer metadata from the GC object header.

## Use cases and "mutation xor collection"

The "mutation xor collection" design means that `Arenae::mutate` must *return*
for collection to be performed, and thus code that uses GC objects cannot
be running while collection takes place. This is not as much of a problem as
it sounds like since there is a built-in mechanism for rooted `Gc` pointers that
is fairly cheap, and with rooted pointers the crate can be used more from the
"outside" than from within `Arena::mutate`, but it is still a major limitation
to consider. When used from the "outside" this way, the crate is more like
other garbage collector systems for Rust that primarily use rooted pointers
that are not zero-cost, and `Arena::mutate` then acts as an "accelerator" for
as-cheap-as-possible pointer manipulation.

Sometimes "mutation xor collection" is not even a problem though! Video games
(or programs that are similar to video games) generally want to run VM code that
returns once per frame and then also run some garbage collection once per frame.
If you are writing a VM that will primarily be used in that way, this crate
may be perfect for you! This was the original use case for this crate and what
initially formed its design -- writing fast VMs for scripting languages that are
used in video games in safe Rust.

### Some other notable limitations:

* Generators (or just plain `async`) can be used, with some effort, to overcome
  the limitation that collection cannot be run while GC code is running. This
  would be *much* easier if it were possible to generate `Collect` impls for
  generator state machines, but there is currently no way to do this in Rust nor
  any planned feature that would enable it.

* This crate is designed for single-threaded use and multiple, independent
  arenas. It has no support for multi-threaded mutation or collection.

* There is currently no mechanism for moving allocated objects or heap
  compaction.

## Prior Art

The ideas here are mostly not mine. Much of the user-facing design is borrowed
heavily from [rust-gc](
https://manishearth.github.io/blog/2015/09/01/designing-a-gc-in-rust/),
and the idea of using "generativity" comes from [You can't spell trust without
Rust](https://raw.githubusercontent.com/Gankro/thesis/master/thesis.pdf). The
design of the actual garbage collection system itself borrows heavily from the
incremental mark-and-sweep collector in Lua 5.4.

## License

Everything in this repository is licensed under either of:

* MIT license [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
* Creative Commons CC0 1.0 Universal Public Domain Dedication
  [LICENSE-CC0](LICENSE-CC0) or
  https://creativecommons.org/publicdomain/zero/1.0/

at your option.
