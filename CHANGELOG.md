## [0.6.0]

Big release with a huge number of changes.

Much faster `DynamicRootSet` that does not require individual allocations per
root!

Non-object-safe `Collect` trait enables more optimization for automatic impls.

Explicit forward and backwards GC write barriers to enable more kinds of safe
container abstractions.

More precise control over GC phase.

Entirely new, much more tunable, much more correct GC pacing system.

Removes the idea of "external allocation" from collection entirely, as it was
simply too complex to reason about. Instead, users can use custom allocators
and directly adjust collection debt up and down based on recorded external
allocation. This direct adjustment of collection debt can also be use to
directly drive incremental collection independent of GC allocation.

### Release Highlights
* Implement `Ord`/`PartialOrd` for `CollectionPhase`.
* Let `Gc::cast`, `Gc::from_ptr` `GcWeak::cast` and `GcWeak::from_ptr` work with
  unsized types.
* Redesign `DynamicRootSet` to not require individual allocations per root.
* Remove `Collect`, `Sized`, and `'static` bounds on `Rootable::Root`.
* Add convenience `Gc::new_static`, `Gc::erase` and `GcWeak::erase` convenience
  methods.
* Make `StaticCollect<T>` work with unsized `T`.
* Add (unsafe) forward write barriers and explicit (unsafe) backwards write
  barriers to `Mutation`.
* Rename `CollectionPhase::Collecting` -> `CollectionPhase::Sweeping` and
  `StaticCollect` -> `Static`.
* Get rid of `unsafe_empty_collect!` macro.
* Slightly re-org crate exports and drop some special case re-exports from the
  crate root.
* Rename `MarkedArena::start_collecting` to `MarkedArena::start_sweeping` to
  match `CollectionPhase::Sweeping`
* Implement `Borrow<T>` for `Gc<'gc, T>`.
* Rework the `Collect` trait. Adds a `'gc` lifetime for correctness.
  `Collect::NEEDS_TRACE` associated constant allows for more optimization
  of tracing. No longer object safe! Adds a `Trace` trait for the trace
  implementation passed to `Collect::trace`.
* Use unsafe interior mutability to reduce internal `RefCell` locking in the
  collector.
* Update `hashbrown` to `0.16`.
* New, much more correct GC pacing system with many more tunable parameters.
* Add `Arena::cycle_debt` (equivalent of `Arena::collect_debt` which stops at
  the end of the GC cycle. and some method renames to aid phase tracking.
* Add the ability to add / subtract artificial debt from the GC pacing.
* Improvements to correctness around inheriting GC debt across cycles.
* Implement `Collect` for `BinaryHeap`.
* Add `OnceLock` (a GC-aware `OnceCell`).
* Add `{Deref, Index}Write` traits, for deref. and indexing behind write
  barriers.
* Migrate to Edition 2024 to fix `field!` macro unsoundness.
* Add a `DynCollect` trait and a way to safely GC custom dyn traits via a macro
  (replacement for lost `Collect` object safety).
* Add `Collect` impl for `enum_map::EnumMap` behind feature gate.
* Add `Collect` impl for `slotmap::SlotMap` behind feature gate.
* Add `Collect` impl for `smallvec::SmallVec` behind feature gate.
* `Collect` impl for `indexmap::IndexMap` and `indexmap::IndexSet` behind
  feature gate.
* Remove all notions of externally allocated bytes, `MetricsAlloc`, and
  `allocator-api2` dependency.

## [0.5.3]
* Adds a `Collect` impl for `hashbrown::HashTable`.

## [0.5.2]
* Add the ability to transition from a fully marked arena immediately into
  `CollectionPhase::Collecting`.

## [0.5.1]
* Correct the behavior of `Arena::mark_debt` and `Arena::mark_all`
  to do what their documentation suggest and do nothing during
  `CollectionPhase::Collecting`
* Implement `Collect` for `std::collections::LinkedList`
* Make `StaticCollect<T>` `#[repr(transparent)]`, to support sound casting from
  `Gc<StaticCollect<T>>` to `Gc<T>`.

## [0.5.0]

This release adds the concept of "finalization" to arenas. At the end of the
"mark" phase, at the moment that an arena is considered "fully marked" and
is ready to transition to the "sweep" phase, the "fully marked" arena can be
examined with some special powers...

`MarkedArena::finalize` can be called to check which pointers are considered
"dead" for this collection cycle. A "dead" pointer is one that is destined
(without further changes) to be freed during the next "sweep" phase. You can
"resurrect" these pointers either through normal root mutation before entering
the "sweep" phase, or more simply by calling explicit `resurrect` methods
on those pointers. Any resurrection, whether through mutation or `resurrect`
methods, immediately makes the arena no longer *fully* marked, but the arena can
then be re-marked and re-examined.

This simple, safe API (examining a "fully marked" arena for potentially dead
pointers, performing mutation, then potentially re-marking and re-examining)
is enough to implement quite complex garbage collector behavior. It can be used
to implement "finalizer" methods on objects, "ephemeron tables", Java-style
"reference queues" and more.

## Release Highlights
* New `Finalization` API.
* New API to query the current phase of the collector.
* Fixup the pacing algorithm for correctness and simplicity. Makes
  `Pacing::with_timing_factor(0.0)` actually work correctly for stop-the-world
  collection.
* `GcWeak::is_dropped` now works only during collection, not mutation.
* Add more trait impls to `StaticCollect`.
* `Gc::ptr_eq` and `GcWeak::ptr_eq` now ignore metadata of `dyn` pointers,
  matching the behavior of `Rc::ptr_eq` and `Arc::ptr_eq`.

## [0.4.0]

This release adds the ability to track *external* allocations (allocations
which are not stored in a `Gc<T>`) which also participate in pacing the garbage
collector. There is now a new (feature-gated) type `allocator_api::MetricsAlloc`
which implements the `allocator-api2::Allocator` trait which can be used to
automatically track the external allocation of collection types.

This release also adds (feature-gated) `tracing` support, which emits a span
per GC phase (propagate, sweep, drop), and events when collection resumes and
yields.

## Release Highlights
- Tracked external allocations API which participates in GC pacing.
- Feature-gated support for a `allocator_api2::Allocator` implementation that
  automatically tracks allocation.
- Feature-gated support for `hashbrown` types, to automatically implement
  `Collect` on them.
- Feature-gated `tracing` support.
- Implement `Collect` for `Box<T: ?Sized>`.
- Add methods to project `Write<Option<T>>` and `Write<Result<T, E>>`.
- Don't fail to build by trying to implement `Collect` on `Arc<T>` for platforms
  without `Arc`.

## [0.3.3]
- Actually pause for the configured amount of time in the gc, rather than the
  minimum.

## [0.3.2]
- Implement `Eq`, `PartialEq`, `Ord`, `PartialOrd`, and `Hash` traits on `Gc<T>`
  similar to the traits on std smart pointers like `Rc<T>`.
- Relax unnecessary bounds on `Collect` impls of std collections.
- Make `Arena::remembered_size()` return reasonable values.

## [0.3.1]
- Fallible `DynamicRootSet` API.

## [0.3.0]

An enormous number of breaking API changes, too many to list, almost the entire
API has been rethought.

The credit goes mostly to others for the release, @Bale001, @Aaron1011,
@dragazo, and especially @moulins.

### Release Highlights
- New `Arena` API that does not require macros and instead uses a `Rootable`
  trait and HRTBs for lifetime projection.
- Methods on `Arena` to directly mutate the root type and map the root from one
  type to another.
- A new API for 'static roots that are held in smart pointers
  (`DynamicRootSet`).
- `Gc` pointers can now point to DSTs, and there is an `unsize!` macro for
  unsizing coercions to replace the unstable `Unsize` trait.
- Weak pointers!
- `GcCell` has been replaced by explicit public lock types held within `Gc`
  pointers with safe ways of mutating them.
- Field projection on held lock types to allow for separate locks held within a
  single `Gc` pointer to be safely mutated.
- Unsafe `Gc` pointer coercions to compatible pointee types.
- Soundly get references to `Gc` types with `&'gc` lifetime.
- More ergonomic `Mutation` and `Collection` context types.
- *Tons* of correctness and soundness fixes.

This release also **completely drops** the `gc-sequence` combinator crate.
Because of other API changes, anything you could do with `gc-sequence` before
can almost certainly be expressed better either using the new 'static root API
or with the new map API. See [this comment](https://github.com/kyren/gc-arena/pull/50#issuecomment-1538421347) for a bit more info.

## [0.2.2]
- No changes, fixing a release snafu with cargo-release

## [0.2.1]
- Allow using `#[collect(require_static)]` on fields
- Add no_std compatibility for gc-arena and gc-sequence
- Add `Collect` impl for VecDeque`, `PhantomData`.
- Add `#[track_caller]` for better panic error messages
- Improve error messages for proc-macro derived `Collect` implementations
  substantially.
- Improve generated code for `Context::allocate` in release builds.

## [0.2]
- API incompatible change: depend on proc-macro2, quote, and syn 1.0
- API incompatible change: update synstructure to 0.12
- API incompatible change: use the trick from the `pin-project` crate to prevent
  types from implementing Drop by making it cause a conflicting impl of a
  `MustNotImplDrop` trait.
- API incompatible change: Add `#[collect(no_drop)]` and remove the `empty_drop`
  and `require_copy` versions since they are now less useful than `no_drop`.
