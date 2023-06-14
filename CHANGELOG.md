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
- API incompatible chagne: update synstructure to 0.12
- API incompatible change: use the trick from the `pin-project` crate to prevent
  types from implementing Drop by making it cause a conflicting impl of a
  `MustNotImplDrop` trait.
- API incompatible change: Add `#[collect(no_drop)]` and remove the `empty_drop`
  and `require_copy` versions since they are now less useful than `no_drop`.
