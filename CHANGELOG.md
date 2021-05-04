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
