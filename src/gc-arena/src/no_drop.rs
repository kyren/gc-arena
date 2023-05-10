// Trait that is automatically implemented for all types that implement `Drop`.
//
// Used to cause a conflicting trait impl if a type implements `Drop` to forbid implementing `Drop`.
#[doc(hidden)]
pub trait __MustNotImplDrop {}

#[allow(drop_bounds)]
impl<T: Drop> __MustNotImplDrop for T {}
