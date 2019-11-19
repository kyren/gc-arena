pub trait MustNotImplDrop {}

impl<T: Drop> MustNotImplDrop for T {}
