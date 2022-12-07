mod collect;
mod helpers;
mod mutable;

synstructure::decl_derive! {
    [Collect, attributes(collect)] =>
    /// Derives the `Collect` trait needed to trace a gc type.
    ///
    /// To derive `Collect`, an additional attribute is required on the struct/enum called
    /// `collect`. This has several optional arguments, but the only required argument is the derive
    /// strategy. This can be one of
    ///
    /// - `#[collect(require_static)]` - Adds a `'static` bound, which allows for a no-op trace
    ///   implementation. This is the ideal choice where possible.
    /// - `#[collect(no_drop)]` - The typical safe tracing derive strategy which only has to add a
    ///   requirement that your struct/enum does not have a custom implementation of `Drop`.
    /// - `#[collect(unsafe_drop)]` - The most versatile tracing derive strategy which allows a
    ///   custom drop implementation. However, this strategy can lead to unsoundness if care is not
    ///   taken (see the above explanation of `Drop` interactions).
    ///
    /// The `collect` attribute also accepts a number of optional configuration settings:
    ///
    /// - `#[collect(bound = "<code>")]` - Replaces the default generated `where` clause with the
    ///   given code. This can be an empty string to add no `where` clause, or otherwise must start
    ///   with `"where"`, e.g., `#[collect(bound = "where T: Collect")]`. Note that this option is
    ///   ignored for `require_static` mode since the only bound it produces is `Self: 'static`.
    ///   Also note that providing an explicit bound in this way is safe, and only changes the trait
    ///   bounds used to enable the implementation of `Collect`.
    ///
    /// Options may be passed to the `collect` attribute together, e.g., `#[collect(no_drop, bound
    /// = "")]`.
    ///
    /// The `collect` attribute may also be used on any field of an enum or struct, however the
    /// only allowed usage is to specify the strategy as `require_static` (no other strategies are
    /// allowed, and no optional settings can be specified). This will add a `'static` bound to the
    /// type of the field (regardless of an explicit `bound` setting) in exchange for not having
    /// to trace into the given field (the ideal choice where possible). Note that if the entire
    /// struct/enum is marked with `require_static` then this is unnecessary.
    collect::expand_derive
}

/// A derive macro for safe `Mutable` projections.
///
/// The macro will create the following static methods on the `derive`'d type:
/// ```rust,ignore
/// impl MyType {
///     fn project_mut<'a>(this: &'a Mutable<Self>) -> Projection<'a>;
///     // Only generated if your type implements `Collect`.
///     fn mutate_fields<'a, 'gc>(
///         mc: MutationContext<'gc, _>,
///         this: &'a Gc<'gc, Self>,
///     ) -> Projection<'a>;
/// }
/// ```
/// Where `Projection<'a>` is an auto-generated type created as follows:
///  - For every field marked with `#[mutable]`, the projection type will contain a `&'a Mutable<T>` reference;
///  - For every other field, the projection type will contain a normal `&'a T` reference.
///     - **Exception:** For struct with named fields, non-`#[mutable]` fields are skipped instead, and
///     `Deref<Target = Self>` is implemented on the projection type.
///
/// By default, the projection type is unnameable, but a name can be provided as an argument to the derive
/// macro (this is mandatory for `enum` types):
/// ```rust,ignore
/// #[derive(Mutable)]
/// #[mutable(name = "MyEnumMut")]
/// pub enum MyEnum<T> {
///     Foo(#[mutable] T),
///     Bar(String),
/// }
///
/// pub fn project_foo<T>(e: &Mutable<MyEnum<T>>) -> Option<&Mutable<T>> {
///     let e_mut: MyEnumMut<'_, T> = MyEnum::project_mut(e);
///     match e_mut {
///         MyEnumMut::Foo(v) => Some(v),
///         MyEnumMut::Bar(_) => None,
///     }
/// }
/// ```
///
/// The generated projection type and projection methods have the same visibility as the original type,
/// except that `pub` is downgraded to `pub(crate)`.
#[proc_macro_derive(Mutable, attributes(mutable, collect))]
pub fn derive_mutable(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    mutable::expand_derive(input).into()
}
