use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::spanned::Spanned;
use synstructure::{decl_derive, AddBounds};

fn collect_derive(mut s: synstructure::Structure) -> TokenStream {
    // Deriving `Collect` must be done with care, because an implementation of `Drop` is not
    // necessarily safe for `Collect` types.  This derive macro has three available modes to ensure
    // that this is safe:
    //   1) Require that the type be 'static with `#[collect(require_static)]`.
    //   2) Prohibit a `Drop` impl on the type with `#[collect(no_drop)]`
    //   3) Allow a custom `Drop` impl that might be unsafe with `#[collect(unsafe_drop)]`.  Such
    //      `Drop` impls must *not* access garbage collected pointers during `Drop::drop`.
    #[derive(PartialEq)]
    enum Mode {
        RequireStatic,
        NoDrop,
        UnsafeDrop,
    }

    macro_rules! usage_error {
        ($($t:tt)*) => {{
            let msg = format!($($t)*);
            panic!("{}. `#[collect(...)]` requires one mode (`require_static`, `no_drop`, or `unsafe_drop`) and optionally `bound = \"...\"`.", msg);
        }}
    }

    let mut mode = None;
    let mut override_bound = None;

    for attr in &s.ast().attrs {
        match attr.parse_meta() {
            Ok(syn::Meta::List(syn::MetaList { path, nested, .. })) => {
                if path.is_ident("collect") {
                    if mode.is_some() {
                        panic!("multiple `#[collect(...)]` attributes found on the same item. consider combining them.");
                    }

                    for nested in nested {
                        match nested {
                            syn::NestedMeta::Meta(syn::Meta::Path(path)) => {
                                if mode.is_some() {
                                    usage_error!("multiple modes specified");
                                }

                                if path.is_ident("require_static") {
                                    mode = Some(Mode::RequireStatic);
                                } else if path.is_ident("no_drop") {
                                    mode = Some(Mode::NoDrop);
                                } else if path.is_ident("unsafe_drop") {
                                    mode = Some(Mode::UnsafeDrop);
                                } else {
                                    usage_error!("unknown option")
                                }
                            }
                            syn::NestedMeta::Meta(syn::Meta::NameValue(value)) => {
                                if override_bound.is_some() {
                                    usage_error!("multiple bounds specified");
                                }

                                if value.path.is_ident("bound") {
                                    match value.lit {
                                        syn::Lit::Str(x) => override_bound = Some(x),
                                        _ => usage_error!("bound must be str"),
                                    }
                                } else {
                                    usage_error!("unknown option");
                                }
                            }
                            _ => usage_error!("unknown option"),
                        }
                    }

                    if mode.is_none() {
                        usage_error!("missing mode");
                    }
                }
            }
            _ => {}
        }
    }

    let mode = mode.unwrap_or_else(|| {
        usage_error!("deriving `Collect` requires a `#[collect(...)]` attribute")
    });

    let where_clause = if mode == Mode::RequireStatic {
        quote!(where Self: 'static)
    } else {
        override_bound
            .as_ref()
            .map(|x| {
                x.parse()
                    .expect("`#[collect]` failed to parse explicit trait bound expression")
            })
            .unwrap_or_else(|| quote!())
    };

    let mut errors = vec![];

    let collect_impl = if mode == Mode::RequireStatic {
        s.clone().add_bounds(AddBounds::None).gen_impl(quote! {
            gen unsafe impl gc_arena::Collect for @Self #where_clause {
                #[inline]
                fn needs_trace() -> bool {
                    false
                }
            }
        })
    } else {
        let mut needs_trace_body = TokenStream::new();
        quote!(false).to_tokens(&mut needs_trace_body);

        let mut static_bindings = vec![];

        // Ignore all bindings that have `#[collect(require_static)]`
        // For each binding with `#[collect(require_static)]`, we
        // push a bound of the form `FieldType: 'static` to `static_bindings`,
        // which will be added to the genererated `Collect` impl.
        // The presence of the bound guarantees that the field cannot hold
        // any `Gc` pointers, so it's safe to ignore that field in `needs_trace`
        // and `trace`
        s.filter(|b| {
            let mut static_binding = false;
            let mut seen_collect = false;
            for attr in &b.ast().attrs {
                match attr.parse_meta() {
                    Ok(syn::Meta::List(syn::MetaList { path, nested, .. })) => {
                        if path.is_ident("collect") {
                            if seen_collect {
                                errors.push(
                                    syn::parse::Error::new(
                                        path.span(),
                                        "Cannot specify multiple `#[collect]` attributes!",
                                    )
                                    .to_compile_error(),
                                );
                            }
                            seen_collect = true;

                            let mut valid = false;
                            if let Some(syn::NestedMeta::Meta(syn::Meta::Path(path))) =
                                nested.first()
                            {
                                if path.is_ident("require_static") {
                                    static_binding = true;
                                    static_bindings.push(b.ast().ty.clone());
                                    valid = true;
                                }
                            }

                            if !valid {
                                errors.push(
                                    syn::parse::Error::new(
                                        nested.span(),
                                        "Only `#[collect(require_static)]` is supported on a field",
                                    )
                                    .to_compile_error(),
                                );
                            }
                        }
                    }
                    _ => {}
                }
            }
            !static_binding
        });

        for static_binding in static_bindings {
            s.add_where_predicate(syn::parse_quote! { #static_binding: 'static });
        }

        // `#[collect(require_static)]` only makes sense on fields, not enum
        // variants. Emit an error if it is used in the wrong place
        if let syn::Data::Enum(..) = s.ast().data {
            for v in s.variants() {
                for attr in v.ast().attrs {
                    match attr.parse_meta() {
                        Ok(syn::Meta::List(syn::MetaList { path, nested, .. })) => {
                            if path.is_ident("collect") {
                                errors.push(
                                    syn::parse::Error::new(
                                        nested.span(),
                                        "`#[collect]` is not suppported on enum variants",
                                    )
                                    .to_compile_error(),
                                );
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        // We've already called `s.filter`, so we we won't try to call
        // `needs_trace` for the types of fields that have `#[collect(require_static)]`
        for v in s.variants() {
            for b in v.bindings() {
                let ty = &b.ast().ty;
                // Resolving the span at the call site makes rustc
                // emit a 'the error originates a derive macro note'
                // We only use this span on tokens that need to resolve
                // to items (e.g. `gc_arena::Collect`), so this won't
                // cause any hygiene issues
                let call_span = b.ast().span().resolved_at(Span::call_site());
                quote_spanned!(call_span=>
                    || <#ty as gc_arena::Collect>::needs_trace()
                )
                .to_tokens(&mut needs_trace_body);
            }
        }
        // Likewise, this will skip any fields that have `#[collect(require_static)]`
        let trace_body = s.each(|bi| {
            // See the above call to `needs_trace` for an explanation of this
            let call_span = bi.ast().span().resolved_at(Span::call_site());
            quote_spanned!(call_span=>
                {
                    // Use a temporary variable to ensure that
                    // all tokens in the call to `gc_arena::Collect::trace`
                    // have the same hygiene information. If we used #bi
                    // directly, then we would have a mix of hygiene contexts,
                    // which would cause rustc to produce sub-optimal error
                    // messagse due to its inability to merge the spans.
                    // This is purely for diagnostic purposes, and has no effect
                    // on correctness
                    let bi = #bi;
                    gc_arena::Collect::trace(bi, cc)
                }
            )
        });

        let bounds_type = if override_bound.is_some() {
            AddBounds::None
        } else {
            AddBounds::Generics
        };
        s.clone().add_bounds(bounds_type).gen_impl(quote! {
            gen unsafe impl gc_arena::Collect for @Self #where_clause {
                #[inline]
                fn needs_trace() -> bool {
                    #needs_trace_body
                }

                #[inline]
                fn trace(&self, cc: ::gc_arena::CollectionContext) {
                    match *self { #trace_body }
                }
            }
        })
    };

    let drop_impl = if mode == Mode::NoDrop {
        let mut s = s;
        s.add_bounds(AddBounds::None).gen_impl(quote! {
            gen impl gc_arena::MustNotImplDrop for @Self {}
        })
    } else {
        quote!()
    };

    quote! {
        #collect_impl
        #drop_impl
        #(#errors)*
    }
}

decl_derive! {
    [Collect, attributes(collect)] =>
    /// Derives the `Collect` trait needed to trace a gc type.
    ///
    /// To derive `Collect`, an additional attribute is required on the struct/enum called `collect`.
    /// This has several optional arguments, but the only required argument is the derive strategy.
    /// This can be one of
    ///
    /// - `#[collect(require_static)]` - Adds a `'static` bound, which allows for a no-op trace implementation.
    ///   This is the ideal choice where possible.
    /// - `#[collect(no_drop)]` - The typical safe tracing derive strategy which only has to add a requirement
    ///   that your struct/enum does not have a custom implementation of `Drop`.
    /// - `#[collect(unsafe_drop)]` - The most versatile tracing derive strategy which allows a custom drop implementation.
    ///   However, this strategy can lead to unsoundness if care is not taken (see the above explanation of `Drop` interactions).
    ///
    /// The `collect` attribute also accepts a number of optional configuration settings:
    ///
    /// - `#[collect(bound = "<code>")]` - Replaces the default generated `where` clause with the given code.
    ///   This can be an empty string to add no `where` clause, or otherwise must start with `"where"`,
    ///   e.g., `#[collect(bound = "where T: Collect")]`.
    ///   Note that this option is ignored for `require_static` mode since the only bound it produces is `Self: 'static`.
    ///   Also note that providing an explicit bound in this way is safe, and only changes the trait bounds used
    ///   to enable the implementation of `Collect`.
    ///
    /// Options may be passed to the `collect` attribute together, e.g., `#[collect(no_drop, bound = "")]`.
    ///
    /// The `collect` attribute may also be used on any field of an enum or struct, however the only allowed usage
    /// is to specify the strategy as `require_static` (no other strategies are allowed, and no optional settings can be specified).
    /// This will add a `'static` bound to the type of the field (regardless of an explicit `bound` setting)
    /// in exchange for not having to trace into the given field (the ideal choice where possible).
    /// Note that if the entire struct/enum is marked with `require_static` then this is unnecessary.
    collect_derive
}
