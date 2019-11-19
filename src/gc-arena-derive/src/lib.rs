use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens};
use synstructure::{decl_derive, AddBounds};

fn collect_derive(s: synstructure::Structure) -> TokenStream {
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

    let mut mode = None;

    for attr in &s.ast().attrs {
        match attr.interpret_meta() {
            Some(syn::Meta::List(syn::MetaList { ident, nested, .. })) => {
                if ident == Ident::new("collect", Span::call_site()) {
                    if let Some(prev_mode) = mode {
                        let prev_mode_str = match prev_mode {
                            Mode::RequireStatic => "require_static",
                            Mode::NoDrop => "no_drop",
                            Mode::UnsafeDrop => "unsafe_drop",
                        };
                        panic!("`Collect` mode was already specified with `#[collect({})]`, cannot specify twice", prev_mode_str);
                    }

                    if let Some(syn::punctuated::Pair::End(nmeta)) = nested.first() {
                        if nmeta
                            == &syn::NestedMeta::Meta(syn::Meta::Word(Ident::new(
                                "require_static",
                                Span::call_site(),
                            )))
                        {
                            mode = Some(Mode::RequireStatic);
                        } else if nmeta
                            == &syn::NestedMeta::Meta(syn::Meta::Word(Ident::new(
                                "no_drop",
                                Span::call_site(),
                            )))
                        {
                            mode = Some(Mode::NoDrop);
                        } else if nmeta
                            == &syn::NestedMeta::Meta(syn::Meta::Word(Ident::new(
                                "unsafe_drop",
                                Span::call_site(),
                            )))
                        {
                            mode = Some(Mode::UnsafeDrop);
                        } else {
                            panic!("`#[collect]` requires one of: \"require_static\", \"no_drop\", or \"unsafe_drop\" as an argument");
                        }
                    }
                }
            }
            _ => {}
        }
    }

    let mode = mode.expect("deriving `Collect` requires a `#[collect(<mode>)]` attribute, where `<mode>` is one of \"require_static\", \"no_drop\", or \"unsafe_drop\"");

    let where_clause = if mode == Mode::RequireStatic {
        quote!(where Self: 'static)
    } else {
        quote!()
    };

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
        for v in s.variants() {
            for b in v.bindings() {
                let ty = &b.ast().ty;
                quote!(|| <#ty as gc_arena::Collect>::needs_trace())
                    .to_tokens(&mut needs_trace_body);
            }
        }

        let trace_body = s.each(|bi| quote!(gc_arena::Collect::trace(#bi, cc)));

        s.clone().add_bounds(AddBounds::Fields).gen_impl(quote! {
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
            gen impl gc_arena::no_drop::MustNotImplDrop for @Self {}
        })
    } else {
        quote!()
    };

    quote! {
        #collect_impl
        #drop_impl
    }
}

decl_derive!([Collect, attributes(collect)] => collect_derive);
