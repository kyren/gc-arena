use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::{parse_quote, parse_quote_spanned, spanned::Spanned};

use crate::helpers::{
    find_zero_or_one_attr, parse_meta_list, punctuated_retain_mut, ErrorSink, MetaEntry, MetaValue,
};

fn parse_unique_entry<T>(
    entry: &MetaEntry,
    slot: &mut Option<T>,
    value: impl FnOnce() -> syn::Result<T>,
) -> Result<(), syn::Error> {
    if slot.is_some() {
        Err(syn::Error::new(
            entry.span,
            format_args!("multiple {}s specified", entry.name),
        ))
    } else {
        value().map(|v| *slot = Some(v))
    }
}

struct TopLevelAttrs {
    name: Option<syn::Ident>,
    impl_collect: bool,
}

fn parse_top_level_attrs(sink: &mut ErrorSink, attrs: &[syn::Attribute]) -> TopLevelAttrs {
    let mut name = None;

    let metas = find_zero_or_one_attr(sink, attrs, ATTR_NAME)
        .and_then(|a| sink.ok(parse_meta_list(a, false)))
        .unwrap_or_default();

    // Assume this type implements Collect if we have some collect attributes;
    // a little hacky but it works.
    let impl_collect = attrs.iter().any(|a| a.path.is_ident("collect"));

    for e in metas.into_iter().map(MetaEntry::from) {
        match (e.name.as_str(), &e.value) {
            ("name", MetaValue::Lit(syn::Lit::Str(s))) => {
                sink.ok(parse_unique_entry(&e, &mut name, || s.parse()));
            }
            _ => sink.error(e.span, "unknown option"),
        }
    }

    TopLevelAttrs { name, impl_collect }
}

fn parse_enum_variant_attrs(sink: &mut ErrorSink, attrs: &[syn::Attribute]) {
    if let Some(attr) = attrs.iter().find(|a| a.path.is_ident(ATTR_NAME)) {
        sink.error(
            attr.span(),
            format_args!("`#[{ATTR_NAME}]` isn't allowed here"),
        );
    }
}

fn parse_field_attrs(sink: &mut ErrorSink, attrs: &[syn::Attribute]) -> Option<()> {
    let metas = find_zero_or_one_attr(sink, attrs, ATTR_NAME)
        .and_then(|a| sink.ok(parse_meta_list(a, true)))?;

    for e in metas.into_iter().map(MetaEntry::from) {
        // No allowed options for fields.
        sink.error(e.span, "unknown option");
    }

    Some(())
}

struct CommonConfig {
    vis: syn::Visibility,
    generics: syn::Generics,
    ty: syn::Ident,
    fields_ty: syn::Ident,
    fields_lt: syn::Lifetime,
    expose_to_module: bool,
    gc_lt: syn::Lifetime,
    impl_collect: bool,
}

fn common_config(sink: &mut ErrorSink, input: syn::DeriveInput) -> (CommonConfig, syn::Data) {
    let attrs = parse_top_level_attrs(sink, &input.attrs);

    // Helper for choosing a free lifetime name.
    let choose_lifetime = |name: &str| -> syn::Lifetime {
        let mut name = name.to_string();
        while input
            .generics
            .lifetimes()
            .any(|lt| lt.lifetime.ident == name)
        {
            name.insert(0, '_');
        }
        name.insert(0, '\'');
        syn::Lifetime::new(&name, Span::call_site())
    };

    // Choose the name of the generated field type.
    let (expose_to_module, fields_ty) = if let Some(name) = attrs.name {
        (true, name)
    } else {
        (false, quote::format_ident!("__{}Mut", input.ident))
    };

    let config = CommonConfig {
        expose_to_module,
        fields_ty,
        fields_lt: choose_lifetime("fields"),
        gc_lt: choose_lifetime("gc"),
        generics: input.generics,
        ty: input.ident,
        // Downgrade `pub` to `pub(crate)`
        vis: match input.vis {
            syn::Visibility::Public(v) => parse_quote_spanned!(v.span() => pub(crate)),
            other => other,
        },
        impl_collect: attrs.impl_collect,
    };
    (config, input.data)
}

// Mutable fields (`T`) are coerced to `&'fields Mutable<T>`.
fn wrap_mutable_field(
    field: &mut syn::Field,
    binding: &syn::Ident,
    lt: &syn::Lifetime,
) -> TokenStream {
    let ty = &field.ty;
    let expr = quote!({
        // This will fail to compile if the field isn't of the type we expect.
        let _ = &(#binding,) as *const (&#ty,);
        // Safe, as we know both initial and final types, so no Deref coercion can happen.
        unsafe { ::gc_arena::cell::Mutable::<#ty>::assume(#binding) }
    });

    field.ty = parse_quote!(&#lt ::gc_arena::cell::Mutable<#ty>);
    expr
}

// Immutable fields stay simple references.
fn wrap_immutable_field(field: &mut syn::Field, lt: &syn::Lifetime) {
    let ty = &field.ty;
    field.ty = parse_quote!(&#lt #ty);
}

type FieldsList = syn::punctuated::Punctuated<syn::Field, syn::Token![,]>;

#[derive(Default)]
struct VariantParts {
    field_pats: TokenStream,
    field_exprs: TokenStream,
    has_mutable: bool,
}

fn wrap_variant_fields(
    sink: &mut ErrorSink,
    fields: &mut FieldsList,
    config: &CommonConfig,
    self_field: Option<syn::Ident>,
) -> VariantParts {
    let mut pats = TokenStream::new();
    let mut exprs = TokenStream::new();

    let mut has_mutable = false;
    let mut id = 0;
    punctuated_retain_mut(fields, |f| {
        id += 1;
        let is_mutable = parse_field_attrs(sink, &f.attrs).is_some();
        // Clear attributes, we don't want them in the generated type.
        f.attrs.clear();

        // Skip immutable fields if a self is added.
        if !is_mutable && self_field.is_some() {
            return false;
        }

        // Choose the binding name.
        let binding = f
            .ident
            .clone()
            .unwrap_or_else(|| quote::format_ident!("__{}", id, span = f.span()));

        let span = binding.span();
        let binding_toks = quote_spanned!(span => #binding,);
        binding_toks.to_tokens(&mut pats);

        // Convert the field to its new type.
        if is_mutable {
            let expr = wrap_mutable_field(f, &binding, &config.fields_lt);
            if let Some(name) = &f.ident {
                quote_spanned!(span => #name:).to_tokens(&mut exprs);
            }
            quote_spanned!(span => #expr,).to_tokens(&mut exprs);
            has_mutable = true;
        } else {
            wrap_immutable_field(f, &config.fields_lt);
            binding_toks.to_tokens(&mut exprs);
        }

        true
    });

    // Insert `..` if we skipped fields.
    if fields.len() < id {
        syn::Token![..](fields.span()).to_tokens(&mut pats);
    }

    // Insert the self field (for Deref impls).
    if let Some(self_name) = self_field {
        let (ty, generics, lt) = (&config.ty, &config.generics, &config.fields_lt);
        let (_, generics, _) = generics.split_for_impl();
        let span = self_name.span();
        quote_spanned!(span => #self_name: this).to_tokens(&mut exprs);

        fields.push(syn::Field {
            attrs: vec![parse_quote!(#[doc(hidden)])],
            vis: syn::Visibility::Inherited,
            ident: Some(self_name),
            colon_token: Some(syn::Token![:](span)),
            ty: syn::parse_quote!(&#lt #ty #generics),
        });
    }

    VariantParts {
        field_pats: pats,
        field_exprs: exprs,
        has_mutable,
    }
}

fn wrap_variant(
    sink: &mut ErrorSink,
    fields: &mut syn::Fields,
    config: &CommonConfig,
    enum_variant: Option<&syn::Ident>,
) -> (TokenStream, bool) {
    use proc_macro2::{Delimiter, Group, TokenTree};

    let mut lhs = config.ty.to_token_stream();
    let mut rhs = config.fields_ty.to_token_stream();
    if let Some(variant) = enum_variant {
        let extra = quote!(::#variant);
        extra.to_tokens(&mut lhs);
        extra.to_tokens(&mut rhs);
    }

    let (parts, delim) = match fields {
        syn::Fields::Unit => (VariantParts::default(), Delimiter::None),
        syn::Fields::Named(f) => {
            // Only add a self field for structs with named fields.
            let self_field = if enum_variant.is_some() {
                None
            } else {
                Some(syn::Ident::new(SELF_FIELD_NAME, config.ty.span()))
            };
            let parts = wrap_variant_fields(sink, &mut f.named, config, self_field);
            (parts, Delimiter::Brace)
        }
        syn::Fields::Unnamed(f) => {
            let parts = wrap_variant_fields(sink, &mut f.unnamed, config, None);
            (parts, Delimiter::Parenthesis)
        }
    };

    if delim != Delimiter::None {
        fn group_to_toks(grp: Group) -> impl Iterator<Item = TokenTree> {
            std::iter::once(grp.into())
        }
        lhs.extend(group_to_toks(Group::new(delim, parts.field_pats)));
        rhs.extend(group_to_toks(Group::new(delim, parts.field_exprs)));
    }

    (quote!(#lhs => #rhs,), parts.has_mutable)
}

const SELF_FIELD_NAME: &str = "__self";
const ATTR_NAME: &str = "mutable";
const MACRO_NAME: &str = "#[derive(Mutable)]";

pub(crate) fn expand_derive(input: syn::DeriveInput) -> TokenStream {
    let mut sink = ErrorSink::new();
    let (mut config, mut data) = common_config(&mut sink, input);

    let mut has_deref_impl = false;
    let (match_body, has_mutable) = match &mut data {
        syn::Data::Union(_) => {
            sink.error(
                Span::call_site(),
                format_args!("`{MACRO_NAME}` isn't supported on unions"),
            );
            return sink.into_tokens();
        }
        syn::Data::Struct(data) => {
            has_deref_impl = matches!(data.fields, syn::Fields::Named(_));
            wrap_variant(&mut sink, &mut data.fields, &config, None)
        }
        syn::Data::Enum(data) => {
            if !config.expose_to_module {
                sink.error(
                    Span::call_site(),
                    format_args!(
                        "`{MACRO_NAME}` on enums requires `#[{ATTR_NAME}(name = \"...\")]`"
                    ),
                );
            }
            data.variants
                .iter_mut()
                .fold((TokenStream::new(), false), |mut state, variant| {
                    parse_enum_variant_attrs(&mut sink, &variant.attrs);
                    // Clear attributes, we don't want them in the generated type.
                    variant.attrs.clear();
                    let (arm, has_mut) = wrap_variant(
                        &mut sink,
                        &mut variant.fields,
                        &config,
                        Some(&variant.ident),
                    );
                    state.0.extend(arm);
                    (state.0, state.1 | has_mut)
                })
        }
    };

    if !has_mutable {
        sink.error(
            Span::call_site(),
            format_args!("`{MACRO_NAME}` requires at least one `#[{ATTR_NAME}]` field"),
        );
    }

    let errors = sink.into_tokens();
    if !errors.is_empty() {
        return errors;
    }

    let base_generics = config.generics.clone();
    config
        .generics
        .params
        .insert(0, syn::LifetimeDef::new(config.fields_lt.clone()).into());

    let (vis, ty, fields_ty) = (&config.vis, &config.ty, &config.fields_ty);
    let (fields_lt, gc_lt) = (&config.fields_lt, &config.gc_lt);
    let (gen_impl, gen_ty, where_clause) = base_generics.split_for_impl();
    let (gen_impl_f, gen_ty_f, where_clause_f) = config.generics.split_for_impl();

    let mut impl_body = quote! {
        /// Projects out the mutable fields of `Self`.
        #[allow(unused)]
        #[inline(always)]
        #vis fn project_mut<#fields_lt>(
            this: &#fields_lt ::gc_arena::cell::Mutable<Self>
        ) -> #fields_ty #gen_ty_f {
            match ::core::ops::Deref::deref(this) {
                #match_body
            }
        }
    };

    // Only add the `mutate_fields` method if `Self: Collect` isn't trivially
    // unsatisfiable, to avoid spurious errors.
    if config.impl_collect {
        let mutate_fields = quote! {
            /// Projects out the mutable fields of a `Gc<'_, Self>`.
            #[allow(unused)]
            #[inline(always)]
            #vis fn mutate_fields<#fields_lt, #gc_lt>(
                mc: ::gc_arena::MutationContext<#gc_lt, '_>,
                this: &#fields_lt ::gc_arena::Gc<#gc_lt, Self>,
            ) -> #fields_ty #gen_ty_f where Self: ::gc_arena::Collect {
                Self::project_mut(::gc_arena::Gc::mutate(mc, this))
            }
        };
        mutate_fields.to_tokens(&mut impl_body);
    }

    let deref_impl = if has_deref_impl {
        Some(quote! {
            impl #gen_impl_f ::core::ops::Deref for #fields_ty #gen_ty_f #where_clause_f {
                type Target = #ty #gen_ty;
                #[inline(always)]
                fn deref(&self) -> &Self::Target {
                    &self.__self
                }
            }
        })
    } else {
        None
    };

    let fields_ty = syn::DeriveInput {
        attrs: Vec::new(),
        vis: config.vis,
        ident: config.fields_ty,
        generics: config.generics,
        data,
    };

    let out = quote! {
        #[derive(Copy, Clone)]
        #fields_ty
        #deref_impl
        impl #gen_impl #ty #gen_ty #where_clause {
            #impl_body
        }
    };

    if config.expose_to_module {
        out
    } else {
        quote! { const _: () = { #out }; }
    }
}
