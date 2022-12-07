use std::fmt::Display;

use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::{punctuated::Punctuated, spanned::Spanned};

#[derive(Default)]
pub struct ErrorSink {
    out: TokenStream,
}

impl ErrorSink {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn into_tokens(self) -> TokenStream {
        self.out
    }

    pub fn error(&mut self, span: Span, msg: impl Display) {
        let error = syn::Error::new(span, msg);
        error.to_compile_error().to_tokens(&mut self.out)
    }

    pub fn ok<R>(&mut self, result: syn::Result<R>) -> Option<R> {
        match result {
            Ok(v) => Some(v),
            Err(e) => {
                e.to_compile_error().to_tokens(&mut self.out);
                None
            }
        }
    }
}

pub fn punctuated_retain_mut<T, P>(
    punct: &mut Punctuated<T, P>,
    mut f: impl FnMut(&mut T) -> bool,
) {
    *punct = std::mem::take(punct)
        .into_pairs()
        .filter_map(|mut pair| {
            if f(pair.value_mut()) {
                Some(pair)
            } else {
                None
            }
        })
        .collect();
}

pub fn find_zero_or_one_attr<'a>(
    sink: &mut ErrorSink,
    attrs: &'a [syn::Attribute],
    name: &str,
) -> Option<&'a syn::Attribute> {
    let mut found_dup = false;
    let mut found = None;
    for attr in attrs {
        if !attr.path.is_ident(name) {
            continue;
        }

        if found.is_some() {
            if !found_dup {
                found_dup = true;
                sink.error(
                    attr.path.span(),
                    format_args!("duplicate `#[{}(...)]` attribute", name),
                );
            }
        } else {
            found = Some(attr);
        }
    }

    found
}

pub type NestedMetaList = syn::punctuated::Punctuated<syn::NestedMeta, syn::Token![,]>;

pub fn parse_meta_list(
    attr: &syn::Attribute,
    allow_empty: bool,
) -> Result<NestedMetaList, syn::Error> {
    match attr.parse_meta()? {
        syn::Meta::List(list) if allow_empty || !list.nested.is_empty() => Ok(list.nested),
        // Normalize paths to empty list.
        syn::Meta::Path(_) if allow_empty => Ok(Default::default()),
        other => Err(syn::Error::new(
            other.span(),
            if allow_empty {
                "expected attribute list"
            } else {
                "expected non-empty attribute list"
            },
        )),
    }
}

pub enum MetaValue {
    None,
    Lit(syn::Lit),
    Many(NestedMetaList),
}

pub struct MetaEntry {
    pub span: Span,
    pub name: String,
    pub value: MetaValue,
}

impl From<syn::NestedMeta> for MetaEntry {
    fn from(meta: syn::NestedMeta) -> Self {
        let meta = match meta {
            syn::NestedMeta::Lit(lit) => {
                return Self {
                    span: lit.span(),
                    name: String::new(),
                    value: MetaValue::Lit(lit),
                }
            }
            syn::NestedMeta::Meta(m) => m,
        };

        fn path_to_string(path: syn::Path) -> String {
            if let Some(ident) = path.get_ident() {
                ident.to_string()
            } else {
                path.to_token_stream().to_string()
            }
        }

        match meta {
            syn::Meta::Path(p) => Self {
                span: p.span(),
                name: path_to_string(p),
                value: MetaValue::None,
            },
            syn::Meta::List(l) => Self {
                span: l.span(),
                name: path_to_string(l.path),
                value: MetaValue::Many(l.nested),
            },
            syn::Meta::NameValue(v) => Self {
                span: v.span(),
                name: path_to_string(v.path),
                value: MetaValue::Lit(v.lit),
            },
        }
    }
}
