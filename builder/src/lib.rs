extern crate proc_macro;

use proc_macro::TokenStream;

use quote::{format_ident, quote};
use syn::{
    Data, DataStruct, DeriveInput, Field, Fields, FieldsNamed, GenericArgument, Ident,
    Lit, Meta, MetaList, MetaNameValue, NestedMeta, parse_macro_input, PathArguments, Type,
};

#[derive(Clone, Debug)]
enum TokenResult<T> {
    Fail(TokenStream),
    NotFound,
    Token(T),
}

macro_rules! fail {
    ($msg:literal) => {
        syn::Error::new(proc_macro2::Span::call_site(), $msg).to_compile_error().into()
    };
    ($tkns:ident, $msg:literal) => {
        syn::Error::new_spanned($tkns, $msg).to_compile_error().into()
    };
    // ($msg:literal, $($arg:expr),+) => {
    //     syn::Error::new(proc_macro2::Span::call_site(), format!($msg, $($arg),*)).to_compile_error().into()
    // };
    // ($tkns:ident, $msg:literal, $($arg:expr),+) => {
    //     syn::Error::new_spanned($tkns, format!($msg, $($arg),*)).to_compile_error().into()
    // };
}

macro_rules! replace_expr {
    ($_t:tt $sub:expr) => {$sub};
}

macro_rules! count_tts {
    ($($tts:tt)*) => {<[()]>::len(&[$(replace_expr!($tts ())),*])};
}

macro_rules! match_segments {
    ($segments:expr; $($segment:literal),+) => {
        $segments.len() == count_tts!($($segment) *) &&
            $segments.iter()
                .zip(&vec![$($segment),*])
                .all(|(seg, ident)| seg.ident == ident)
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    const OPTION_SEGMENTS: &[&'static str] = &["std", "option", "Option"];
    const VEC_SEGMENTS: &[&'static str] = &["std", "vec", "Vec"];

    let ast = parse_macro_input!(input as DeriveInput);
    let named_fields =
        if let Data::Struct(DataStruct { fields: Fields::Named(FieldsNamed { ref named, .. }), .. }) = ast.data {
            named
        } else {
            return fail!("derive builder only supports structs with named fields")
        };

    let struct_name = ast.ident;
    let builder_name = format_ident!("{}Builder", struct_name);
    let builder_fields = named_fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        let is_optional = inner_ty(ty, OPTION_SEGMENTS).is_some();
        let is_vec = inner_ty(ty, VEC_SEGMENTS).is_some();

        if is_optional || is_vec {
            quote! { #name: #ty, }
        } else {
            quote! { #name: std::option::Option<#ty>, }
        }
    });
    let builder_methods = named_fields.iter().map(|field| {
        let name = &field.ident;
        let mut is_vec = false;
        let field_ty = &field.ty;

        let ty = if let Some(inner_ty) = inner_ty(field_ty, OPTION_SEGMENTS) {
            inner_ty
        } else {
            if let Some(inner_ty) = inner_ty(field_ty, VEC_SEGMENTS) {
                is_vec = true;
                inner_ty
            } else {
                field_ty
            }
        };

        if is_vec {
            let setter = quote! {
                pub fn #name(&mut self, #name: #field_ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
            };

            match builder_each(&field) {
                // build attribute is broken
                TokenResult::Fail(fail) => return fail.into(),
                // build attribute was not set
                TokenResult::NotFound => setter,
                TokenResult::Token(each) => {
                    let extender = quote! {
                        pub fn #each(&mut self, #each: #ty) -> &mut Self {
                            self.#name.push(#each);
                            self
                        }
                    }.into();

                    if each != name.as_ref().map_or(String::new(), |v| v.to_string()) {
                        // build attribute each is unique, not the same as field
                        quote! { #setter #extender }
                    } else {
                        // build attribute each is not unique, only derive extender
                        extender
                    }
                },
            }
        } else {
            match builder_each(&field) {
                // build attribute was not set, this is ok
                TokenResult::NotFound => {},
                // build attribute is broken
                TokenResult::Fail(_) |
                TokenResult::Token(_) =>
                    return fail!(field, "extenders are only supported on vec fields"),
            }

            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        }
    });
    let build_fields = named_fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        let is_optional = inner_ty(ty, OPTION_SEGMENTS).is_some();
        let is_vec = inner_ty(ty, VEC_SEGMENTS).is_some();

        if is_optional || is_vec {
            quote! {
                #name: self.#name.clone(),
             }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?,
            }
        }
    });
    let init_fields = named_fields.iter().map(|field| {
        let name = &field.ident;
        let is_vec = inner_ty(&field.ty, VEC_SEGMENTS).is_some();

        if is_vec {
            quote! { #name: std::vec::Vec::default(), }
        } else {
            quote! { #name: std::option::Option::None, }
        }
    });

    let output = quote! {
        pub struct #builder_name {
            #(#builder_fields)*
        }

        impl #builder_name {
            #(#builder_methods)*

            pub fn build(&self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#struct_name {
                    #(#build_fields)*
                })
            }
        }

        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#init_fields)*
                }
            }
        }
    };

    output.into()
}

fn inner_ty<'a>(ty: &'a Type, segments: &[&str]) -> Option<&'a Type> {
    if let Type::Path(ref path) = ty {
        if !(path.path.segments
            .iter().rev().zip(segments.iter().rev())
            .all(|(seg, opt)| seg.ident == opt)) { return None }

        if let PathArguments::AngleBracketed(ref inner_ty) = path.path.segments.last()?.arguments {
            if inner_ty.args.len() != 1 { return None }

            let inner_ty = inner_ty.args.first()?;
            if let GenericArgument::Type(ref inner_ty) = inner_ty {
                Some(inner_ty)
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn builder_each(field: &Field) -> TokenResult<Ident> {
    for attr in &field.attrs {
        return match attr.parse_meta() {
            Ok(meta) => {
                match &meta {
                    Meta::List(MetaList { path, nested, .. }) if match_segments!(path.segments; "builder") && nested.len() == 1 => {
                        match nested.first() {
                            Some(NestedMeta::Meta(Meta::NameValue(MetaNameValue { path, lit: Lit::Str(lit_str), .. })))
                            if match_segments!(path.segments; "each") => {
                                TokenResult::Token(Ident::new(&lit_str.value(), lit_str.span()))
                            }
                            _ => TokenResult::Fail(fail!(meta, "expected `builder(each = \"...\")`"))
                        }
                    }
                    _ => continue
                }
            }
            Err(err) => TokenResult::Fail(err.to_compile_error().into())
        }
    }

    TokenResult::NotFound
}
