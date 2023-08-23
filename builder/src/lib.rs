use proc_macro::TokenStream;

use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{self, parse_macro_input};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    let name = input.ident;
    let builder_name = Ident::new(format!("{}Builder", &name).as_str(), name.span());

    let named_fields = match input.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
            ..
        }) => named,
        _ => unimplemented!("`derive[Builder]` implemented for struct only"),
    };

    let optionised_fields = named_fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if unwrap_option_ty(ty).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let methods = named_fields.iter().map(|f| {
        let set_method = generate_setter_method(f);
        if f.attrs.is_empty() {
            let tokens = quote! { #set_method };
            return tokens;
        }
        f.attrs
            .iter()
            .filter(|attr| attr.path().is_ident("builder"))
            .fold(
                proc_macro2::TokenStream::new(),
                |acc, attr| match extract_attr_value(attr) {
                    Ok(lit_str) => {
                        let build_method = generate_builder_method(f, lit_str.as_str());
                        if f.ident.as_ref().is_some_and(|x| x == lit_str.as_str()) {
                            quote! {
                                #acc
                                #build_method
                            }
                        } else {
                            quote! {
                                #acc
                                #build_method
                                #set_method
                            }
                        }
                    }
                    Err(e) => syn::Error::into_compile_error(e),
                },
            )
    });

    let build_default_fields = named_fields.iter().map(|f| {
        let name = &f.ident;
        if !f.attrs.is_empty() {
            assert!(unwrap_vec_ty(&f.ty).is_some());
            quote! { #name: std::option::Option::Some(vec![]) }
        } else {
            quote! { #name: std::option::Option::None }
        }
    });

    let build_field_stmts = named_fields.iter().map(|f| {
        let name = &f.ident;
        if unwrap_option_ty(&f.ty).is_some() {
            quote! { #name: self.#name.clone() }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let tokens = quote! {
        pub struct #builder_name {
            #(#optionised_fields,)*
        }

        impl #builder_name {
            #(#methods)*

            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                Ok(
                    #name {
                        #(#build_field_stmts,)*
                    }
                )
            }
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#build_default_fields,)*
                }
            }

        }

    };

    tokens.into()
}

/// Returns the ast inner type of `wrapper`
///
/// e.g. Give ast representing `Option<String>`, returns ast representing `String`
fn unwrap_inner_ty<'a>(ty: &'a syn::Type, wrapper: &'_ str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { ref segments, .. },
        ..
    }) = ty
    {
        if !(segments.len() == 1 && segments[0].ident == wrapper) {
            return None;
        }
        let option_segment = segments.first().unwrap();
        if let syn::PathSegment {
            arguments:
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
            ..
        } = option_segment
        {
            if args.len() != 1 {
                return None;
            }

            if let syn::GenericArgument::Type(inner_ty) = args.first().unwrap() {
                return Some(inner_ty);
            }
        }
    }

    None
}

fn unwrap_option_ty(ty: &syn::Type) -> Option<&syn::Type> {
    unwrap_inner_ty(ty, "Option")
}

fn unwrap_vec_ty(ty: &syn::Type) -> Option<&syn::Type> {
    unwrap_inner_ty(ty, "Vec")
}

fn generate_setter_method(f: &syn::Field) -> proc_macro2::TokenStream {
    let name = &f.ident;
    let ty = match unwrap_option_ty(&f.ty) {
        Some(inner_ty) => inner_ty,
        None => &f.ty,
    };
    quote! {
        pub fn #name(&mut self, #name: #ty) -> &mut Self {
            self.#name = std::option::Option::Some(#name);
            self
        }
    }
}

/// Extract String "..." from `#[builder(each = "...")]`
fn extract_attr_value(attr: &syn::Attribute) -> syn::Result<String> {
    let parsed: syn::Expr = attr.meta.require_list()?.parse_args()?;
    if let syn::Expr::Assign(syn::ExprAssign { left, right, .. }) = parsed {
        if let syn::Expr::Path(syn::ExprPath { path, .. }) = *left {
            if !path.is_ident("each") {
                return Err(syn::Error::new_spanned(
                    attr.meta.clone(),
                    "expected `builder(each = \"...\")`",
                ));
            }
            if let syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(lit_str),
                ..
            }) = *right
            {
                return Ok(lit_str.value());
            }
        }
    }

    Err(syn::Error::new_spanned(
        attr.meta.clone(),
        "expected `builder(each = \"...\")`",
    ))
}

fn generate_builder_method(f: &syn::Field, lit_str: &str) -> proc_macro2::TokenStream {
    let name = &f.ident;
    let inner_ty = match unwrap_vec_ty(&f.ty) {
        Some(ty) => ty,
        None => panic!("the `each` attribute is only accepted on `Vec` fields"),
    };

    let new_ident = Ident::new(lit_str, Span::call_site());
    quote! {
        pub fn #new_ident(&mut self, #new_ident: #inner_ty) -> &mut Self {
            let mut vec = self.#name.clone().expect("Vec should be initialised");
            vec.push(#new_ident);
            self.#name = std::option::Option::Some(vec);
            self
        }
    }
}
