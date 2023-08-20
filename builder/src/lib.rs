use proc_macro::TokenStream;

use proc_macro2::Ident;
use quote::quote;
use syn::{self, parse_macro_input};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    let name = input.ident;
    let builder_name = Ident::new(format!("{}Builder", &name).as_str(), name.span());

    // procedurally generally builder impl
    let named_fields = match input.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
            ..
        }) => named,
        _ => unimplemented!("derive for struct only"),
    };

    let optionised_fields = named_fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! { #name: std::option::Option<#ty> }
    });

    let setter_methods = named_fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! {
            fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = std::option::Option::Some(#name);
                self
            }
        }
    });

    let build_field_stmts = named_fields.iter().map(|f| {
        let name = &f.ident;
        quote! {
            #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
        }
    });

    let tokens = quote! {
        pub struct #builder_name {
            #(#optionised_fields,)*
        }

        impl #builder_name {
            #(#setter_methods)*

            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(
                    #name {
                        #(#build_field_stmts),*
                    }
                )
            }
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    executable: None,
                    args: None,
                    env: None,
                    current_dir: None,
                }
            }


        }

    };

    // eprintln!("TOKENS: {}", tokens);

    tokens.into()
}
