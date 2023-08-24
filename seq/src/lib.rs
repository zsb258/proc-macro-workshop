use proc_macro::TokenStream;

use proc_macro2::TokenTree;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Result, Token};

#[derive(Debug)]
struct SeqMacroInput {
    var: syn::Ident,
    from: i32,
    to: i32,
    inclusive: bool,
    ts: proc_macro2::TokenStream,
}

impl Parse for SeqMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let var = input.parse()?;
        let _in = <Token![in]>::parse(input)?;

        let from = input.parse::<syn::LitInt>()?.base10_parse::<i32>()?;
        let range_limit = syn::RangeLimits::parse(input)?;
        let inclusive = match range_limit {
            syn::RangeLimits::HalfOpen(_) => false,
            syn::RangeLimits::Closed(_) => true,
        };
        let to = input.parse::<syn::LitInt>()?.base10_parse::<i32>()?;

        let content;
        let _braces = syn::braced!(content in input);
        let ts: proc_macro2::TokenStream = content.parse()?;
        // dbg!(&ts);

        Ok(SeqMacroInput {
            var,
            from,
            to,
            inclusive,
            ts,
        })
    }
}

#[proc_macro]
pub fn seq(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as SeqMacroInput);
    match seq_impl(input) {
        Ok(expanded) => expanded.into(),
        Err(err) => err.into_compile_error().into(),
    }
}

fn seq_impl(input: SeqMacroInput) -> Result<proc_macro2::TokenStream> {
    let n_iter = input.from..match input.inclusive {
        false => input.to,
        true => input.to + 1,
    };
    let tokens = n_iter.map(|n| walk_and_paste_var(input.ts.clone().into_iter(), &input.var, n));
    let expanded = quote! {
        #(#tokens)*
    };
    // dbg!(&expanded);
    Ok(expanded)
}

fn walk_and_paste_var(
    ts: proc_macro2::token_stream::IntoIter,
    var: &proc_macro2::Ident,
    n: i32,
) -> proc_macro2::TokenStream {
    let mut vec: Vec<proc_macro2::TokenStream> = vec![];
    let mut rest = ts.peekable();
    let mut carryover_token: Option<proc_macro2::TokenTree> = None;
    loop {
        let token = match carryover_token {
            Some(t) => {
                carryover_token = None;
                t.clone()
            }
            None => match rest.next() {
                Some(tt) => tt,
                None => break,
            },
        };

        let stream = match token.clone() {
            TokenTree::Ident(ident) if &ident == var => {
                let value = syn::LitInt::new(format!("{}", n).as_str(), ident.span());
                quote! { #value }
            }
            TokenTree::Ident(ident1) => {
                if rest.peek().is_some() {
                    let token_next = rest.next().unwrap();
                    match token_next {
                        TokenTree::Punct(tilde) if tilde.as_char() == '~' => match rest.peek() {
                            Some(TokenTree::Ident(ident_var)) if ident_var == var => {
                                let _ident2 = rest.next();
                                let new_ident = proc_macro2::Ident::new(
                                    format!("{}{}", &ident1.to_string(), n).as_str(),
                                    ident1.span(),
                                );
                                dbg!(&new_ident);
                                quote! { #new_ident }
                            }
                            _ => quote! { #token, #tilde },
                        },
                        _ => {
                            carryover_token = Some(token_next);
                            token.into()
                        }
                    }
                } else {
                    token.into()
                }
            }
            TokenTree::Group(group) => {
                let ts_new = walk_and_paste_var(group.stream().into_iter(), var, n);
                TokenTree::Group(proc_macro2::Group::new(group.delimiter(), ts_new)).into()
            }
            _ => token.into(),
        };
        vec.push(stream);
    }
    vec.iter().fold(
        proc_macro2::TokenStream::new(),
        |acc, stream| quote! {#acc #stream},
    )
}
