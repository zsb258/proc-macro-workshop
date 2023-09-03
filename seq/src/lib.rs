use proc_macro2::TokenTree;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Result};

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
        let _in = <syn::Token![in]>::parse(input)?;

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
pub fn seq(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
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

    let rep_grp_stream = get_repetition_stream(input.ts.clone());
    let expanded: proc_macro2::TokenStream = match rep_grp_stream {
        Some(rep_stream) => {
            let grp_tokens = n_iter.map(|n| walk_and_paste_var(rep_stream.clone(), &input.var, n));
            let expanded_grp = quote! { #(#grp_tokens)* };
            replace_repetition_grp(input.ts.clone(), expanded_grp)
        }
        None => {
            let tokens = n_iter.map(|n| walk_and_paste_var(input.ts.clone(), &input.var, n));
            quote! { #(#tokens)* }
        }
    };

    Ok(expanded)
}

/// Walk the token stream and extract stream within `#(..)*` pattern
fn get_repetition_stream(ts: proc_macro2::TokenStream) -> Option<proc_macro2::TokenStream> {
    let tt_vec = Vec::from_iter(ts);
    for i in 0..tt_vec.len() {
        let curr = &tt_vec[i];
        if let TokenTree::Group(grp) = curr {
            if let Some(s) = get_repetition_stream(grp.stream()) {
                return Some(s);
            }
        }

        let prev = if i > 0 { Some(&tt_vec[i - 1]) } else { None };
        let next = if i + 1 < tt_vec.len() {
            Some(&tt_vec[i + 1])
        } else {
            None
        };
        match (prev, curr, next) {
            (
                Some(TokenTree::Punct(pound)),
                TokenTree::Group(grp),
                Some(TokenTree::Punct(asterisk)),
            ) if (pound.as_char() == '#'
                && grp.delimiter() == proc_macro2::Delimiter::Parenthesis
                && asterisk.as_char() == '*') =>
            {
                return Some(grp.stream());
            }
            _ => continue,
        }
    }
    None
}

/// Walk the token stream and replace `#(..)*` pattern with expanded repetition tokens
fn replace_repetition_grp(
    ts: proc_macro2::TokenStream,
    rep_grp: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let tokens = Vec::from_iter(ts);
    let mut res: Vec<proc_macro2::TokenStream> = vec![];

    let mut i = 0;
    while i < tokens.len() {
        let token = &tokens[i];
        let mut advance_by = 1;

        let stream = match token {
            TokenTree::Punct(pound) if pound.as_char() == '#' => {
                if i + 2 < tokens.len() {
                    match (&tokens[i + 1], &tokens[i + 2]) {
                        (TokenTree::Group(grp), TokenTree::Punct(asterisk))
                            if (grp.delimiter() == proc_macro2::Delimiter::Parenthesis
                                && asterisk.as_char() == '*') =>
                        {
                            advance_by = 3;
                            Some(rep_grp.clone())
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            TokenTree::Group(group) => {
                let ts_new = replace_repetition_grp(group.stream(), rep_grp.clone());
                Some(TokenTree::Group(proc_macro2::Group::new(group.delimiter(), ts_new)).into())
            }
            _ => None,
        };

        match stream {
            Some(stream) => res.push(stream),
            None => res.push(token.clone().into()),
        };

        i += advance_by;
    }

    res.iter().fold(
        proc_macro2::TokenStream::new(),
        |acc, stream| quote! {#acc #stream},
    )
}

fn walk_and_paste_var(
    ts: proc_macro2::TokenStream,
    var: &proc_macro2::Ident,
    n: i32,
) -> proc_macro2::TokenStream {
    let tokens = Vec::from_iter(ts);
    let mut res: Vec<proc_macro2::TokenStream> = vec![];

    let mut i = 0;
    while i < tokens.len() {
        let token = &tokens[i];
        let mut advance_by = 1;

        let stream = match token {
            TokenTree::Ident(ident) if ident == var => {
                let value = syn::LitInt::new(format!("{}", n).as_str(), ident.span());
                let exp = quote! { #value };
                Some(exp)
            }
            TokenTree::Ident(ident) => {
                if i + 2 < tokens.len() {
                    match (&tokens[i + 1], &tokens[i + 2]) {
                        (TokenTree::Punct(tilde), TokenTree::Ident(ident_var))
                            if (tilde.as_char() == '~' && ident_var == var) =>
                        {
                            let new_ident = proc_macro2::Ident::new(
                                format!("{}{}", &ident.to_string(), n).as_str(),
                                ident.span(),
                            );
                            advance_by = 3;
                            let exp = quote! { #new_ident };
                            Some(exp)
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            TokenTree::Group(group) => {
                let ts_new = walk_and_paste_var(group.stream(), var, n);
                Some(TokenTree::Group(proc_macro2::Group::new(group.delimiter(), ts_new)).into())
            }
            _ => None,
        };

        match stream {
            Some(stream) => res.push(stream),
            None => res.push(token.clone().into()),
        };

        i += advance_by;
    }

    res.iter().fold(
        proc_macro2::TokenStream::new(),
        |acc, stream| quote! {#acc #stream},
    )
}
