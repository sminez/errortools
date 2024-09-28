use proc_macro::TokenStream;
use quote::quote;
use syn::{
    braced,
    parse::{Parse, ParseStream, Result},
    parse_macro_input,
    punctuated::Punctuated,
    Expr, Ident, Pat, Token,
};

mod kw {
    syn::custom_keyword!(contains);
}

struct Input {
    err_ident: Ident,
    cases: Vec<Case>,
    default_case: Expr,
}

impl Parse for Input {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![if]>()?;
        let err_ident: Ident = input.parse()?;
        input.parse::<kw::contains>()?;

        let content;
        let _braces = braced!(content in input);
        let cases: Punctuated<Case, Token![,]> = content.parse_terminated(Case::parse)?;
        let cases = cases.into_iter().collect();
        let default_case: DefaultCase = input.parse()?;

        Ok(Self {
            err_ident,
            cases,
            default_case: default_case.expr,
        })
    }
}

struct Case {
    pat: Pat,
    expr: Expr,
}

impl Parse for Case {
    fn parse(input: ParseStream) -> Result<Self> {
        let pat: Pat = input.parse()?;
        input.parse::<Token![=>]>()?;
        let expr: Expr = input.parse()?;

        Ok(Self { pat, expr })
    }
}

struct DefaultCase {
    expr: Expr,
}

impl Parse for DefaultCase {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![default]>()?;
        input.parse::<Token![=>]>()?;
        let expr: Expr = input.parse()?;
        input.parse::<Token![,]>()?;

        Ok(Self { expr })
    }
}

#[proc_macro]
pub fn error_source_internal(input: TokenStream) -> TokenStream {
    let Input {
        err_ident,
        cases,
        default_case,
    } = parse_macro_input!(input as Input);

    let mut mapped_cases = vec![];

    for Case { pat, expr } in cases.into_iter() {
        mapped_cases.push(quote! {
            if let Some(#pat) = #err_ident.contained() {#expr}
        });
        mapped_cases.push(quote! {else});
    }

    mapped_cases.pop(); // remove the trailing 'else'

    TokenStream::from(quote! {
        #(#mapped_cases)*
        else { #default_case }
    })
}
