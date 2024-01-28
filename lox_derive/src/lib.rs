extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
#[macro_use]
extern crate quote;
use quote::{quote, ToTokens};
use syn::{DeriveInput, Data};

use proc_macro::TokenStream;

#[proc_macro_derive(EnumStrings)]
pub fn enum_strings_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as DeriveInput);

    // Build the impl
    let gen = impl_enum_strings(&ast);

    // Return the generated impl
    TokenStream::from(gen)
}

struct IsTokenTypeLambda<'a> {
    ident: &'a syn::Ident,
    num_fields: usize
}

impl<'a> ToTokens for IsTokenTypeLambda<'a> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ident = self.ident;
        let is_ident = format_ident!("is_{}", ident.to_string().to_lowercase());
        // I could easily generalizr this for multiple fields
        let fields_string = if self.num_fields == 0 {
            ""
        }
        else {
            "(_)"
        };
        let fields: proc_macro2::TokenStream = syn::parse_str(fields_string).unwrap();
        let gen = quote! {
            pub fn #is_ident(x: &Token) -> bool {
                    match x {
                        Token::#ident #fields => true,
                        _ => false
                    }
                }
        };
        gen.to_tokens(tokens);
    }
}

impl<'a> IsTokenTypeLambda<'a> {
    fn new(value: &'a syn::Ident, num_fields: usize) -> Self {
        IsTokenTypeLambda{ident: value, num_fields}
    }
}


fn impl_enum_strings(ast: &syn::DeriveInput) -> proc_macro2::TokenStream {
    let name = &ast.ident;

    if let Data::Enum(ref enums) = ast.data {
        let mut branches:Vec<proc_macro2::TokenStream> = Vec::new();
        let mut token_checks: Vec<IsTokenTypeLambda> = Vec::new();
        for variant in enums.variants.iter(){
            let variant_name = &variant.ident;
            let num_fields = variant.fields.len();

            token_checks.push(IsTokenTypeLambda::new(&variant.ident, num_fields));

            let variant_tokens = if num_fields > 0 {
                let destructure_vars: Vec<proc_macro2::Ident> = (0..num_fields).map(|i| {
                    format_ident!("x{}",i)
                }).collect();
                quote!{
                    #name::#variant_name( #(#destructure_vars,)* ) => {
                        let mut temp: Vec<String> = vec![#(#destructure_vars.enum_to_element(),)*];
                        rvec.push(stringify!(#variant_name).to_string());
                        rvec.append(&mut temp);
                    },
                }
            }
            else {
                quote!{
                    #name::#variant_name => {
                        rvec.push(stringify!(#variant_name).to_string());
                    },
                }
            };
            branches.push(variant_tokens);
        }
        return quote! {
            impl EnumVectorize for #name {
                fn enum_to_vector(&self) -> Vec<String> {
                    let mut rvec: Vec<String> = Vec::new();
                    match self {
                        #(#branches)*
                    };
                    rvec
                }
            }

            impl #name {
                #(#token_checks)*
            }
        };
    }

    quote! {
    }
}


