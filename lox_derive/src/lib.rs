extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
#[macro_use]
extern crate quote;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Data};

use proc_macro::TokenStream;

#[proc_macro_derive(EnumStrings)]
pub fn enum_strings_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as DeriveInput);

    // Build the impl
    let gen = impl_enum_strings(&ast);

    // Return the generated impl
    TokenStream::from(gen)
}

fn impl_enum_strings(ast: &syn::DeriveInput) -> proc_macro2::TokenStream {
    let name = &ast.ident;

    if let Data::Enum(ref enums) = ast.data {
        let mut branches:Vec<proc_macro2::TokenStream> = Vec::new();
        for variant in enums.variants.iter(){
            let variant_name = &variant.ident;
            let num_fields = variant.fields.len();

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
            impl enum_vectorize for #name<'_> {
                fn enum_to_vector(&self) -> Vec<String> {
                    let mut rvec: Vec<String> = Vec::new();
                    match self {
                        #(#branches)*
                    };
                    rvec
                }
            }
        };
    }

    quote! {
    }
}


