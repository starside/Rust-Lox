use proc_macro;
use proc_macro::TokenStream;
use proc_macro2;
use quote::{format_ident, quote};

struct Field {
    name: String,
    typename: String
}

struct Rule {
    name: String,
    fields: Vec<Field>
}

struct VisitFunctionSignature {
    name: String
}

struct AcceptTrait {
    table_name: String,
    rule_name: String
}

struct ExprEnum {
    name: String
}

struct ExprEnumMatch {
    branch_name: String,
    enum_name: String
}

impl ExprEnumMatch {
    fn new(branch: &ExprEnum, enum_name: &String) -> Self {
        ExprEnumMatch{branch_name: branch.name.clone(), enum_name: enum_name.clone()}
    }
}

impl quote::ToTokens for ExprEnumMatch {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let enum_name = self.enum_name.clone();
        let enum_name = format_ident!("{}", enum_name);

        let branch_name = self.branch_name.clone();
        let branch_name_lowercase = self.branch_name.clone().to_lowercase();

        let branch_name = format_ident!("{}", branch_name);
        let visitor_idn = format_ident!("visit_{}", branch_name_lowercase);

        let gen = quote! {
            #enum_name::#branch_name(x) => {
                visitor.#visitor_idn(x)
            }
        };
        gen.to_tokens(tokens);
    }
}

impl From<&Rule> for ExprEnum {
    fn from(value: &Rule) -> Self {
        ExprEnum {name: value.name.clone()}
    }
}

impl quote::ToTokens for ExprEnum {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let name = self.name.clone();
        let idn = format_ident!("{}", name);
        let gen = quote! {
            #idn(Box<#idn>),
        };
        gen.to_tokens(tokens);
    }
}

impl quote::ToTokens for AcceptTrait {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let table_name = self.table_name.clone();
        let table_name_idn = format_ident!("{}Visitor", table_name);

        let rule_name = self.rule_name.clone();
        let rule_name_idn = format_ident!("{}", rule_name);

        let low_name = rule_name.to_lowercase();
        let low_name_idn = format_ident!("visit_{}", low_name);

        let gen = quote! {
            impl<R> Accept<R> for #rule_name_idn {
                fn accept<V: #table_name_idn<R>>(&self, visitor: &mut V) -> R {
                    visitor.#low_name_idn(self)
                }
            }
        };
        gen.to_tokens(tokens);
    }
}

impl AcceptTrait {
    fn new(table_name: String, rule: &Rule) -> Self {
        AcceptTrait {table_name: table_name.clone(), rule_name: rule.name.clone()}
    }
}

impl From<&Rule> for VisitFunctionSignature {
    fn from(value: &Rule) -> Self {
        VisitFunctionSignature {name: value.name.clone()}
    }
}

impl quote::ToTokens for VisitFunctionSignature {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let name = self.name.clone();
        let idn = format_ident!("{}", name);
        let low_name = name.to_lowercase();
        let func_name_idn = format_ident!("visit_{}", low_name);
        let gen = quote! {
            fn #func_name_idn(&mut self, visitor: &#idn) -> R;
        };
        gen.to_tokens(tokens);
    }
}

impl quote::ToTokens for Field {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let name_string = self.name.clone();
        let name_idn = format_ident!("{}", name_string);
        let type_string = self.typename.clone();
        let type_string_idn = format_ident!("{}", type_string);
        let gen = quote! {
            pub #name_idn: #type_string_idn
        };
        gen.to_tokens(tokens);
    }
}

impl quote::ToTokens for Rule {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let name = self.name.clone();
        let idn = format_ident!("{}", name);
        let field_iter = self.fields.iter();
        let gen = quote! {
            pub struct #idn {
                #(#field_iter),*
            }
        };
        gen.to_tokens(tokens);
    }
}

fn parse_rule(rule: &str) -> Result<Rule, String> {
    let parts: Vec<&str> = rule.split(":")
        .map(|x| x.trim())
        .filter(|x| x.len() > 0)
        .collect();
    if parts.len() != 2 {
        return Err(String::from("Rule is not of form name : fields"));
    }
    let rule_name = parts[0];

    let field_list = if parts[1].contains(",")
    {
        parts[1].split(",")
            .map(|x| x.trim())
            .filter(|x| x.len() > 0)
            .collect()
    }
    else {
        vec![parts[1].trim()]
    };

    let field_list = field_list.iter()
        .map(|field| {
            let new_field: Vec<&str> = field.split(char::is_whitespace)
                .map(|x| x.trim())
                .filter(|x| x.len() > 0)
                .collect();
            // ignore error handling.  new_field should be length 2
            if new_field.len() != 2 {
                panic!("Field in rule is not formatted as <Type> <Name>");
            }
            Field {
                name: String::from(new_field[1]),
                typename: String::from(new_field[0])
            }
        }).collect();
    Ok(Rule {
        name: String::from(rule_name),
        fields: field_list
    })
}

#[proc_macro]
pub fn derive_ast(input: TokenStream) -> TokenStream {
    let raw_input_string = input.to_string();

    let input_array: Vec<&str> = raw_input_string
        .split("/")
        .map(|x| x.trim())
        .filter(|x| x.len() > 0)
        .collect();
    let table_name = input_array[0].to_string().clone();
    let expression_name = input_array[1].to_string().clone();
    let input_string = input_array[2].to_string().clone();

    println!("{}: {:?}", input_array.len(), input_array);

    let rules: Vec<&str> =input_string
        .split(";")
        .map(|rule| rule.trim())
        .filter(|rule| rule.len() > 0)
        .collect();

    let rules: Vec<Rule> = rules.iter()
        .map(|rule| parse_rule(rule).unwrap())
        .collect();

    //
    // Generate data structures
    //
    let rules_iter = rules.iter();
    let gen_structs = quote! {
        #(#rules_iter)*
    };

    //
    // Generate expression enum
    //
    let rules_iter = rules.iter();
    let expression_name_idn = format_ident!("{}", expression_name);
    let expr_enum_branches: Vec<ExprEnum> = rules_iter
        .map(|x| ExprEnum::from(x))
        .collect();
    let expr_enum_branches_iter = expr_enum_branches.iter();
    let gen_enum = quote! {
        pub enum #expression_name_idn {
            Empty,
            #(#expr_enum_branches_iter)*
        }
    };

    //
    // Create Visitor trait
    //
    let rules_iter = rules.iter();
    let accept_visitor_signatures: Vec<VisitFunctionSignature> = rules_iter
        .map(|x| VisitFunctionSignature::from(x))
        .collect();
    let x = accept_visitor_signatures.iter();

    let visitor_ident = format_ident!("{}Visitor", table_name);
    let gen_visitor_trait = quote! {
        pub trait #visitor_ident<R> {
            #(#x)*
        }
    };

    //
    // Generate accept trait
    //
    let gen_accept_trait = quote! {
        pub trait Accept<R> {
            fn accept<V: #visitor_ident<R>>(&self, visitor: &mut V) -> R;
        }
    };

    //
    // Implement accept trait for rules
    //
    let rules_iter = rules.iter();
    let accept_trait_impl: Vec<AcceptTrait> = rules_iter
        .map(|x| AcceptTrait::new(table_name.to_string(), x))
        .collect();
    let acti = accept_trait_impl.iter();
    let gen_accept_trait_impl = quote! {
        #(#acti)*
    };

    //
    // Implement accept trait for Expr
    //
    let expr_match_arms: Vec<ExprEnumMatch> = expr_enum_branches.iter()
        .map(|x| ExprEnumMatch::new(x, &expression_name))
        .collect();
    let expr_match_arms_iter = expr_match_arms.iter();
    let gen_expr_accept_trait_impl = quote! {
        impl<R> Accept<R> for #expression_name_idn {
            fn accept<V: #visitor_ident<R>>(&self, visitor: &mut V) -> R {
                match self {
                    #expression_name_idn::Empty => {panic!("Cannot visit empty")},
                    #(#expr_match_arms_iter)*
                }
            }
        }
    };

    let gen = quote! {
        #gen_enum

        #gen_structs

        #gen_visitor_trait

        #gen_accept_trait

        #gen_expr_accept_trait_impl

        #gen_accept_trait_impl
    };

    gen.into()
}
