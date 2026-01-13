//! Procedural macro for deriving LitParser implementations for Python AST literals.
//!
//! This crate provides the `#[derive(LitParser)]` macro that generates implementations
//! for parsing Python dictionary literals into Rust structs.

extern crate proc_macro;

use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote, Data, DeriveInput, Fields, GenericParam, Generics};

/// Derive macro for implementing LitParser trait.
///
/// This macro generates an implementation of the `LitParser` trait for structs
/// with named fields. The generated code parses Python dictionary literals into
/// the annotated Rust struct.
///
/// # Example
///
/// ```ignore
/// use pylitparser_derive::LitParser;
///
/// #[derive(LitParser)]
/// struct Config {
///     name: String,
///     timeout: Option<std::time::Duration>,
///     retries: i32,
/// }
/// ```
#[proc_macro_derive(LitParser)]
pub fn derive_lit_parser(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;

    // Check if the struct has any lifetime parameters.
    // If so, use the first one as the trait lifetime.
    let struct_lifetime = input.generics.lifetimes().next().map(|l| l.lifetime.clone());

    // Check if any field has an AST reference with a lifetime.
    // This is used to determine if we need direct reference assignment.
    let ast_ref_lifetime = find_ast_ref_lifetime(&input.data);

    // Determine which lifetime to use for the trait.
    // Prefer: AST ref lifetime > struct lifetime > fresh lifetime
    let trait_lifetime = ast_ref_lifetime
        .clone()
        .or(struct_lifetime.clone())
        .unwrap_or_else(|| syn::Lifetime::new("'__pylitparser_py", proc_macro2::Span::call_site()));

    // Add a bound `T: LitParser<'py>` to every type parameter T.
    let generics = add_trait_bounds(input.generics, &trait_lifetime);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let input_ident = format_ident!("input");
    let impl_stream = generate_impl(&input.data, &input_ident);

    // Build the output, possibly using quasi-quotation
    let expanded = if struct_lifetime.is_some() || ast_ref_lifetime.is_some() {
        // Use the struct's existing lifetime
        quote! {
            // The generated impl.
            #[allow(non_snake_case)]
            impl #impl_generics pylitparser::LitParser<#trait_lifetime> for #name #ty_generics #where_clause {
                fn parse_lit(#input_ident: &#trait_lifetime ruff_python_ast::Expr) -> pylitparser::ParseResult<Self> {
                    use ruff_text_size::Ranged;
                    #impl_stream
                }
            }
        }
    } else {
        // Generate a fresh lifetime for types that don't need to borrow from input
        quote! {
            // The generated impl.
            #[allow(non_snake_case)]
            impl<'__pylitparser_py> pylitparser::LitParser<'__pylitparser_py> for #name #where_clause {
                fn parse_lit(#input_ident: &'__pylitparser_py ruff_python_ast::Expr) -> pylitparser::ParseResult<Self> {
                    use ruff_text_size::Ranged;
                    #impl_stream
                }
            }
        }
    };

    // Hand the output tokens back to the compiler.
    proc_macro::TokenStream::from(expanded)
}

/// Add a bound `T: LitParser<'py>` to every type parameter T.
fn add_trait_bounds(mut generics: Generics, lifetime: &syn::Lifetime) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param
                .bounds
                .push(parse_quote!(pylitparser::LitParser<#lifetime>));
        }
    }
    generics
}

/// Generate an implementation to parse literals for a type.
fn generate_impl(data: &Data, input_ident: &syn::Ident) -> proc_macro2::TokenStream {
    let init_stream = fields_init(data);
    let match_stream = match_expr(data, input_ident);
    let return_stream = gen_return(data, input_ident);
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(_) => {
                quote! {
                    #init_stream
                    #match_stream
                    #return_stream
                }
            }
            Fields::Unnamed(_) => {
                unimplemented!("LitParser derive does not support tuple structs")
            }
            Fields::Unit => {
                unimplemented!("LitParser derive does not support unit structs")
            }
        },
        Data::Enum(_) | Data::Union(_) => {
            unimplemented!("LitParser derive only supports structs with named fields")
        }
    }
}

/// Turns a struct into a list of field initialization statements in the form:
///    let field_name: ::std::option::Option<field_type> = None;
fn fields_init(data: &Data) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let inits = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let typ = &f.ty;
                    quote_spanned! {f.span() =>
                        let mut #name: ::std::option::Option<#typ> = None;
                    }
                });
                quote! { #(#inits)* }
            }
            Fields::Unnamed(_) => {
                unimplemented!()
            }
            Fields::Unit => {
                unimplemented!()
            }
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

/// Generates the match expression for parsing a Python dictionary.
fn match_expr(data: &Data, input_ident: &syn::Ident) -> proc_macro2::TokenStream {
    let dict_ident = format_ident!("dict");
    let field_match_stream = gen_field_match_body(data, &dict_ident, input_ident);

    quote! {
        match #input_ident {
            ruff_python_ast::Expr::Dict(ref #dict_ident) => {
                #field_match_stream
            }
            _ => return Err(pylitparser::ParseError {
                range: #input_ident.range(),
                message: "expected dictionary literal".to_string(),
            }),
        }
    }
}

/// Generates the body for matching dictionary keys and values.
fn gen_field_match_body(
    data: &Data,
    dict_ident: &syn::Ident,
    input_ident: &syn::Ident,
) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let match_cases = fields.named.iter().map(|f| {
                    let name = f.ident.as_ref().unwrap();
                    let match_literal = format!("{name}");

                    // For direct AST reference fields, directly assign the value.
                    // For other fields (including Option<&'py ast::Expr>), call parse_lit.
                    let assign_value = if is_direct_ast_ref(&f.ty) {
                        quote_spanned! {f.span() =>
                            #name = Some(__pylitparser_value);
                        }
                    } else {
                        quote_spanned! {f.span() =>
                            let __pylitparser_parsed_val = pylitparser::LitParser::parse_lit(__pylitparser_value)?;
                            #name = Some(__pylitparser_parsed_val);
                        }
                    };

                    quote_spanned! {f.span() =>
                        #match_literal => {
                            if #name.is_some() {
                                return Err(pylitparser::ParseError {
                                    range: __pylitparser_key.range(),
                                    message: format!("field {} set twice", #match_literal),
                                });
                            }
                            #assign_value
                        }
                    }
                });

                quote! {
                    // Python dicts in ruff_python_ast have iter_keys() returning Option<&Expr>
                    // (None for **kwargs spread) and iter_values() returning &Expr
                    for (__pylitparser_key_opt, __pylitparser_value) in #dict_ident.iter_keys().zip(#dict_ident.iter_values()) {
                        // Check for None key (indicates **kwargs spread)
                        let Some(__pylitparser_key) = __pylitparser_key_opt else {
                            return Err(pylitparser::ParseError {
                                range: #input_ident.range(),
                                message: "spread operator (**) not supported in dict literal".to_string(),
                            });
                        };

                        // Get the key as a string
                        let __pylitparser_key_str = match __pylitparser_key {
                            ruff_python_ast::Expr::StringLiteral(lit) => lit.value.to_string(),
                            ruff_python_ast::Expr::Name(name) => {
                                // In Python, bare names in dict literals are treated as variables,
                                // not string keys. We don't support this.
                                return Err(pylitparser::ParseError {
                                    range: __pylitparser_key.range(),
                                    message: format!("dict keys must be string literals, got identifier '{}'", name.id),
                                });
                            }
                            _ => {
                                return Err(pylitparser::ParseError {
                                    range: __pylitparser_key.range(),
                                    message: "dict keys must be string literals".to_string(),
                                });
                            }
                        };

                        match __pylitparser_key_str.as_str() {
                            #(#match_cases)*
                            _ => {
                                return Err(pylitparser::ParseError {
                                    range: __pylitparser_key.range(),
                                    message: format!("unexpected field '{}'", __pylitparser_key_str),
                                });
                            }
                        }
                    }
                }
            }
            Fields::Unnamed(_) => unimplemented!(),
            Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

/// Generates the return statement that constructs the final struct.
fn gen_return(data: &Data, input_ident: &syn::Ident) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let field_names = fields.named.iter().map(|f| {
                    let name = &f.ident;

                    if is_optional(&f.ty) {
                        quote_spanned! {f.span() =>
                            #name: #name.flatten()
                        }
                    } else {
                        quote_spanned! {f.span() =>
                            #name: #name.ok_or_else(|| pylitparser::ParseError {
                                range: #input_ident.range(),
                                message: format!("field {} is required but is missing", stringify!(#name)),
                            })?
                        }
                    }
                });
                quote! {
                    Ok(Self {
                        #(#field_names),*
                    })
                }
            }
            Fields::Unnamed(_) => {
                unimplemented!()
            }
            Fields::Unit => {
                unimplemented!()
            }
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

/// Check if a type is Option<T>.
fn is_optional(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path { segments, .. },
        }) => {
            // Return true if the last path segment is "Option".
            segments.last().is_some_and(|seg| seg.ident == "Option")
        }
        _ => false,
    }
}

/// Check if a type is a reference and return the lifetime and inner type.
fn as_reference(ty: &syn::Type) -> Option<(Option<&syn::Lifetime>, &syn::Type)> {
    match ty {
        syn::Type::Reference(ref_type) => Some((ref_type.lifetime.as_ref(), &*ref_type.elem)),
        _ => None,
    }
}

/// Check if a type is an AST type (starts with `ast::` or `ruff_python_ast::`).
fn is_ast_type(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(type_path) => {
            if let Some(first_segment) = type_path.path.segments.first() {
                first_segment.ident == "ast" || first_segment.ident == "ruff_python_ast"
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Check if a type is a reference to an AST type and return its lifetime.
fn as_ast_reference(ty: &syn::Type) -> Option<Option<&syn::Lifetime>> {
    if let Some((lifetime, inner)) = as_reference(ty) {
        if is_ast_type(inner) {
            return Some(lifetime);
        }
    }
    None
}

/// Unwrap Option<T> and return the inner type T.
fn unwrap_option_type(ty: &syn::Type) -> Option<&syn::Type> {
    match ty {
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path { segments, .. },
        }) => {
            if let Some(seg) = segments.last() {
                if seg.ident == "Option" {
                    if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                        if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                            return Some(inner);
                        }
                    }
                }
            }
            None
        }
        _ => None,
    }
}

/// Check if a field type is an AST reference (direct or wrapped in Option).
/// Returns the lifetime if found.
fn get_ast_ref_lifetime(ty: &syn::Type) -> Option<Option<&syn::Lifetime>> {
    // Check direct reference: &'py ast::Expr
    if let Some(lifetime) = as_ast_reference(ty) {
        return Some(lifetime);
    }
    // Check Option<&'py ast::Expr>
    if let Some(inner) = unwrap_option_type(ty) {
        if let Some(lifetime) = as_ast_reference(inner) {
            return Some(lifetime);
        }
    }
    None
}

/// Check if a field type is a direct AST reference (not wrapped in Option).
/// For Option<&'py ast::Expr>, we use LitParser which handles it correctly.
fn is_direct_ast_ref(ty: &syn::Type) -> bool {
    as_ast_reference(ty).is_some()
}

/// Find the first lifetime used in AST reference fields.
fn find_ast_ref_lifetime(data: &Data) -> Option<syn::Lifetime> {
    match data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                for f in &fields.named {
                    if let Some(Some(lifetime)) = get_ast_ref_lifetime(&f.ty) {
                        return Some(lifetime.clone());
                    }
                }
                None
            }
            _ => None,
        },
        _ => None,
    }
}
