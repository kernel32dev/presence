use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{Fields, FieldsNamed, FieldsUnnamed};

use crate::{generics::type_mentions_generics, Args};

pub fn impl_struct_debug_presence(
    args: &Args,
    derive_debug: syn::Path,
    item: &syn::ItemStruct,
) -> proc_macro2::TokenStream {
    let Args { presence } = args;
    let name = &item.ident;
    let name_str = name.to_string();
    let code = match &item.fields {
        Fields::Named(fields_named) => {
            let debug_entries = fields_named.named.iter().map(|f| {
                let name = f.ident.as_ref().unwrap();
                let name_str = name.to_string();
                quote! {
                    if let ::core::option::Option::Some(value) = #presence::debug::PresenceAwareDebug::as_presence_aware_debug(&self.#name) {
                        debug_struct.field(#name_str, value);
                    }
                }
            });

            quote! {
                let mut debug_struct = f.debug_struct(#name_str);
                #(#debug_entries)*
                debug_struct.finish()
            }
        }
        Fields::Unnamed(fields_unnamed) => {
            let debug_entries = (0..fields_unnamed.unnamed.len()).map(|i| {
                let i = syn::Index::from(i);
                quote! {
                    if let ::core::option::Option::Some(value) = #presence::debug::PresenceAwareDebug::as_presence_aware_debug(&self.#i) {
                        debug_tuple.field(value);
                    }
                }
            });

            quote! {
                let mut debug_tuple = f.debug_tuple(#name_str);
                #(#debug_entries)*
                debug_tuple.finish()
            }
        }
        Fields::Unit => {
            quote! {
                f.write_str(#name_str)
            }
        }
    };

    let fmt_fn = quote! {
        fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
            #code
        }
    };

    add_constraints(
        args,
        derive_debug,
        &item.ident,
        &item.generics,
        &item.fields,
        fmt_fn,
    )
}

pub fn impl_enum_debug_presence(
    args: &Args,
    derive_debug: syn::Path,
    item: &syn::ItemEnum,
) -> proc_macro2::TokenStream {
    let Args { presence } = args;
    let name = &item.ident;
    let match_arms = item.variants.iter().map(|variant| {
        let var_ident = &variant.ident;
        let variant_str = var_ident.to_string();

        match &variant.fields {
            Fields::Named(FieldsNamed { named, .. }) => {
                let bindings = named.iter().enumerate().map(|(index, f)| {
                    let field_name = f.ident.as_ref().unwrap();
                    let ident = format_ident!("f{}", index);
                    quote! { #field_name: #ident }
                });

                let field_debugs = named.iter().enumerate().map(|(index, f)| {
                    let ident = format_ident!("f{}", index);
                    let name_str = f.ident.as_ref().unwrap().to_string();
                    quote! {
                        if let ::core::option::Option::Some(value) = #presence::debug::PresenceAwareDebug::as_presence_aware_debug(#ident) {
                            debug_struct.field(#name_str, value);
                        }
                    }
                });

                quote! {
                    #name::#var_ident { #(#bindings),* } => {
                        let mut debug_struct = f.debug_struct(#variant_str);
                        #(#field_debugs)*
                        debug_struct.finish()
                    }
                }
            }

            Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                let bindings = (0..unnamed.len()).map(|i| format_ident!("f{}", i));
                
                let field_debugs = (0..unnamed.len()).map(|i| {
                    let ident = format_ident!("f{}", i);
                    quote! {
                        if let ::core::option::Option::Some(value) = #presence::debug::PresenceAwareDebug::as_presence_aware_debug(#ident) {
                            debug_tuple.field(value);
                        }
                    }
                });

                quote! {
                    #name::#var_ident( #(#bindings),* ) => {
                        let mut debug_tuple = f.debug_tuple(#variant_str);
                        #(#field_debugs)*
                        debug_tuple.finish()
                    }
                }
            }

            Fields::Unit => {
                quote! {
                    #name::#var_ident => {
                        f.write_str(#variant_str)
                    }
                }
            }
        }
    });

    let fmt_fn = quote! {
        fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
            match self {
                #(#match_arms),*
            }
        }
    };

    let fields = item.variants.iter().flat_map(|x| &x.fields);

    add_constraints(
        args,
        derive_debug,
        &item.ident,
        &item.generics,
        fields,
        fmt_fn,
    )
}

fn add_constraints<'a>(
    args: &Args,
    derive_debug: syn::Path,
    name: &syn::Ident,
    generics: &syn::Generics,
    fields: impl IntoIterator<Item = &'a syn::Field>,
    fmt_fn: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let Args { presence } = args;

    // unlike serde, the derive macro is not used
    let _ = derive_debug;

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let mut where_clause = where_clause.cloned().unwrap_or_else(|| syn::WhereClause {
        where_token: Default::default(),
        predicates: Default::default(),
    });

    let bounds = syn::punctuated::Punctuated::from_iter([syn::TypeParamBound::Trait(
        syn::parse_quote! { #presence::debug::PresenceAwareDebug },
    )]);

    for field in fields {
        if !type_mentions_generics(&field.ty, generics) {
            continue;
        }
        where_clause
            .predicates
            .push(syn::WherePredicate::Type(syn::PredicateType {
                lifetimes: None,
                // TODO! there is a very strage E0283 error that happens when the same bound is places on two identical types that only differ in the lifetimes
                // making them HOTB (with the for<'...> ...) syntax seems to resolve the issue and allow repeats, but does not seem to be a complete solution
                // as of currently this prevents structs with more than one lifetime from working with Debug, Serialize and Deserialize
                bounded_ty: field.ty.clone(),
                colon_token: syn::Token![:](Span::call_site()),
                bounds: bounds.clone(),
            }));
    }

    quote! {
        impl #impl_generics ::core::fmt::Debug for #name #ty_generics #where_clause {
            #fmt_fn
        }
    }
}
