#![cfg(feature = "salvo")]
use proc_macro2::TokenStream;
use quote::quote;

use crate::Args;

pub fn derive_struct_presence_aware_to_schema(
    args: &Args,
    derive_to_schema: syn::Path,
    input: &syn::ItemStruct,
) -> TokenStream {
    let Args { presence } = args;
    let struct_name = &input.ident;
    let generics = &input.generics;

    let proxy_attrs = input
        .attrs
        .iter()
        .filter(|attr| attr.path().is_ident("serde") || attr.path().is_ident("salvo"));

    let proxy_generics_params = &generics.params;
    let proxy_generics_where = &generics.where_clause;

    let proxy_fields = input.fields.iter().map(|field| {
        let attrs = field
            .attrs
            .iter()
            .filter(|x| x.path().is_ident("serde") || x.path().is_ident("salvo"));
        let name = &field.ident;
        let ty = &field.ty;

        let last_segment = match ty {
            syn::Type::Path(path) => path.path.segments.last(),
            _ => None,
        };
        if let Some(syn::PathSegment { ident, arguments }) = last_segment {
            let ident = ident.to_string();
            if ident == "Presence" {
                return quote!(
                    #(#attrs)*
                    #[serde(default)]
                    #name: #ty,
                );
            }
            if ident == "Option" {
                return quote!(
                    #(#attrs)*
                    #name: #presence::salvo_oapi::RenamedOption #arguments,
                );
            }
        }
        quote!(
            #(#attrs)*
            #name: #ty,
        )
    });

    // now combine everything
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    quote! {
        impl #impl_generics #presence::salvo_oapi::ToSchema for #struct_name #ty_generics #where_clause {
            fn to_schema(components: &mut #presence::salvo_oapi::Components) -> #presence::salvo_oapi::RefOr<#presence::salvo_oapi::schema::Schema> {
                #[allow(dead_code)]
                #[derive(#presence::_consume_serde_attrs, #derive_to_schema)]
                #(#proxy_attrs)*
                struct __ToSchemaProxy__<#proxy_generics_params> #proxy_generics_where {
                    #(#proxy_fields)*
                }
                <__ToSchemaProxy__ as #presence::salvo_oapi::ToSchema>::to_schema(components)
            }
        }
    }
}

pub fn derive_enum_presence_aware_to_schema(
    args: &Args,
    derive_to_schema: syn::Path,
    input: &syn::ItemEnum,
) -> TokenStream {
    let Args { presence } = args;
    let enum_name = &input.ident;
    let generics = &input.generics;

    // 1. Extract container attributes (serde and salvo)
    let proxy_attrs = input
        .attrs
        .iter()
        .filter(|attr| attr.path().is_ident("serde") || attr.path().is_ident("salvo"));

    // 2. Map variants and transform their fields
    let proxy_variants = input.variants.iter().map(|variant| {
        let variant_attrs = variant
            .attrs
            .iter()
            .filter(|x| x.path().is_ident("serde") || x.path().is_ident("salvo"));
        let variant_ident = &variant.ident;

        // Transform fields within the variant
        let transformed_fields = variant.fields.iter().map(|field| {
            let field_attrs = field
                .attrs
                .iter()
                .filter(|x| x.path().is_ident("serde") || x.path().is_ident("salvo"));
            let field_name = &field.ident;
            let ty = &field.ty;

            // Apply the same logic as derive_struct_presence_aware_to_schema
            let last_segment = match ty {
                syn::Type::Path(path) => path.path.segments.last(),
                _ => None,
            };

            let mut field_type_tokens = quote!(#ty);
            let mut extra_attrs = quote!();

            if let Some(syn::PathSegment { ident, arguments }) = last_segment {
                let ident_str = ident.to_string();
                if ident_str == "Presence" {
                    extra_attrs = quote!(#[serde(default)]);
                } else if ident_str == "Option" {
                    field_type_tokens = quote!(#presence::salvo_oapi::RenamedOption #arguments);
                }
            }

            // Handle Named vs Unnamed fields for the token stream
            if let Some(name) = field_name {
                quote!(
                    #(#field_attrs)*
                    #extra_attrs
                    #name: #field_type_tokens
                )
            } else {
                quote!(
                    #(#field_attrs)*
                    #extra_attrs
                    #field_type_tokens
                )
            }
        });

        // Reconstruct the variant structure
        match &variant.fields {
            syn::Fields::Named(_) => {
                quote!(
                    #(#variant_attrs)*
                    #variant_ident { #(#transformed_fields),* }
                )
            }
            syn::Fields::Unnamed(_) => {
                quote!(
                    #(#variant_attrs)*
                    #variant_ident ( #(#transformed_fields),* )
                )
            }
            syn::Fields::Unit => {
                quote!(
                    #(#variant_attrs)*
                    #variant_ident
                )
            }
        }
    });

    // 3. Prepare generics
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let proxy_generics_params = &generics.params;
    let proxy_generics_where = &generics.where_clause;

    // 4. Final implementation block
    quote! {
        impl #impl_generics #presence::salvo_oapi::ToSchema for #enum_name #ty_generics #where_clause {
            fn to_schema(components: &mut #presence::salvo_oapi::Components) -> #presence::salvo_oapi::RefOr<#presence::salvo_oapi::schema::Schema> {
                #[allow(dead_code)]
                #[derive(#presence::_consume_serde_attrs, #derive_to_schema)]
                #(#proxy_attrs)*
                enum __ToSchemaProxy__<#proxy_generics_params> #proxy_generics_where {
                    #(#proxy_variants),*
                }

                <__ToSchemaProxy__<#proxy_generics_params> as #presence::salvo_oapi::ToSchema>::to_schema(components)
            }
        }
    }
}
