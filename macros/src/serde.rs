use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{ItemStruct, Lifetime};

use crate::{generics::type_mentions_generics, Args};

// TODO! serde ignores skip_serializing_if on tuples of size 1, these should not support having a Presence in them and should instead forward the serialize perfectly

// TODO! handle the user defined functions passed in serde attributes so that the proxy structs and its fields are not exposed to user code

// TODO! reutilize repeated code

pub fn derive_struct_presence_aware_serialize(
    args: &Args,
    derive_serialize: syn::Path,
    input: &ItemStruct,
) -> TokenStream {
    let Args { presence } = args;
    let struct_name = &input.ident;
    let mut generics = input.generics.clone();

    // make the where clause so we can add requirements for the fields to be PresenceAwareSerialize
    let where_clause = generics.make_where_clause();

    // prepare an utility to wrap types in #presence::serde::PresenceAwareSerialize<_>
    let presence_aware_serialize_tuple: syn::Path =
        syn::parse_quote! { #presence::serde::PresenceAwareSerialize<()> };
    let presence_aware_serialize = |ty| {
        let mut x = presence_aware_serialize_tuple.clone();
        let syn::PathArguments::AngleBracketed(args) =
            &mut x.segments.last_mut().unwrap().arguments
        else {
            panic!();
        };
        args.args.clear();
        args.args.push(syn::GenericArgument::Type(ty));
        x
    };

    // prepare the bounds that all #presence::serde::PresenceAwareSerialize<{field_type}> must be to implement serialize
    let bounds = syn::punctuated::Punctuated::from_iter([
        syn::TypeParamBound::Trait(syn::TraitBound {
            paren_token: None,
            modifier: syn::TraitBoundModifier::None,
            lifetimes: None,
            path: syn::parse_quote! { #presence::serde::Serialize },
        }),
        syn::TypeParamBound::Trait(syn::TraitBound {
            paren_token: None,
            modifier: syn::TraitBoundModifier::None,
            lifetimes: None,
            path: syn::parse_quote! { #presence::serde::PresenceAwareSerializeEx },
        }),
    ]);

    // add the bounds for each field
    for field in &input.fields {
        where_clause
            .predicates
            .push(syn::WherePredicate::Type(syn::PredicateType {
                lifetimes: None,
                bounded_ty: syn::Type::Path(syn::TypePath {
                    qself: None,
                    // TODO! there is a very strage E0283 error that happens when the same bound is places on two identical types that only differ in the lifetimes
                    // making them HOTB (with the for<'...> ...) syntax seems to resolve the issue and allow repeats, but does not seem to be a complete solution
                    // as of currently this prevents structs with more than one lifetime from working with Debug, Serialize and Deserialize
                    path: presence_aware_serialize(field.ty.clone()),
                }),
                colon_token: syn::Token![:](proc_macro2::Span::call_site()),
                bounds: bounds.clone(),
            }));
    }

    // now we start preparing things needed by the proxy

    // first the lifetime for the references to the real struct
    let proxy_lifetime = Lifetime::new("'__serialization_proxy__", proc_macro2::Span::call_site());

    // then the container atributes
    let mut proxy_serde_attrs = input
        .attrs
        .iter()
        .filter(|attr| attr.path().is_ident("serde"))
        .cloned()
        .collect::<Vec<_>>();

    // the path to the serde crate for serde derive to use
    let proxy_serde_crate = match serde_config::remove(&mut proxy_serde_attrs, "crate") {
        Some(Some(crate_name)) => crate_name,
        None | Some(None) => format!("{}::serde", presence.to_token_stream()),
    };

    // the where clauses needed for the proxy to be serializable (none as they are put in the struct bounds)
    let proxy_serde_bound = String::new();

    // then the params, it needs the extra lifetime at the beginning
    let mut proxy_generics_params = generics.params.clone();
    proxy_generics_params.insert(
        0,
        syn::GenericParam::Lifetime(syn::LifetimeParam::new(proxy_lifetime.clone())),
    );

    // then the where clause, no extra predicates needed
    let proxy_generics_where = generics.where_clause.as_ref();

    let skip_serializing_if = format!(
        "{}::serde::PresenceAwareSerialize::skip_serializing_if",
        presence.to_token_stream()
    );

    // now we define the types for each one wrapping in presence_aware_serialize and adding skip_serializing_if to ensure absent fields don't get serialize (the whole point of this macro)
    let proxy_fields = input.fields.iter().map(|field| {
        let attrs = field.attrs.iter().filter(|x| x.path().is_ident("serde"));
        let name = &field.ident;
        let ty = presence_aware_serialize(field.ty.clone());
        // TODO! handle conflict between user defined skip_serializing_if and our attribute
        quote!(
            #(#attrs)*
            #[serde(skip_serializing_if = #skip_serializing_if)]
            #name: &#proxy_lifetime #ty,
        )
    });

    // and the expressions to construct the proxy
    let proxy_field_moves = input.fields.iter().map(|field| {
        let name = &field.ident;
        quote!(
            #name: #presence::serde::PresenceAwareSerialize::new(&self.#name),
        )
    });

    // now combine everything
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    quote! {
        impl #impl_generics #presence::serde::Serialize for #struct_name #ty_generics #where_clause {
            fn serialize<S>(&self, serializer: S) -> ::core::result::Result<S::Ok, S::Error>
            where
                S: #presence::serde::Serializer,
            {
                #[derive(#derive_serialize)]
                #[serde(crate = #proxy_serde_crate, bound = #proxy_serde_bound)]
                #(#proxy_serde_attrs)*
                struct __SerializationProxy__<#proxy_generics_params> #proxy_generics_where {
                    #(#proxy_fields)*
                }

                #presence::serde::Serialize::serialize(&__SerializationProxy__ {
                    #(#proxy_field_moves)*
                }, serializer)
            }
        }
    }
}

pub fn derive_struct_presence_aware_deserialize(
    args: &Args,
    derive_deserialize: syn::Path,
    input: &ItemStruct,
) -> TokenStream {
    let Args { presence } = args;
    let struct_name = &input.ident;
    let de = Lifetime::new("'de", proc_macro2::Span::call_site());

    // prepare an utility to wrap types in #presence::serde::PresenceAwareDeserialize<_>
    let presence_aware_deserialize_tuple: syn::Path =
        syn::parse_quote! { #presence::serde::PresenceAwareDeserialize<()> };
    let presence_aware_deserialize = |ty| {
        let mut x = presence_aware_deserialize_tuple.clone();
        let syn::PathArguments::AngleBracketed(args) =
            &mut x.segments.last_mut().unwrap().arguments
        else {
            panic!();
        };
        args.args.clear();
        args.args.push(syn::GenericArgument::Type(ty));
        x
    };

    // first we prepare the where predicates that have to hold for the fields to be deserializable
    let bounds = {
        // prepare the bounds that all #presence::serde::PresenceAwareDeserialize<{field_type}> must be to implement serialize
        let bounds_for_presence_aware_deserialize =
            syn::punctuated::Punctuated::from_iter([syn::TypeParamBound::Trait(syn::TraitBound {
                paren_token: None,
                modifier: syn::TraitBoundModifier::None,
                lifetimes: None,
                path: syn::parse_quote! { #presence::serde::Deserialize<#de> },
            })]);
        // prepare the bounds that all fields must be to implement serialize
        let bounds_for_field =
            syn::punctuated::Punctuated::from_iter([syn::TypeParamBound::Trait(syn::TraitBound {
                paren_token: None,
                modifier: syn::TraitBoundModifier::None,
                lifetimes: None,
                path: syn::parse_quote! { #presence::serde::PresenceAwareDeserializeEx<#de> },
            })]);

        // prepare the deserialization bounds for each field
        input
            .fields
            .iter()
            .filter(|field| type_mentions_generics(&field.ty, &input.generics))
            .flat_map(|field| {
                [
                    syn::WherePredicate::Type(syn::PredicateType {
                        lifetimes: None,
                        bounded_ty: syn::Type::Path(syn::TypePath {
                            qself: None,
                            // TODO! there is a very strage E0283 error that happens when the same bound is places on two identical types that only differ in the lifetimes
                            // making them HOTB (with the for<'...> ...) syntax seems to resolve the issue and allow repeats, but does not seem to be a complete solution
                            // as of currently this prevents structs with more than one lifetime from working with Debug, Serialize and Deserialize
                            path: presence_aware_deserialize(field.ty.clone()),
                        }),
                        colon_token: syn::Token![:](proc_macro2::Span::call_site()),
                        bounds: bounds_for_presence_aware_deserialize.clone(),
                    }),
                    syn::WherePredicate::Type(syn::PredicateType {
                        lifetimes: None,
                        // TODO! there is a very strage E0283 error that happens when the same bound is places on two identical types that only differ in the lifetimes
                        // making them HOTB (with the for<'...> ...) syntax seems to resolve the issue and allow repeats, but does not seem to be a complete solution
                        // as of currently this prevents structs with more than one lifetime from working with Debug, Serialize and Deserialize
                        bounded_ty: field.ty.clone(),
                        colon_token: syn::Token![:](proc_macro2::Span::call_site()),
                        bounds: bounds_for_field.clone(),
                    }),
                ]
            })
            .collect::<Vec<_>>()
    };

    // second we prepare the things needed by the impl
    let (impl_generics_with_de, impl_generics_without_de, impl_where_clause) = {
        // this generic will be used only for the impl_generics (the impl<...> part) which needs an extra lifetime 'de, which must not be present in ty_generics (the StructName<...> part)
        let mut impl_generics_with_de = input.generics.clone();
        // add the de lifetime to the impl_generics, affecting only the impl
        impl_generics_with_de.params.insert(
            0,
            // TODO! this adds bounds to 'de for every lifetime in the struct, this might be unecessary, either add the bound only for the lifetimes needed or change this comment to explain why it is necessary
            syn::GenericParam::Lifetime(syn::LifetimeParam {
                attrs: Vec::new(),
                lifetime: de.clone(),
                colon_token: Some(syn::Token![:](proc_macro2::Span::call_site())),
                bounds: input
                    .generics
                    .params
                    .iter()
                    .filter_map(|x| match x {
                        syn::GenericParam::Lifetime(lifetime_param) => {
                            Some(lifetime_param.lifetime.clone())
                        }
                        syn::GenericParam::Type(_) | syn::GenericParam::Const(_) => None,
                    })
                    .collect(),
            }),
        );

        // this generic will be used only for the ty_generics (the StructName<...> part) which is already correct, and does not need the extra lifetime 'de
        let impl_generics_without_de = &input.generics;

        // clone and add the bounds needed for deserialization
        let mut impl_where_clause =
            input
                .generics
                .where_clause
                .clone()
                .unwrap_or_else(|| syn::WhereClause {
                    where_token: Default::default(),
                    predicates: Default::default(),
                });
        impl_where_clause.predicates.extend(bounds.iter().cloned());

        (
            impl_generics_with_de,
            impl_generics_without_de,
            impl_where_clause,
        )
    };
    // isolate the parts we care about
    let (impl_generics, _, _) = impl_generics_with_de.split_for_impl();
    let (_, impl_ty_generics, _) = impl_generics_without_de.split_for_impl();

    // third we prepare things needed by the proxy

    // first the params, no extra params needed
    let proxy_generics_params = &input.generics.params;

    // then the where clause, no extra predicates needed
    let proxy_generics_where = input.generics.where_clause.as_ref();

    // then the container atributes
    let mut proxy_serde_attrs = input
        .attrs
        .iter()
        .filter(|attr| attr.path().is_ident("serde"))
        .cloned()
        .collect::<Vec<_>>();

    // the path to the serde crate for serde derive to use
    let proxy_serde_crate = match serde_config::remove(&mut proxy_serde_attrs, "crate") {
        Some(Some(crate_name)) => crate_name,
        None | Some(None) => format!("{}::serde", presence.to_token_stream()),
    };

    // the where clauses needed for the proxy to be deserializable
    let proxy_serde_bound = {
        let mut buffer = String::new();
        for bound in bounds {
            std::fmt::Write::write_fmt(&mut buffer, format_args!("{},", bound.into_token_stream()))
                .unwrap();
        }
        buffer
    };

    // the fields of the proxy struct, with the #[serde(default)] attribute.
    let proxy_fields = input.fields.iter().map(|field| {
        let mut attrs = field
            .attrs
            .iter()
            .filter(|x| x.path().is_ident("serde"))
            .cloned()
            .collect::<Vec<_>>();
        serde_config::remove(&mut attrs, "default");
        let name = &field.ident;
        let ty = presence_aware_deserialize(field.ty.clone());
        quote! {
            #(#attrs)*
            #[serde(default)]
            #name: #ty,
        }
    });

    // the construction of the final struct from the deserialized proxy.
    let proxy_field_destructuring = input
        .fields
        .iter()
        .map(|field| {
            let name = field.ident.as_ref().unwrap();
            let name_str = name.to_string();
            match serde_config::read_path(&field.attrs, "default") {
                Some(Some(default_with)) => quote! {
                    #name: #presence::serde::PresenceAwareDeserializeEx::resolve_presence(proxy.#name.0).unwrap_or_else(#default_with),
                },
                Some(None) => quote! {
                    #name: #presence::serde::PresenceAwareDeserializeEx::resolve_presence(proxy.#name.0).unwrap_or_default(),
                },
                None => quote! {
                    #name: match #presence::serde::PresenceAwareDeserializeEx::resolve_presence(proxy.#name.0) {
                        ::core::option::Option::None => return Err(#presence::serde::de::Error::missing_field(#name_str)),
                        ::core::option::Option::Some(value) => value,
                    },
                },
                // None => quote! {
                //     #name: match #presence::serde::PresenceAwareDeserializeEx::resolve_presence(proxy.#name.0) {
                //         ::core::option::Option::None => return Err(#presence::serde::de::Error::missing_field(#name_str)),
                //         ::core::option::Option::Some(value) => value,
                //     },
                // },
            }
        })
        .collect::<Box<[_]>>();

    quote! {
        impl #impl_generics #presence::serde::Deserialize<#de> for #struct_name #impl_ty_generics #impl_where_clause {
            fn deserialize<D>(deserializer: D) -> ::core::result::Result<Self, D::Error>
            where
                D: #presence::serde::Deserializer<#de>,
            {
                #[derive(#derive_deserialize)]
                #[serde(crate = #proxy_serde_crate, bound = #proxy_serde_bound)]
                #(#proxy_serde_attrs)*
                struct __DeserializationProxy__<#proxy_generics_params> #proxy_generics_where {
                    #(#proxy_fields)*
                }

                let proxy = <__DeserializationProxy__ #impl_ty_generics as #presence::serde::Deserialize<#de>>::deserialize(deserializer)?;

                let result = #struct_name {
                    #(#proxy_field_destructuring)*
                };

                Ok(result)
            }
        }
    }
}

pub fn derive_enum_presence_aware_serialize(
    args: &Args,
    derive_serialize: syn::Path,
    input: &syn::ItemEnum,
) -> TokenStream {
    let Args { presence } = args;
    let enum_name = &input.ident;
    let mut generics = input.generics.clone();

    // make the where clause so we can add requirements for the fields to be PresenceAwareSerialize
    let where_clause = generics.make_where_clause();

    // prepare an utility to wrap types in #presence::serde::PresenceAwareSerialize<_>
    let presence_aware_serialize_tuple: syn::Path =
        syn::parse_quote! { #presence::serde::PresenceAwareSerialize<()> };
    let presence_aware_serialize = |ty| {
        let mut x = presence_aware_serialize_tuple.clone();
        let syn::PathArguments::AngleBracketed(args) =
            &mut x.segments.last_mut().unwrap().arguments
        else {
            panic!();
        };
        args.args.clear();
        args.args.push(syn::GenericArgument::Type(ty));
        x
    };

    // prepare the bounds that all #presence::serde::PresenceAwareSerialize<{field_type}> must be to implement serialize
    let bounds = syn::punctuated::Punctuated::from_iter([
        syn::TypeParamBound::Trait(syn::TraitBound {
            paren_token: None,
            modifier: syn::TraitBoundModifier::None,
            lifetimes: None,
            path: syn::parse_quote! { #presence::serde::Serialize },
        }),
        syn::TypeParamBound::Trait(syn::TraitBound {
            paren_token: None,
            modifier: syn::TraitBoundModifier::None,
            lifetimes: None,
            path: syn::parse_quote! { #presence::serde::PresenceAwareSerializeEx },
        }),
    ]);

    // add the bounds for each field in each variant
    for variant in &input.variants {
        for field in &variant.fields {
            where_clause
                .predicates
                .push(syn::WherePredicate::Type(syn::PredicateType {
                    lifetimes: None,
                    bounded_ty: syn::Type::Path(syn::TypePath {
                        qself: None,
                        // TODO! there is a very strage E0283 error that happens when the same bound is places on two identical types that only differ in the lifetimes
                        // making them HOTB (with the for<'...> ...) syntax seems to resolve the issue and allow repeats, but does not seem to be a complete solution
                        // as of currently this prevents structs with more than one lifetime from working with Debug, Serialize and Deserialize
                        path: presence_aware_serialize(field.ty.clone()),
                    }),
                    colon_token: syn::Token![:](proc_macro2::Span::call_site()),
                    bounds: bounds.clone(),
                }));
        }
    }

    // now we start preparing things needed by the proxy

    // first the lifetime for the references to the real enum
    let proxy_lifetime = Lifetime::new("'__serialization_proxy__", proc_macro2::Span::call_site());

    // then the container atributes
    let mut proxy_serde_attrs = input
        .attrs
        .iter()
        .filter(|attr| attr.path().is_ident("serde"))
        .cloned()
        .collect::<Vec<_>>();

    // the path to the serde crate for serde derive to use
    let proxy_serde_crate = match serde_config::remove(&mut proxy_serde_attrs, "crate") {
        Some(Some(crate_name)) => crate_name,
        None | Some(None) => format!("{}::serde", presence.to_token_stream()),
    };

    // the where clauses needed for the proxy to be serializable (none as they are put in the struct bounds)
    let proxy_serde_bound = String::new();

    // then the params, it needs the extra lifetime at the beginning
    let mut proxy_generics_params = generics.params.clone();
    proxy_generics_params.insert(
        0,
        syn::GenericParam::Lifetime(syn::LifetimeParam::new(proxy_lifetime.clone())),
    );

    // then the where clause, no extra predicates needed
    let proxy_generics_where = generics.where_clause.as_ref();

    let skip_serializing_if = format!(
        "{}::serde::PresenceAwareSerialize::skip_serializing_if",
        presence.to_token_stream()
    );

    // now we define the variants for each one wrapping in presence_aware_serialize and adding skip_serializing_if to ensure absent fields don't get serialized (the whole point of this macro)
    let proxy_variants = input.variants.iter().map(|variant| {
        let attrs = variant.attrs.iter().filter(|x| x.path().is_ident("serde"));
        let name = &variant.ident;
        let fields = variant.fields.iter().map(|field| {
            let field_attrs = field.attrs.iter().filter(|x| x.path().is_ident("serde"));
            let field_name = &field.ident;
            let ty = presence_aware_serialize(field.ty.clone());
            // TODO! handle conflict between user defined skip_serializing_if and our attribute
            if let Some(field_name) = field_name {
                quote!(
                    #(#field_attrs)*
                    #[serde(skip_serializing_if = #skip_serializing_if)]
                    #field_name: &#proxy_lifetime #ty,
                )
            } else {
                quote!(
                    #(#field_attrs)*
                    #[serde(skip_serializing_if = #skip_serializing_if)]
                    &#proxy_lifetime #ty,
                )
            }
        });

        match &variant.fields {
            syn::Fields::Named(_) => {
                quote!(
                    #(#attrs)*
                    #name { #(#fields)* },
                )
            }
            syn::Fields::Unnamed(_) => {
                quote!(
                    #(#attrs)*
                    #name ( #(#fields)* ),
                )
            }
            syn::Fields::Unit => {
                quote!(
                    #(#attrs)*
                    #name,
                )
            }
        }
    });

    // and the expressions to construct the proxy
    let proxy_variant_moves = input.variants.iter().map(|variant| {
        let name = &variant.ident;
        let fields = variant.fields.iter().enumerate().map(|(i, field)| {
            if let Some(field_name) = &field.ident {
                quote!(#field_name: #presence::serde::PresenceAwareSerialize::new(#field_name))
            } else {
                let field_name = syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site());
                quote!(#presence::serde::PresenceAwareSerialize::new(#field_name))
            }
        });
        let field_names = variant.fields.iter().map(|field| &field.ident);
        let field_pats = variant.fields.iter().enumerate().map(|(i, field)| {
            if let Some(name) = &field.ident {
                quote!(#name)
            } else {
                let name = syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site());
                quote!(#name)
            }
        });

        match &variant.fields {
            syn::Fields::Named(_) => {
                quote!(
                    #enum_name::#name { #(#field_names),* } => __SerializationProxy__::#name {
                        #(#fields),*
                    }
                )
            }
            syn::Fields::Unnamed(_) => {
                quote!(
                    #enum_name::#name ( #(#field_pats),* ) => __SerializationProxy__::#name ( #(#fields),* )
                )
            }
            syn::Fields::Unit => {
                quote!(
                    #enum_name::#name => __SerializationProxy__::#name
                )
            }
        }
    });

    // now combine everything
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    quote! {
        impl #impl_generics #presence::serde::Serialize for #enum_name #ty_generics #where_clause {
            fn serialize<S>(&self, serializer: S) -> ::core::result::Result<S::Ok, S::Error>
            where
                S: #presence::serde::Serializer,
            {
                #[derive(#derive_serialize)]
                #[serde(crate = #proxy_serde_crate, bound = #proxy_serde_bound)]
                #(#proxy_serde_attrs)*
                enum __SerializationProxy__<#proxy_generics_params> #proxy_generics_where {
                    #(#proxy_variants)*
                }

                let proxy = match self {
                    #(#proxy_variant_moves),*
                };

                #presence::serde::Serialize::serialize(&proxy, serializer)
            }
        }
    }
}

pub fn derive_enum_presence_aware_deserialize(
    args: &Args,
    derive_deserialize: syn::Path,
    input: &syn::ItemEnum,
) -> TokenStream {
    let Args { presence } = args;
    let enum_name = &input.ident;
    let de = Lifetime::new("'de", proc_macro2::Span::call_site());

    // prepare an utility to wrap types in #presence::serde::PresenceAwareDeserialize<_>
    let presence_aware_deserialize_tuple: syn::Path =
        syn::parse_quote! { #presence::serde::PresenceAwareDeserialize<()> };
    let presence_aware_deserialize = |ty| {
        let mut x = presence_aware_deserialize_tuple.clone();
        let syn::PathArguments::AngleBracketed(args) =
            &mut x.segments.last_mut().unwrap().arguments
        else {
            panic!();
        };
        args.args.clear();
        args.args.push(syn::GenericArgument::Type(ty));
        x
    };

    // first we prepare the where predicates that have to hold for the fields to be deserializable
    let bounds = {
        // prepare the bounds that all #presence::serde::PresenceAwareDeserialize<{field_type}> must be to implement serialize
        let bounds_for_presence_aware_deserialize =
            syn::punctuated::Punctuated::from_iter([syn::TypeParamBound::Trait(syn::TraitBound {
                paren_token: None,
                modifier: syn::TraitBoundModifier::None,
                lifetimes: None,
                path: syn::parse_quote! { #presence::serde::Deserialize<#de> },
            })]);
        // prepare the bounds that all fields must be to implement serialize
        let bounds_for_field =
            syn::punctuated::Punctuated::from_iter([syn::TypeParamBound::Trait(syn::TraitBound {
                paren_token: None,
                modifier: syn::TraitBoundModifier::None,
                lifetimes: None,
                path: syn::parse_quote! { #presence::serde::PresenceAwareDeserializeEx<#de> },
            })]);

        // prepare the deserialization bounds for each field in each variant
        input
            .variants
            .iter()
            .flat_map(|variant| {
                variant
                    .fields
                    .iter()
                    .filter(|field| type_mentions_generics(&field.ty, &input.generics))
                    .flat_map(|field| {
                        [
                            syn::WherePredicate::Type(syn::PredicateType {
                                lifetimes: None,
                                bounded_ty: syn::Type::Path(syn::TypePath {
                                    qself: None,
                                    // TODO! there is a very strage E0283 error that happens when the same bound is places on two identical types that only differ in the lifetimes
                                    // making them HOTB (with the for<'...> ...) syntax seems to resolve the issue and allow repeats, but does not seem to be a complete solution
                                    // as of currently this prevents structs with more than one lifetime from working with Debug, Serialize and Deserialize
                                    path: presence_aware_deserialize(field.ty.clone()),
                                }),
                                colon_token: syn::Token![:](proc_macro2::Span::call_site()),
                                bounds: bounds_for_presence_aware_deserialize.clone(),
                            }),
                            syn::WherePredicate::Type(syn::PredicateType {
                                lifetimes: None,
                                // TODO! there is a very strage E0283 error that happens when the same bound is places on two identical types that only differ in the lifetimes
                                // making them HOTB (with the for<'...> ...) syntax seems to resolve the issue and allow repeats, but does not seem to be a complete solution
                                // as of currently this prevents structs with more than one lifetime from working with Debug, Serialize and Deserialize
                                bounded_ty: field.ty.clone(),
                                colon_token: syn::Token![:](proc_macro2::Span::call_site()),
                                bounds: bounds_for_field.clone(),
                            }),
                        ]
                    })
            })
            .collect::<Box<[_]>>()
    };

    // second we prepare the things needed by the impl
    let (impl_generics_with_de, impl_generics_without_de, impl_where_clause) = {
        // this generic will be used only for the impl_generics (the impl<...> part) which needs an extra lifetime 'de, which must not be present in ty_generics (the EnumName<...> part)
        let mut impl_generics_with_de = input.generics.clone();
        // add the de lifetime to the impl_generics, affecting only the impl
        impl_generics_with_de.params.insert(
            0,
            // TODO! this adds bounds to 'de for every lifetime in the enum, this might be unecessary, either add the bound only for the lifetimes needed or change this comment to explain why it is necessary
            syn::GenericParam::Lifetime(syn::LifetimeParam {
                attrs: Vec::new(),
                lifetime: de.clone(),
                colon_token: Some(syn::Token![:](proc_macro2::Span::call_site())),
                bounds: input
                    .generics
                    .params
                    .iter()
                    .filter_map(|x| match x {
                        syn::GenericParam::Lifetime(lifetime_param) => {
                            Some(lifetime_param.lifetime.clone())
                        }
                        syn::GenericParam::Type(_) | syn::GenericParam::Const(_) => None,
                    })
                    .collect(),
            }),
        );

        // this generic will be used only for the ty_generics (the EnumName<...> part) which is already correct, and does not need the extra lifetime 'de
        let impl_generics_without_de = &input.generics;

        // clone and add the bounds needed for deserialization
        let mut impl_where_clause =
            input
                .generics
                .where_clause
                .clone()
                .unwrap_or_else(|| syn::WhereClause {
                    where_token: Default::default(),
                    predicates: Default::default(),
                });
        impl_where_clause.predicates.extend(bounds.iter().cloned());

        (
            impl_generics_with_de,
            impl_generics_without_de,
            impl_where_clause,
        )
    };
    // isolate the parts we care about
    let (impl_generics, _, _) = impl_generics_with_de.split_for_impl();
    let (_, impl_ty_generics, _) = impl_generics_without_de.split_for_impl();

    // third we prepare things needed by the proxy

    // first the params, no extra params needed
    let proxy_generics_params = &input.generics.params;

    // then the where clause, no extra predicates needed
    let proxy_generics_where = input.generics.where_clause.as_ref();

    // then the container atributes
    let mut proxy_serde_attrs = input
        .attrs
        .iter()
        .filter(|attr| attr.path().is_ident("serde"))
        .cloned()
        .collect::<Vec<_>>();

    // the path to the serde crate for serde derive to use
    let proxy_serde_crate = match serde_config::remove(&mut proxy_serde_attrs, "crate") {
        Some(Some(crate_name)) => crate_name,
        None | Some(None) => format!("{}::serde", presence.to_token_stream()),
    };

    // the where clauses needed for the proxy to be deserializable
    let proxy_serde_bound = {
        let mut buffer = String::new();
        for bound in bounds {
            std::fmt::Write::write_fmt(&mut buffer, format_args!("{},", bound.into_token_stream()))
                .unwrap();
        }
        buffer
    };

    // the variants of the proxy enum, with the #[serde(default)] attribute on fields for struct-like variants.
    let proxy_variants = input.variants.iter().map(|variant| {
        let name = &variant.ident;
        let attrs = variant
            .attrs
            .iter()
            .filter(|attr| attr.path().is_ident("serde"));
        let fields = variant.fields.iter().map(|field| {
            let mut attrs = input
                .attrs
                .iter()
                .filter(|attr| attr.path().is_ident("serde"))
                .cloned()
                .collect::<Vec<_>>();
            serde_config::remove(&mut attrs, "default");

            let field_name = &field.ident;
            let ty = presence_aware_deserialize(field.ty.clone());

            if let Some(field_name) = field_name {
                quote! {
                    #(#attrs)*
                    #[serde(default)]
                    #field_name: #ty,
                }
            } else {
                quote! {
                    #(#attrs)*
                    #[serde(default)]
                    #ty,
                }
            }
        });

        match &variant.fields {
            syn::Fields::Named(_) => {
                quote! {
                    #(#attrs)*
                    #name { #(#fields)* },
                }
            }
            syn::Fields::Unnamed(_) => {
                quote! {
                    #(#attrs)*
                    #name ( #(#fields)* ),
                }
            }
            syn::Fields::Unit => {
                quote! {
                    #(#attrs)*
                    #name,
                }
            }
        }
    });

    // the construction of the final enum from the deserialized proxy.
    let proxy_variant_destructuring = input.variants.iter().map(|variant| {
        let name = &variant.ident;
        
        match &variant.fields {
            syn::Fields::Named(fields_named) => {
                let destructured_fields = fields_named.named.iter().map(|field| &field.ident);
                let field_moves = fields_named.named.iter().map(|field| {
                    let field_name = field.ident.as_ref().unwrap();
                    let field_name_str = field_name.to_string();
                    match serde_config::read_path(&field.attrs, "default") {
                        Some(Some(default_with)) => quote! {
                            #field_name: #presence::serde::PresenceAwareDeserializeEx::resolve_presence(#field_name.0).unwrap_or_else(#default_with)
                        },
                        Some(None) => quote! {
                            #field_name: #presence::serde::PresenceAwareDeserializeEx::resolve_presence(#field_name.0).unwrap_or_default()
                        },
                        None => quote! {
                            #field_name: match #presence::serde::PresenceAwareDeserializeEx::resolve_presence(#field_name.0) {
                                ::core::option::Option::None => return Err(#presence::serde::de::Error::missing_field(#field_name_str)),
                                ::core::option::Option::Some(value) => value,
                            }
                        },
                    }
                });
                quote! {
                    __DeserializationProxy__::#name { #(#destructured_fields),* } => {
                        #enum_name::#name { #(#field_moves),* }
                    }
                }
            }
            syn::Fields::Unnamed(fields_unnamed) => {
                let destructured_fields = fields_unnamed.unnamed.iter().enumerate().map(|(i, _)| {
                    let name = syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site());
                    quote! { #name }
                });
                let field_moves = fields_unnamed.unnamed.iter().enumerate().map(|(i, field)| {
                    let name = syn::Ident::new(&format!("field_{}", i), proc_macro2::Span::call_site());
                    let name_str = format!("unnamed field {}", i);
                    match serde_config::read_path(&field.attrs, "default") {
                        Some(Some(default_with)) => quote! {
                            #presence::serde::PresenceAwareDeserializeEx::resolve_presence(#name.0).unwrap_or_else(#default_with)
                        },
                        Some(None) => quote! {
                            #presence::serde::PresenceAwareDeserializeEx::resolve_presence(#name.0).unwrap_or_default()
                        },
                        None => quote! {
                            match #presence::serde::PresenceAwareDeserializeEx::resolve_presence(#name.0) {
                                ::core::option::Option::None => return Err(#presence::serde::de::Error::missing_field(#name_str)),
                                ::core::option::Option::Some(value) => value,
                            }
                        },
                    }
                });
                quote! {
                    __DeserializationProxy__::#name ( #(#destructured_fields),* ) => {
                        #enum_name::#name ( #(#field_moves),* )
                    }
                }
            }
            syn::Fields::Unit => {
                quote! {
                    __DeserializationProxy__::#name => {
                        #enum_name::#name
                    }
                }
            }
        }
    });

    quote! {
        impl #impl_generics #presence::serde::Deserialize<#de> for #enum_name #impl_ty_generics #impl_where_clause {
            fn deserialize<D>(deserializer: D) -> ::core::result::Result<Self, D::Error>
            where
                D: #presence::serde::Deserializer<#de>,
            {
                #[derive(#derive_deserialize)]
                #[serde(crate = #proxy_serde_crate, bound = #proxy_serde_bound)]
                #(#proxy_serde_attrs)*
                enum __DeserializationProxy__<#proxy_generics_params> #proxy_generics_where {
                    #(#proxy_variants)*
                }

                let proxy = <__DeserializationProxy__ #impl_ty_generics as #presence::serde::Deserialize<#de>>::deserialize(deserializer)?;

                let result = match proxy {
                    #(#proxy_variant_destructuring),*
                };

                Ok(result)
            }
        }
    }
}

mod serde_config {
    use syn::ext::IdentExt;

    pub(super) fn remove(attrs: &mut [syn::Attribute], key: &str) -> Option<Option<String>> {
        for attr in attrs {
            if !attr.path().is_ident("serde") {
                continue;
            }
            let syn::Meta::List(syn::MetaList { tokens, .. }) = &mut attr.meta else {
                continue;
            };

            let configs = syn::parse2::<KeyValueList>(tokens.clone()).unwrap().0;

            for (index, config) in configs.iter().enumerate() {
                if config.key.to_string() == key {
                    let target = Some(config.value.as_ref().map(syn::LitStr::value));
                    let target_index = index;
                    let configs = KeyValueList(
                        configs
                            .into_iter()
                            .enumerate()
                            .filter(|&(i, _)| i != target_index)
                            .map(|(_, v)| v)
                            .collect(),
                    );
                    *tokens = quote::quote! { #configs };
                    return target;
                }
            }
        }
        None
    }
    pub(super) fn read(attrs: &[syn::Attribute], key: &str) -> Option<Option<String>> {
        for attr in attrs {
            if !attr.path().is_ident("serde") {
                continue;
            }
            let syn::Meta::List(syn::MetaList { tokens, .. }) = &attr.meta else {
                continue;
            };

            let configs = syn::parse2::<KeyValueList>(tokens.clone()).unwrap().0;

            for config in &configs {
                if config.key.to_string() == key {
                    return Some(config.value.as_ref().map(syn::LitStr::value));
                }
            }
        }
        None
    }
    pub(super) fn read_path(attrs: &[syn::Attribute], key: &str) -> Option<Option<syn::Path>> {
        match read(attrs, key) {
            Some(Some(text)) => Some(Some(syn::parse_str(&text).unwrap())),
            Some(None) => Some(None),
            None => None,
        }
    }

    struct KeyValue {
        key: syn::Ident,
        value: Option<syn::LitStr>,
    }

    impl syn::parse::Parse for KeyValue {
        fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
            let key = input.call(syn::Ident::parse_any)?;
            let value = if input.peek(syn::Token![=]) {
                input.parse::<syn::Token![=]>()?;
                Some(input.parse::<syn::LitStr>()?)
            } else {
                None
            };
            Ok(KeyValue { key, value })
        }
    }

    impl quote::ToTokens for KeyValue {
        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            let key = &self.key;
            if let Some(val) = &self.value {
                tokens.extend(quote::quote! { #key = #val });
            } else {
                tokens.extend(quote::quote! { #key });
            }
        }
    }

    struct KeyValueList(syn::punctuated::Punctuated<KeyValue, syn::token::Comma>);

    impl syn::parse::Parse for KeyValueList {
        fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
            let entries =
                syn::punctuated::Punctuated::<KeyValue, syn::token::Comma>::parse_terminated(
                    input,
                )?;
            Ok(KeyValueList(entries))
        }
    }

    impl quote::ToTokens for KeyValueList {
        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            let items = &self.0;
            tokens.extend(quote::quote! { #items });
        }
    }
}
