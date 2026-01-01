mod args;
mod debug;
pub(crate) mod generics;
mod salvo;
mod serde;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Item};

use crate::args::Args;

/// makes a struct or enum presence aware
///
/// add this macro **before** the `#[derive(...)]` attribute
///
/// adding this attribute will override the implementation of the `Debug` derive macro (if present) to omit fields that are `Presence::Absent` and to not write the redudant `Present` to the output on fields that are `Presence::Present`
///
/// it will also override the implementation of the `serde::Serialize` derive macro (if present) to omit fields that are `Presence::Absent` and to not write the redudant `Present` to the output on fields that are `Presence::Present`
///
/// and, it will also override the implementation of the `serde::Deserialize` derive macro (if present) to represent omitted fields with `Presence::Absent` and fields that were specified with `Present`
///
/// finally, it will also override the implementation of the `serde_oapi::ToSchema` derive macro (if present and if the `salvo` feature is enabled) to emit `Presence` fields as not required, and `Option` fields as required (the default by salvo is for nullable fields also be ommitable, presence_aware overrides that)
///
/// additionally you can override the path to the presence crate by usign this syntax: `#[presence_aware(crate = ::my_renamed_presence_import)]`, this is usefull if you rename the crate when depending on it
#[proc_macro_attribute]
pub fn presence_aware(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as Args);
    let mut item = parse_macro_input!(input as Item);

    let impls = match &mut item {
        Item::Struct(item_struct) => {
            let [derive_debug, derive_serialize, derive_deserialize, derive_to_schema] =
                take_relevant_derives(&mut item_struct.attrs);
            [
                derive_debug.map(|derive_debug| {
                    debug::impl_struct_debug_presence(&args, derive_debug, &item_struct)
                }),
                derive_serialize.map(|derive_serialize| {
                    serde::derive_struct_presence_aware_serialize(
                        &args,
                        derive_serialize,
                        item_struct,
                    )
                }),
                derive_deserialize.map(|derive_deserialize| {
                    serde::derive_struct_presence_aware_deserialize(
                        &args,
                        derive_deserialize,
                        item_struct,
                    )
                }),
                derive_to_schema.map(|derive_to_schema| {
                    #[cfg(not(feature = "salvo"))]
                    return unreachable!(
                        "derive_to_schema should never be Some if the salvo feature is not enabled"
                    );
                    #[cfg(feature = "salvo")]
                    return salvo::derive_struct_presence_aware_to_schema(
                        &args,
                        derive_to_schema,
                        item_struct,
                    );
                }),
            ]
        }
        Item::Enum(item_enum) => {
            let [derive_debug, derive_serialize, derive_deserialize, derive_to_schema] =
                take_relevant_derives(&mut item_enum.attrs);
            [
                derive_debug.map(|derive_debug| {
                    debug::impl_enum_debug_presence(&args, derive_debug, item_enum)
                }),
                derive_serialize.map(|derive_serialize| {
                    serde::derive_enum_presence_aware_serialize(&args, derive_serialize, item_enum)
                }),
                derive_deserialize.map(|derive_deserialize| {
                    serde::derive_enum_presence_aware_deserialize(
                        &args,
                        derive_deserialize,
                        item_enum,
                    )
                }),
                derive_to_schema.map(|derive_to_schema| {
                    #[cfg(not(feature = "salvo"))]
                    return unreachable!(
                        "derive_to_schema should never be Some if the salvo feature is not enabled"
                    );
                    #[cfg(feature = "salvo")]
                    return salvo::derive_enum_presence_aware_to_schema(
                        &args,
                        derive_to_schema,
                        item_enum,
                    );
                }),
            ]
        }
        _ => panic!("the presence_aware attribute can only be used on structs or enums"),
    };

    let remove_serde_attrs = |attrs: &mut Vec<syn::Attribute>| {
        for index in (0..attrs.len()).rev() {
            if attrs[index].path().is_ident("serde") {
                attrs.remove(index);
            }
        }
    };

    if impls[1].is_some() || impls[2].is_some() {
        // serde derivations occoured, remove the #[serde] attributes from the struct
        match &mut item {
            Item::Struct(item_struct) => {
                remove_serde_attrs(&mut item_struct.attrs);
                for field in &mut item_struct.fields {
                    remove_serde_attrs(&mut field.attrs);
                }
            }
            Item::Enum(item_enum) => {
                remove_serde_attrs(&mut item_enum.attrs);
                for variant in &mut item_enum.variants {
                    remove_serde_attrs(&mut variant.attrs);
                    for field in &mut variant.fields {
                        remove_serde_attrs(&mut field.attrs);
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    quote!(#item #(#impls)*).into()
}

/// the purpose of this derive macro is to allow structs to have serde attributes without having to derive serde on them
///
/// rust not allow attributes on a struct that are not used by any derives, so we have this noop derive to use them
#[cfg(feature = "salvo")]
#[proc_macro_derive(_consume_serde_attrs, attributes(serde))]
pub fn _consume_serde_attrs(_: TokenStream) -> TokenStream {
    TokenStream::new()
}

/// ```ignore
/// (derive_debug, derive_serialize, derive_deserialize, #[cfg(feature = "salvo")] derive_to_schema)
/// ```
fn take_relevant_derives(attrs: &mut Vec<syn::Attribute>) -> [Option<syn::Path>; 4] {
    let mut derives = [const { None }; 4];
    for attr in attrs {
        let syn::Meta::List(meta_list) = &mut attr.meta else {
            continue;
        };
        if !meta_list.path.is_ident("derive") {
            continue;
        }

        let PathList(paths) = syn::parse2(meta_list.tokens.clone()).unwrap();

        let PathList(paths) = PathList(
            paths
                .into_iter()
                .filter_map(|path| {
                    let last = path
                        .segments
                        .last()
                        .map(|x| x.ident.to_string())
                        .unwrap_or_default();
                    let index = match last.as_str() {
                        "Debug" => 0,
                        "Serialize" => 1,
                        "Deserialize" => 2,
                        #[cfg(feature = "salvo")]
                        "ToSchema" => 3,
                        _ => return Some(path),
                    };
                    derives[index].get_or_insert(path);
                    None
                })
                .collect(),
        );

        meta_list.tokens = quote!(#paths);
    }
    return derives;

    struct PathList(syn::punctuated::Punctuated<syn::Path, syn::token::Comma>);

    impl syn::parse::Parse for PathList {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            syn::punctuated::Punctuated::<syn::Path, syn::token::Comma>::parse_terminated(input)
                .map(Self)
        }
    }

    impl quote::ToTokens for PathList {
        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            let items = &self.0;
            tokens.extend(quote! { #items });
        }
    }
}
