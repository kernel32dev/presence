
use std::collections::HashSet;
use syn::{GenericArgument, GenericParam, Generics, Ident, PathArguments, Type};

/// Checks if a `syn::Type` contains any of the generic type parameters from a `syn::Generics` block.
///
/// # Arguments
///
/// * `ty` - A reference to the `syn::Type` to inspect.
/// * `generics` - A reference to the `syn::Generics` block that defines the generic parameters.
///
/// # Returns
///
/// * `true` if the type uses one of the generic parameters, `false` otherwise.
pub(crate) fn type_mentions_generics(ty: &Type, generics: &Generics) -> bool {
    // 1. Collect all type parameter identifiers from the `Generics` block into a HashSet.
    // This provides fast O(1) average-time complexity for lookups.
    let generic_type_params: HashSet<_> = generics
        .params
        .iter()
        .filter_map(|param| {
            if let GenericParam::Type(type_param) = param {
                Some(&type_param.ident)
            } else {
                None // We only care about type generics (e.g., `T`), not lifetimes or const generics.
            }
        })
        .collect();

    // If there are no generic type parameters to look for, we can stop early.
    if generic_type_params.is_empty() {
        return false;
    }

    // 2. Call the recursive helper function to traverse the type.
    let type_mentions_generics = type_mentions_generics_recursive(ty, &generic_type_params);

    // if type_mentions_generics {
    //     eprintln!("the generics {generic_type_params:?} were found in {}", ty.to_token_stream());
    // } else {
    //     eprintln!("the generics {generic_type_params:?} were NOT found in {}", ty.to_token_stream());
    // }

    type_mentions_generics
}

/// Recursive helper function to traverse a `syn::Type` and check for generic parameter usage.
fn type_mentions_generics_recursive(ty: &Type, generic_params: &HashSet<&Ident>) -> bool {
    match ty {
        // This is the most common case, e.g., `T`, `String`, `Vec<T>`.
        Type::Path(type_path) => {
            // Check the `QSelf` part of a path.**
            // This handles cases like `<T as Iterator>::Item`. We must recurse on `T`.
            if let Some(qself) = &type_path.qself {
                if type_mentions_generics_recursive(&qself.ty, generic_params) {
                    return true;
                }
            }

            // Check if the path starts with a generic.**
            // This handles associated types like `T::Item`. We check if the *first* segment
            // is a generic parameter identifier.
            if type_path.qself.is_none() && !type_path.path.segments.is_empty() {
                let first_segment = type_path.path.segments.first().unwrap();
                if first_segment.arguments.is_none() && generic_params.contains(&first_segment.ident) {
                    return true;
                }
            }

            // Check within the generic arguments of any path segment (e.g., the `T` in `Vec<T>`).
            // This part of the logic remains essential.
            for segment in &type_path.path.segments {
                if let PathArguments::AngleBracketed(args) = &segment.arguments {
                    for arg in &args.args {
                        if let GenericArgument::Type(inner_ty) = arg {
                            // Recurse on the inner type.
                            if type_mentions_generics_recursive(inner_ty, generic_params) {
                                return true;
                            }
                        }
                    }
                }
            }

            false
        }

        // For composite types, recurse on their inner types.
        Type::Array(ty) => type_mentions_generics_recursive(&ty.elem, generic_params),
        Type::Slice(ty) => type_mentions_generics_recursive(&ty.elem, generic_params),
        Type::Reference(ty) => type_mentions_generics_recursive(&ty.elem, generic_params),
        Type::Ptr(ty) => type_mentions_generics_recursive(&ty.elem, generic_params),
        Type::Paren(ty) => type_mentions_generics_recursive(&ty.elem, generic_params),
        Type::Group(ty) => type_mentions_generics_recursive(&ty.elem, generic_params),

        // For tuples, check if *any* element mentions a generic.
        Type::Tuple(ty) => ty
            .elems
            .iter()
            .any(|elem| type_mentions_generics_recursive(elem, generic_params)),

        // For function pointers (`fn(A) -> B`), check all inputs and the output type.
        Type::BareFn(ty) => {
            let input_mentions = ty
                .inputs
                .iter()
                .any(|arg| type_mentions_generics_recursive(&arg.ty, generic_params));

            let output_mentions = if let syn::ReturnType::Type(_, ret_ty) = &ty.output {
                type_mentions_generics_recursive(ret_ty, generic_params)
            } else {
                false
            };

            input_mentions || output_mentions
        }
        
        // For trait objects (`dyn Trait<T>`), check the types in the trait bounds.
        Type::TraitObject(ty) => ty.bounds.iter().any(|bound| {
            if let syn::TypeParamBound::Trait(trait_bound) = bound {
                // A trait bound is essentially a path, so we can reuse path logic.
                // We create a temporary Type::Path to recursively call our function.
                let temp_type = Type::Path(syn::TypePath { qself: None, path: trait_bound.path.clone() });
                return type_mentions_generics_recursive(&temp_type, generic_params);
            }
            false
        }),

        // Types that cannot contain other types, so they cannot mention our generics.
        Type::Infer(_) | Type::Never(_) => false,

        // For unhandled or complex types like macros, we default to false for safety.
        _ => false,
    }
}
