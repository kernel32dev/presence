use syn::{
    parse::{Parse, ParseStream},
    Path, Token,
};

// Represents the parsed arguments for the `presence_aware` macro.
pub struct Args {
    /// the user specifed presence crate path
    pub presence: Path,
}

impl Parse for Args {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut crate_path = None;

        if !input.is_empty() {
            for arg in input.parse_terminated(syn::Meta::parse, Token![,])? {
                match arg {
                    syn::Meta::NameValue(nv) => {
                        if nv.path.is_ident("crate") {
                            if let syn::Expr::Path(expr_path) = nv.value {
                                crate_path = Some(expr_path.path);
                            } else {
                                return Err(syn::Error::new_spanned(
                                    nv.value,
                                    "Expected a path for the `crate` argument",
                                ));
                            }
                        } else {
                            return Err(syn::Error::new_spanned(nv.path, "Unknown argument"));
                        }
                    }
                    _ => {
                        return Err(syn::Error::new_spanned(
                            arg,
                            "Expected `key = value` argument",
                        ));
                    }
                }
            }
        }

        Ok(Args {
            presence: crate_path.unwrap_or_else(|| syn::parse_quote! { ::presence }),
        })
    }
}
