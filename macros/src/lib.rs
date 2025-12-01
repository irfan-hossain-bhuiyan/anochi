
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, FnArg, GenericArgument, ItemFn, Pat, PathArguments, ReturnType, Type};

fn base_name(orig_name: &syn::Ident) -> String {
    let name_str = orig_name.to_string();
    name_str
        .strip_suffix("_checked")
        .expect("Function name must end with `_checked`")
        .to_string()
}

fn inner_type_of_checked_return(ty: &Type) -> Type {
    if let Type::Path(type_path) = ty {
        let seg = type_path.path.segments.last().expect("Invalid type path");
        match seg.ident.to_string().as_str() {
            "Option" | "Result" => {
                if let PathArguments::AngleBracketed(args) = &seg.arguments {
                    for arg in args.args.iter() {
                        if let GenericArgument::Type(t) = arg {
                            return t.clone();
                        }
                    }
                    panic!("No generic type found in Option/Result");
                } else {
                    panic!("Expected angle-bracketed generic type for Option/Result");
                }
            }
            _ => panic!("Return type must be Option<T> or Result<T,E>"),
        }
    } else {
        panic!("Return type must be Option<T> or Result<T,E>");
    }
}

fn extract_call_args(func: &ItemFn) -> Vec<Pat> {
    func.sig
        .inputs
        .iter()
        .map(|arg| match arg {
            FnArg::Typed(pat_type) => (*pat_type.pat).clone(),
            FnArg::Receiver(_) => syn::parse_quote!(self),
        })
        .collect()
}

fn generate_unchecked_fn(func: &ItemFn, new_name: &syn::Ident, inner_ty: &Type) -> proc_macro2::TokenStream {
    let inputs = &func.sig.inputs;
    let call_args = extract_call_args(func);
    let vis = &func.vis;
    let orig_name = &func.sig.ident;

    quote! {
        #vis fn #new_name(#inputs) -> #inner_ty {
            Self::#orig_name(#(#call_args),*).unwrap()
        }
    }
}

#[proc_macro_attribute]
pub fn generate_unchecked(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let func = parse_macro_input!(item as ItemFn);

    let base_name_str = base_name(&func.sig.ident);
    let new_name = format_ident!("{}", base_name_str);

    let inner_ty = match &func.sig.output {
        ReturnType::Type(_, ty) => inner_type_of_checked_return(ty),
        ReturnType::Default => panic!("Function must return Option<T> or Result<T,E>"),
    };

    // ✅ Generate only the new function; leave original in-place
    let new_fn = generate_unchecked_fn(&func, &new_name, &inner_ty);

    // Return original function **unchanged**, plus new function
    quote! {
        #func
        #new_fn
    }
    .into()
}
