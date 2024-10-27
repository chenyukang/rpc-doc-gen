use syn::{Expr, Meta, MetaNameValue, ReturnType, Type};
use tera::{Map, Value};

pub fn get_doc_from_attr(attr: &syn::Attribute) -> String {
    if attr.path().is_ident("doc") {
        if let Meta::NameValue(MetaNameValue {
            value:
                Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(lit),
                    ..
                }),
            ..
        }) = &attr.meta
        {
            let lit = lit.value();
            return lit;
        }
    }
    "".to_string()
}

pub fn get_doc_from_attrs(attrs: &[syn::Attribute]) -> String {
    let mut res = "".to_string();
    for attr in attrs.iter() {
        let doc = get_doc_from_attr(attr);
        res += &format!("\n{}", doc);
    }
    let res = res.trim().to_string();
    if res.is_empty() {
        "TODO: add desc".to_string()
    } else {
        res
    }
}

pub fn get_ident_from_path(path: &syn::Path) -> String {
    path.segments
        .iter()
        .map(|s| s.ident.to_string())
        .collect::<Vec<_>>()
        .join("::")
}

pub fn get_ident_from_type_path(path: &syn::TypePath) -> String {
    get_ident_from_path(&path.path)
}

pub fn get_rpc_return_type(ty: &ReturnType) -> String {
    let ty = match ty {
        ReturnType::Type(_, ty) => ty,
        _ => return "".to_string(),
    };
    match &**ty {
        Type::Path(syn::TypePath { path, .. }) => match path {
            syn::Path { segments, .. } => {
                let first_segment = segments.first().unwrap();
                match first_segment {
                    syn::PathSegment {
                        ident, arguments, ..
                    } => {
                        if let syn::PathArguments::AngleBracketed(args) = arguments {
                            let ty = args.args.first().unwrap();
                            if let syn::GenericArgument::Type(ty) = ty {
                                match ty {
                                    Type::Path(syn::TypePath {
                                        path: syn::Path { segments, .. },
                                        ..
                                    }) => {
                                        let first_segment = segments.first().unwrap();
                                        return format!("{}", first_segment.ident.to_string());
                                    }
                                    _ => return "".to_string(),
                                }
                            }
                        }
                        ident.to_string()
                    }
                }
            }
        },
        _ => "".to_string(),
    }
}

/// wrapper for render value
pub fn gen_value(pairs: &[(&str, Value)]) -> Value {
    let mut res = Value::Object(Map::new());
    for (k, v) in pairs {
        res.as_object_mut()
            .unwrap()
            .insert(k.to_string(), v.to_owned());
    }
    res
}

pub fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}
