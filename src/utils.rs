use syn::{Expr, LitStr, Meta, MetaNameValue, ReturnType, Type};
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

fn split_words(s: &str) -> Vec<String> {
    let chars: Vec<char> = s.chars().collect();
    let mut words = Vec::new();
    let mut current = String::new();

    for (index, ch) in chars.iter().copied().enumerate() {
        if ch == '_' || ch == '-' {
            if !current.is_empty() {
                words.push(current.to_ascii_lowercase());
                current.clear();
            }
            continue;
        }

        let prev = index.checked_sub(1).and_then(|i| chars.get(i)).copied();
        let next = chars.get(index + 1).copied();
        let has_boundary = ch.is_uppercase()
            && !current.is_empty()
            && matches!(prev, Some(prev) if prev.is_lowercase())
            || ch.is_uppercase()
                && !current.is_empty()
                && matches!((prev, next), (Some(prev), Some(next)) if prev.is_uppercase() && next.is_lowercase());

        if has_boundary {
            words.push(current.to_ascii_lowercase());
            current.clear();
        }

        current.push(ch);
    }

    if !current.is_empty() {
        words.push(current.to_ascii_lowercase());
    }

    words
}

fn to_pascal_case(s: &str) -> String {
    split_words(s)
        .into_iter()
        .map(|word| capitalize(&word))
        .collect()
}

fn to_camel_case(s: &str) -> String {
    let mut words = split_words(s).into_iter();
    let Some(first) = words.next() else {
        return String::new();
    };

    let mut result = first;
    for word in words {
        result.push_str(&capitalize(&word));
    }
    result
}

fn to_snake_case(s: &str) -> String {
    split_words(s).join("_")
}

fn to_kebab_case(s: &str) -> String {
    split_words(s).join("-")
}

fn apply_rename_rule(name: &str, rule: &str) -> String {
    match rule {
        "lowercase" => split_words(name).join(""),
        "UPPERCASE" => split_words(name).join("").to_ascii_uppercase(),
        "PascalCase" => to_pascal_case(name),
        "camelCase" => to_camel_case(name),
        "snake_case" => to_snake_case(name),
        "SCREAMING_SNAKE_CASE" => to_snake_case(name).to_ascii_uppercase(),
        "kebab-case" => to_kebab_case(name),
        "SCREAMING-KEBAB-CASE" => to_kebab_case(name).to_ascii_uppercase(),
        _ => name.to_string(),
    }
}

fn get_serde_attr_value(attrs: &[syn::Attribute], key: &str) -> Option<String> {
    attrs.iter().find_map(|attr| {
        if !attr.path().is_ident("serde") {
            return None;
        }

        let mut value = None;
        let _ = attr.parse_nested_meta(|meta| {
            if meta.path.is_ident(key) {
                let lit: LitStr = meta.value()?.parse()?;
                value = Some(lit.value());
            }
            Ok(())
        });
        value
    })
}

pub fn get_enum_variant_name(
    enum_attrs: &[syn::Attribute],
    variant_attrs: &[syn::Attribute],
    default_name: &str,
) -> String {
    get_serde_attr_value(variant_attrs, "rename")
        .or_else(|| {
            get_serde_attr_value(enum_attrs, "rename_all")
                .map(|rule| apply_rename_rule(default_name, &rule))
        })
        .unwrap_or_else(|| default_name.to_string())
}

#[cfg(test)]
mod tests {
    use super::{apply_rename_rule, get_enum_variant_name};
    use syn::parse_quote;

    #[test]
    fn serde_rename_all_is_applied_to_enum_variants() {
        let enum_attrs = vec![parse_quote!(#[serde(rename_all = "snake_case")])];
        let variant_attrs = Vec::new();

        assert_eq!(
            get_enum_variant_name(&enum_attrs, &variant_attrs, "CkbHash"),
            "ckb_hash"
        );
        assert_eq!(
            get_enum_variant_name(&enum_attrs, &variant_attrs, "Sha256"),
            "sha256"
        );
    }

    #[test]
    fn serde_variant_rename_overrides_rename_all() {
        let enum_attrs = vec![parse_quote!(#[serde(rename_all = "snake_case")])];
        let variant_attrs = vec![parse_quote!(#[serde(rename = "custom_value")])];

        assert_eq!(
            get_enum_variant_name(&enum_attrs, &variant_attrs, "CkbHash"),
            "custom_value"
        );
    }

    #[test]
    fn rename_rules_handle_acronyms() {
        assert_eq!(
            apply_rename_rule("HTTPRequest", "snake_case"),
            "http_request"
        );
        assert_eq!(apply_rename_rule("HTTPRequest", "camelCase"), "httpRequest");
        assert_eq!(
            apply_rename_rule("HTTPRequest", "kebab-case"),
            "http-request"
        );
    }
}
