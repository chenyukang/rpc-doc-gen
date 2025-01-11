use clap::Parser;
use fiber_rpc_gen::utils;
use std::fmt::Display;
use std::path::Path;
use syn::visit::Visit;
use syn::Type;
use tera::{Tera, Value};
use walkdir::WalkDir;

#[derive(Debug, Clone, PartialEq, Eq)]
enum ItemType {
    Struct,
    Enum,
}

impl Display for ItemType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemType::Struct => write!(f, "Struct"),
            ItemType::Enum => write!(f, "Enum"),
        }
    }
}

type TypeDef = (ItemType, String, Vec<(String, String, String)>, String);

#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub name: String,
    //  (name, fields(name, type, desc), desc)
    pub types: Vec<TypeDef>,
    // (name, args, ret_ty, desc)
    pub rpc_fns: Vec<(String, Vec<String>, String, String)>,
    pub desc: String,
    pub is_rpc: bool,
}

impl Module {
    pub fn new(name: String, is_rpc: bool) -> Module {
        Module {
            name,
            types: vec![],
            rpc_fns: vec![],
            desc: "".to_string(),
            is_rpc,
        }
    }

    pub fn set_desc(&mut self, desc: String) {
        self.desc = desc;
    }

    pub fn add_struct_type(
        &mut self,
        ty: ItemType,
        name: String,
        fields: Vec<(String, String, String)>,
        desc: String,
    ) {
        self.types.push((ty, name, fields, desc));
    }

    pub fn add_rpc_fn(&mut self, name: String, args: Vec<String>, ret_ty: String, desc: String) {
        self.rpc_fns.push((name, args, ret_ty, desc));
    }

    pub fn gen_menu(&self) -> Value {
        let method_names = self
            .rpc_fns
            .iter()
            .map(|(name, _, _, _)| name.clone())
            .collect::<Vec<_>>();
        let capital_name = utils::capitalize(&self.name);
        utils::gen_value(&[
            ("title", capital_name.into()),
            ("name", self.name.to_lowercase().into()),
            ("methods", method_names.into()),
        ])
    }
}

#[derive(Debug)]
pub(crate) struct SynVisitor {
    current_module: Option<Module>,
    modules: Vec<Module>,
    refered_types: Vec<TypeDef>,
}

fn get_all_ident_type(type_path: &Type) -> Vec<String> {
    match type_path {
        syn::Type::Path(syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) => segments
            .iter()
            .map(|path_seg| match path_seg {
                syn::PathSegment {
                    ident,
                    arguments: syn::PathArguments::AngleBracketed(args),
                } => {
                    let args = args
                        .args
                        .iter()
                        .map(|arg| {
                            if let syn::GenericArgument::Type(inner_type) = arg {
                                get_all_ident_type(inner_type).join("::")
                            } else {
                                "".to_string()
                            }
                        })
                        .collect::<Vec<_>>();
                    format!("{}${}", ident, args.join("::").as_str())
                }
                _ => path_seg.ident.to_string(),
            })
            .collect::<Vec<_>>(),
        _ => vec![],
    }
}

impl Visit<'_> for SynVisitor {
    fn visit_item_enum(&mut self, i: &'_ syn::ItemEnum) {
        let enum_name = i.ident.to_string();
        let desc = utils::get_doc_from_attrs(&i.attrs);
        let mut fields = vec![];
        for v in &i.variants {
            let field_name = v.ident.to_string();
            let desc = utils::get_doc_from_attrs(&v.attrs);
            let field_type = match &v.fields {
                syn::Fields::Unnamed(fields) if fields.unnamed.len() > 0 => {
                    if let syn::Type::Path(syn::TypePath { path, .. }) =
                        fields.unnamed.first().unwrap().ty.clone()
                    {
                        utils::get_ident_from_path(&path)
                    } else {
                        "".to_string()
                    }
                }
                _ => "".to_string(),
            };
            fields.push((field_name, field_type, desc));
        }
        self.current_module.as_mut().unwrap().add_struct_type(
            ItemType::Enum,
            enum_name,
            fields,
            desc,
        );
    }

    fn visit_item_struct(&mut self, i: &syn::ItemStruct) {
        let ident_name = i.ident.to_string();
        let desc = utils::get_doc_from_attrs(&i.attrs);
        let mut fields = vec![];
        for field in &i.fields {
            let desc = utils::get_doc_from_attrs(&field.attrs);
            let field_name = field
                .ident
                .as_ref()
                .map(|i| i.to_string())
                .unwrap_or_default();
            let field_type = get_all_ident_type(&field.ty).join("::");
            fields.push((field_name, field_type, desc));
        }
        self.current_module.as_mut().unwrap().add_struct_type(
            ItemType::Struct,
            ident_name,
            fields,
            desc,
        );
    }

    fn visit_item_trait(&mut self, trait_item: &'_ syn::ItemTrait) {
        let desc = utils::get_doc_from_attrs(&trait_item.attrs);
        self.current_module.as_mut().unwrap().set_desc(desc);
        for i in trait_item.items.iter() {
            if let syn::TraitItem::Fn(item_fn) = i {
                let desc = utils::get_doc_from_attrs(&item_fn.attrs);
                let args: Vec<_> = item_fn
                    .sig
                    .inputs
                    .iter()
                    .filter_map(|arg| {
                        if let syn::FnArg::Typed(pat) = arg {
                            let path = match pat.ty.as_ref() {
                                Type::Path(syn::TypePath { path, .. }) => Some(path),
                                _ => None,
                            };
                            path.map(|path| utils::get_ident_from_path(path))
                        } else {
                            None
                        }
                    })
                    .collect();
                let ret_ty = utils::get_rpc_return_type(&item_fn.sig.output);
                self.current_module.as_mut().unwrap().add_rpc_fn(
                    item_fn.sig.ident.to_string(),
                    args,
                    ret_ty,
                    desc,
                );
            }
        }
    }
}

impl SynVisitor {
    fn visit_source_file(&mut self, file_path: &std::path::Path) {
        let code = std::fs::read_to_string(file_path).unwrap();
        if let Ok(file) = syn::parse_file(&code) {
            let module_name = file_path.file_stem().unwrap().to_string_lossy();
            if file_path.to_string_lossy().contains("/gen/") {
                return;
            }
            let is_rpc = file_path.to_string_lossy().contains("/rpc/");
            self.current_module = Some(Module::new(module_name.to_string(), is_rpc));
            self.visit_file(&file);
            self.modules.push(self.current_module.take().unwrap());
            self.current_module = None;
        }
    }

    pub fn new(dir: &Path) -> SynVisitor {
        let mut finder = SynVisitor {
            current_module: None,
            modules: vec![],
            refered_types: vec![],
        };

        for entry in WalkDir::new(dir).follow_links(true).into_iter() {
            match entry {
                Ok(ref e)
                    if !e.file_name().to_string_lossy().starts_with('.')
                        && e.file_name().to_string_lossy().ends_with(".rs") =>
                {
                    eprintln!("visit file: {:?}", e.path());
                    finder.visit_source_file(e.path());
                }
                _ => (),
            }
        }

        finder.modules.sort_by(|a, b| a.name.cmp(&b.name));
        finder
    }

    fn gen_module_menus(&self) -> Vec<Value> {
        self.get_modules().iter().map(|m| m.gen_menu()).collect()
    }

    fn get_modules(&self) -> Vec<Module> {
        self.modules
            .clone()
            .into_iter()
            .filter(|m| !m.rpc_fns.is_empty() && m.is_rpc)
            .collect()
    }

    fn get_non_rpc_modules(&self) -> Vec<Module> {
        self.modules
            .clone()
            .into_iter()
            .filter(|m| !m.is_rpc)
            .collect()
    }

    fn gen_type_menus(&self) -> Value {
        let mut type_list: Vec<_> = self
            .refered_types
            .iter()
            .map(|(_, name, ..)| name.clone())
            .collect();
        type_list.sort();
        type_list.dedup();
        type_list
            .into_iter()
            .map(|t| {
                utils::gen_value(&[
                    ("name", t.clone().into()),
                    ("link", t.to_lowercase().into()),
                ])
            })
            .collect()
    }

    fn render_generic_type(&self, ty: &str) -> String {
        if let Some((first_type, inner_type)) = ty.split_once('$') {
            format!("`{}<{}>`", first_type, self.render_generic_type(inner_type))
        } else {
            ty.to_string()
        }
    }

    fn gen_type_content(&self) -> Value {
        let mut result = vec![];
        let mut refered_types = self.refered_types.clone();
        refered_types.sort_by(|a, b| a.1.cmp(&b.1));
        refered_types.dedup_by(|a, b| a.0 == b.0 && a.1 == b.1);

        for (item_type, name, fields, desc) in refered_types.iter() {
            let fields: Vec<Value> = fields
                .iter()
                .filter(|(field_name, ..)| !field_name.is_empty())
                .map(|(field_name, field_type, desc)| {
                    let field_type = self.render_generic_type(&field_type);
                    utils::gen_value(&[
                        ("name", field_name.clone().into()),
                        ("type", field_type.clone().into()),
                        ("desc", desc.clone().into()),
                    ])
                })
                .collect();
            result.push(utils::gen_value(&[
                ("type", item_type.to_string().into()),
                ("name", name.clone().into()),
                ("link", name.to_lowercase().into()),
                ("desc", desc.clone().into()),
                ("fields", fields.into()),
            ]));
        }
        result.into()
    }

    fn gen_type_link(&mut self, ty: &str, module: &Module) -> Option<String> {
        if let Some(ty) = self.find_type(ty, module) {
            let res = Some(format!("#type-{}", ty.1.to_lowercase()));
            return res;
        }
        None
    }

    fn find_type(&mut self, ty: &str, module: &Module) -> Option<TypeDef> {
        if let Some(type_def) = module
            .types
            .iter()
            .find(|(_, name, ..)| name == ty)
            .cloned()
            .or_else(|| {
                let res = self
                    .get_non_rpc_modules()
                    .iter()
                    .flat_map(|m| m.types.iter())
                    .find(|(_, name, ..)| name == ty)
                    .cloned();
                res
            })
        {
            self.refered_types.push(type_def.clone());
            return Some(type_def);
        } else {
            return None;
        }
    }

    fn render_type_with_link(&mut self, type_path: &str, module: &Module) -> String {
        if let Some((first_type, inner_type)) = type_path.split_once('$') {
            format!(
                "{}<{}>",
                first_type,
                self.render_type_with_link(inner_type, module)
            )
        } else {
            if let Some(ty_link) = self.gen_type_link(type_path, module) {
                format!("[{}]({})", type_path, ty_link)
            } else {
                type_path.to_string()
            }
        }
    }

    fn convert_types(&mut self, types: &[String], module: &Module) -> Vec<Value> {
        let mut result = vec![];
        types.iter().for_each(|arg| {
            self.find_type(arg, module)
                .map(|(_, _, fields, _)| {
                    let fields: Vec<Value> = fields
                        .iter()
                        .map(|(field_name, field_type, desc)| {
                            let field_type = self.render_type_with_link(field_type, module);
                            let render_ty = if field_type.contains("#type") {
                                field_type
                            } else {
                                format!("`{}`", field_type)
                            };
                            utils::gen_value(&[
                                ("name", field_name.clone().into()),
                                ("type", render_ty.clone().into()),
                                ("desc", desc.clone().into()),
                            ])
                        })
                        .collect::<Vec<_>>();
                    for f in fields {
                        result.push(f);
                    }
                })
                .unwrap_or_else(|| {
                    result.push(utils::gen_value(&[
                        ("name", arg.clone().into()),
                        ("type", arg.clone().into()),
                        ("desc", "".to_string().into()),
                    ]));
                })
        });
        // filter the empty type
        result
            .into_iter()
            .filter(|v| {
                if let Some(v) = v.get("name") {
                    !v.as_str().unwrap().trim().is_empty()
                } else {
                    false
                }
            })
            .collect()
    }

    fn gen_single_module_content(&mut self, module: &Module) -> Value {
        let name = utils::capitalize(&module.name);
        let link = module.name.to_lowercase();
        let methods: Value = module
            .rpc_fns
            .iter()
            .map(|(fn_name, args, ret_ty, desc)| {
                let params = self.convert_types(args, module);
                let return_type = self.convert_types(&[ret_ty.clone()], module);
                self.refered_types
                    .retain(|(_, name, ..)| name != ret_ty && !args.contains(name));
                let link = format!("{}-{}", name.to_lowercase(), fn_name);
                render_tera(
                    include_str!("../templates/method.tera"),
                    &[
                        ("link", link.clone().into()),
                        ("desc", desc.clone().into()),
                        ("name", fn_name.clone().into()),
                        ("params", params.into()),
                        ("return_type", return_type.into()),
                    ],
                )
            })
            .collect::<Vec<_>>()
            .into();
        render_tera(
            include_str!("../templates/module.tera"),
            &[
                ("name", name.clone().into()),
                ("link", link.into()),
                ("desc", module.desc.clone().into()),
                ("methods", methods),
            ],
        )
        .into()
    }

    fn gen_module_content(&mut self) -> Value {
        let content = self
            .get_modules()
            .into_iter()
            .map(|m| self.gen_single_module_content(&m))
            .collect::<Vec<_>>();
        content.into()
    }

    pub fn gen_markdown(&mut self, output_path: &str) {
        let module_menus: Vec<Value> = self.gen_module_menus();
        let modules: Value = self.gen_module_content();
        let type_menus: Value = self.gen_type_menus();
        let types: Value = self.gen_type_content();
        let output = render_tera(
            include_str!("../templates/readme.tera"),
            &[
                ("module_menus", module_menus.into()),
                ("type_menus", type_menus.into()),
                ("modules", modules.into()),
                ("types", types.into()),
            ],
        );
        std::fs::write(output_path, output).unwrap();
    }
}

fn render_tera(template: &str, content: &[(&str, Value)]) -> String {
    let mut context = tera::Context::new();
    for (k, v) in content {
        context.insert(*k, v);
    }
    let mut tera = Tera::default();
    tera.add_raw_template("template", template).unwrap();
    tera.render("template", &context).unwrap()
}

#[derive(Parser)]
#[command(name = "fiber-rpc-gen")]
#[command(about = "Generates markdown documentation for RPCs in fiber", long_about = None)]
struct Args {
    /// RPC directory
    source_code_dir: String,
    /// Output file path
    #[clap(short, long)]
    output: Option<String>,
}

fn main() {
    let args = Args::parse();
    let source_code_dir = args.source_code_dir;
    let mut finder = SynVisitor::new(Path::new(&source_code_dir));
    let output_path = args
        .output
        .unwrap_or(format!("{}/rpc/README.md", source_code_dir));
    finder.gen_markdown(&output_path);
}
