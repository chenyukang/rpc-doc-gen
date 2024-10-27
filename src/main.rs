use clap::Parser;
use fiber_rpc_gen::utils;
use std::fmt::Display;
use std::path::Path;
use syn::visit::Visit;
use syn::{parse2, Type};
use tera::{Tera, Value};
use walkdir::WalkDir;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub name: String,
    //  (name, fields(name, type, desc), desc)
    pub types: Vec<(ItemType, String, Vec<(String, String, String)>, String)>,
    // (name, args, ret_ty, desc)
    pub rpc_fns: Vec<(String, Vec<String>, String, String)>,
    pub desc: String,
}

impl Module {
    pub fn new(name: String) -> Module {
        Module {
            name,
            types: vec![],
            rpc_fns: vec![],
            desc: "".to_string(),
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

    fn convert_types(&self, types: &[String]) -> Vec<Value> {
        let mut result = vec![];
        types.iter().for_each(|arg| {
            self.types
                .iter()
                .find(|(_, name, ..)| name == arg)
                .map(|(_, _, fields, _)| {
                    let fields: Vec<Value> = fields
                        .iter()
                        .map(|(field_name, field_type, desc)| {
                            let field_type = if let Some((first_type, field_type)) =
                                field_type.split_once('$')
                            {
                                format!("`{}<{}>`", first_type, field_type)
                            } else {
                                field_type.clone()
                            };
                            utils::gen_value(&[
                                ("name", field_name.clone().into()),
                                ("type", field_type.clone().into()),
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

    pub fn gen_module_content(&self) -> Value {
        let name = utils::capitalize(&self.name);
        let link = self.name.to_lowercase();
        let methods: Value = self
            .rpc_fns
            .iter()
            .map(|(fn_name, args, ret_ty, desc)| {
                let params = self.convert_types(args);
                let return_type = self.convert_types(&[ret_ty.clone()]);
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
                ("desc", self.desc.clone().into()),
                ("methods", methods),
            ],
        )
        .into()
    }
}

#[derive(Debug)]
pub(crate) struct SynVisitor {
    current_module: Option<Module>,
    modules: Vec<Module>,
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
                syn::Fields::Unnamed(fields) => {
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
            let field_type = match field.ty.clone() {
                syn::Type::Path(syn::TypePath {
                    path: syn::Path { segments, .. },
                    ..
                }) => segments
                    .iter()
                    .map(|s| match s {
                        syn::PathSegment {
                            ident,
                            arguments: syn::PathArguments::AngleBracketed(args),
                        } => {
                            let args = args
                                .args
                                .iter()
                                .map(|arg| {
                                    if let syn::GenericArgument::Type(syn::Type::Path(
                                        syn::TypePath { path, .. },
                                    )) = arg
                                    {
                                        utils::get_ident_from_path(path)
                                    } else {
                                        "".to_string()
                                    }
                                })
                                .collect::<Vec<_>>();
                            format!("{}${}", ident, args.join("::").as_str())
                        }
                        _ => s.ident.to_string(),
                    })
                    .collect::<Vec<_>>(),
                _ => vec![],
            }
            .join("::");
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
        if let Ok(tokens) = code.parse() {
            if let Ok(file) = parse2(tokens) {
                let module_name = file_path.file_stem().unwrap().to_string_lossy();
                self.current_module = Some(Module::new(module_name.to_string()));
                self.visit_file(&file);
                self.modules.push(self.current_module.take().unwrap());
                self.current_module = None;
            }
        }
    }

    pub fn new(dir: &Path) -> SynVisitor {
        let mut finder = SynVisitor {
            current_module: None,
            modules: vec![],
        };

        for entry in WalkDir::new(dir).follow_links(true).into_iter() {
            match entry {
                Ok(ref e)
                    if !e.file_name().to_string_lossy().starts_with('.')
                        && e.file_name().to_string_lossy().ends_with(".rs") =>
                {
                    eprintln!("analysising file: {}", e.path().display());
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
            .filter(|m| !m.rpc_fns.is_empty())
            .collect()
    }

    fn get_should_list_types(&self) -> Vec<String> {
        let mut should_list_types = vec![];
        let all_types = self
            .get_modules()
            .into_iter()
            .flat_map(|m| m.types.into_iter())
            .collect::<Vec<_>>();
        for m in &self.modules {
            for (_, _, ty, ..) in &m.types {
                for (_, t, ..) in ty.iter() {
                    if let Some((_first_type, generic_type)) = t.split_once('$') {
                        should_list_types.push(generic_type.to_string());
                    } else {
                        should_list_types.push(t.clone());
                    }
                }
            }
        }
        should_list_types = should_list_types
            .into_iter()
            .filter(|t| all_types.iter().find(|(_, name, ..)| name == t).is_some())
            .collect();
        should_list_types.sort();
        should_list_types.dedup();
        should_list_types
    }

    fn gen_type_menus(&self) -> Value {
        let should_list_types = self.get_should_list_types();
        should_list_types
            .into_iter()
            .map(|t| {
                utils::gen_value(&[
                    ("name", t.clone().into()),
                    ("link", t.to_lowercase().into()),
                ])
            })
            .collect()
    }

    fn gen_type_content(&self) -> Value {
        let should_list_types = self.get_should_list_types();
        let all_types = self
            .get_modules()
            .into_iter()
            .flat_map(|m| m.types.into_iter())
            .collect::<Vec<_>>();
        let gen_types = all_types
            .into_iter()
            .filter(|(_, name, ..)| should_list_types.iter().find(|t| *t == name).is_some())
            .collect::<Vec<_>>();

        let mut result = vec![];
        for (item_type, name, fields, desc) in gen_types {
            let fields: Vec<Value> = fields
                .iter()
                .map(|(field_name, field_type, desc)| {
                    let field_type =
                        if let Some((first_type, field_type)) = field_type.split_once('$') {
                            format!("`{}<{}>`", first_type, field_type)
                        } else {
                            field_type.clone()
                        };
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

    fn gen_module_content(&self) -> Value {
        let content = self
            .get_modules()
            .into_iter()
            .map(|m| m.gen_module_content())
            .collect::<Vec<_>>();
        content.into()
    }

    pub fn gen_markdown(&self, output_path: &str) -> String {
        let module_menus: Vec<Value> = self.gen_module_menus();
        let type_menus: Value = self.gen_type_menus();
        let modules: Value = self.gen_module_content();
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
        std::fs::write(output_path, output.clone()).unwrap();
        output
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
    rpc_dir: String,
    /// Output file path
    #[clap(short, long)]
    output: Option<String>,
}

fn main() {
    let args = Args::parse();
    let rpc_dir = args.rpc_dir;
    let finder = SynVisitor::new(Path::new(&rpc_dir));
    let output_path = args.output.unwrap_or(format!("{}/README.md", rpc_dir));
    finder.gen_markdown(&output_path);
}
