//! It's bad(sad) JSON Schema currently ignore type alias,
//! maybe it's better to fix it in schemars, but here we only do a quick hack
//! here we use a simple syn visitor to find extra type comments

use rpc_doc_gen::utils;
use std::collections::HashMap;
use std::path::Path;
use syn::visit::Visit;
use syn::{parse2, ItemType, Type};
use tera::{Tera, Value};
use walkdir::WalkDir;

#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub name: String,
    pub types: Vec<(String, Vec<(String, String, String)>)>,
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

    pub fn add_struct_type(&mut self, name: String, fields: Vec<(String, String, String)>) {
        self.types.push((name, fields));
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
                .find(|(name, _)| name == arg)
                .map(|(_name, fields)| {
                    let fields: Vec<Value> = fields
                        .iter()
                        .map(|(field_name, field_type, desc)| {
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

                render_tera(
                    include_str!("../templates/method.tera"),
                    &[
                        ("link", fn_name.clone().into()),
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
    pub type_comments: HashMap<String, String>,
    current_type: Option<String>,
    current_module: Option<Module>,
    modules: Vec<Module>,
}

impl Visit<'_> for SynVisitor {
    fn visit_attribute(&mut self, attr: &syn::Attribute) {
        if let Some(type_name) = &self.current_type {
            let doc = utils::get_doc_from_attr(attr);
            let current_type = type_name.clone();
            *self
                .type_comments
                .entry(current_type)
                .or_insert("".to_string()) += &format!("\n{}", doc.trim_start());
        }
    }

    fn visit_item_struct(&mut self, i: &syn::ItemStruct) {
        let ident_name = i.ident.to_string();
        if !i.attrs.is_empty() {
            self.current_type = Some(ident_name.clone());

            for attr in &i.attrs {
                self.visit_attribute(attr);
            }
            let mut fields = vec![];
            for field in &i.fields {
                let desc = utils::get_doc_from_attrs(&field.attrs);
                let filed_type = field.ty.clone();
                let field_name = field
                    .ident
                    .as_ref()
                    .map(|i| i.to_string())
                    .unwrap_or_default();
                let type_token = match filed_type {
                    syn::Type::Path(syn::TypePath { path, .. }) => path
                        .segments
                        .iter()
                        .map(|s| s.ident.to_string())
                        .collect::<Vec<_>>(),
                    _ => vec![],
                }
                .join("::");
                fields.push((field_name, type_token, desc));
            }
            self.current_module
                .as_mut()
                .unwrap()
                .add_struct_type(ident_name, fields);
            self.current_type = None;
        }
    }

    fn visit_item_type(&mut self, i: &ItemType) {
        let ident_name = i.ident.to_string();
        if !i.attrs.is_empty() {
            self.current_type = Some(ident_name);
            for attr in &i.attrs {
                self.visit_attribute(attr);
            }
            self.current_type = None;
        }
    }

    fn visit_item_trait(&mut self, trait_item: &'_ syn::ItemTrait) {
        let desc = utils::get_doc_from_attrs(&trait_item.attrs);
        eprintln!("module desc: {:?}", desc);
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

                for attr in &item_fn.attrs {
                    self.visit_attribute(attr);
                }
            }
        }
    }

    fn visit_item_enum(&mut self, i: &'_ syn::ItemEnum) {
        let ident_name = i.ident.to_string();

        self.current_type = Some(ident_name);
        for attr in &i.attrs {
            self.visit_attribute(attr);
        }

        let mut variants = vec![];
        for v in &i.variants {
            if !v.attrs.is_empty() {
                let doc: Vec<String> = v.attrs.iter().map(utils::get_doc_from_attr).collect();
                let doc = doc.join("\n");
                variants.push(format!("  - `{}` : {}", v.ident, doc));
            }
        }
        let extra_doc = variants.join("\n");
        *self
            .type_comments
            .entry(i.ident.to_string())
            .or_insert("".to_string()) += &format!("An enum value from one of:\n{}", extra_doc);
        self.current_type = None;
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
            type_comments: Default::default(),
            current_type: None,
            current_module: None,
            modules: vec![],
        };

        for entry in WalkDir::new(dir).follow_links(true).into_iter() {
            match entry {
                Ok(ref e)
                    if !e.file_name().to_string_lossy().starts_with('.')
                        && e.file_name().to_string_lossy().ends_with(".rs") =>
                {
                    finder.visit_source_file(e.path());
                }
                _ => (),
            }
        }

        finder
    }

    fn gen_module_menus(&self) -> Vec<Value> {
        self.modules.iter().map(|m| m.gen_menu()).collect()
    }

    fn get_modules(&self) -> Vec<Module> {
        self.modules
            .clone()
            .into_iter()
            .filter(|m| !m.rpc_fns.is_empty())
            .collect()
    }

    fn gen_type_menus(&self) -> Value {
        let all_types = self
            .get_modules()
            .into_iter()
            .flat_map(|m| m.types.into_iter());
        all_types
            .map(|(name, _)| vec![name.clone().into(), name.to_lowercase().into()])
            .collect::<Vec<Vec<Value>>>()
            .into()
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
        let types: Vec<Value> = vec![];
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

fn main() {
    let arg = std::env::args().nth(1);
    let finder = SynVisitor::new(Path::new(&arg.unwrap()));
    //eprintln!("finder: {:#?}", finder);
    finder.gen_markdown("./README.md");
    //finder.display();
}
