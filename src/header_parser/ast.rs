
#[derive(Debug)]
pub struct File {
    pub top_levels: Vec<TopLevel>
}

#[derive(Debug)]
pub enum TopLevel {
    Struct(Struct),
    TemplateStruct(TemplateStruct),
    TypeDef(TypeDef),
    Enum(Enum),
    Other
}

#[derive(Debug)]
pub struct Enum {
    pub name: String,
    pub variants: Vec<String>
}

#[derive(Debug)]
pub struct TypeDef {
    pub actual: Type,
    pub new: String
}

#[derive(Debug)]
pub struct TemplateStruct {
    pub parameters: Vec<TemplateParameter>,
    pub struct_: Struct
}

#[derive(Debug, Clone)]
pub enum TemplateParameter {
    TypeName(String),
    Const(String)
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub items: Vec<StructItem>
}

#[derive(Debug, Clone)]
pub enum StructItem {
    Field { decl: Decl },
    Other
}

#[derive(Debug, Clone)]
pub struct Decl {
    pub typ: Type,
    pub name: String,
    pub arrays: Vec<ConstExpr>
}

#[derive(Debug, Clone)]
pub enum Type {
    Name(String),
    Template(String, Vec<TemplateArgument>)
}

#[derive(Debug, Clone)]
pub enum TemplateArgument {
    Type(Type),
    Const(ConstExpr)
}

#[derive(Debug, Clone)]
pub enum ConstExpr {
    Name(String),
    Integer(usize),
    Float(f64),
}