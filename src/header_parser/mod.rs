#![allow(dead_code)]

mod ast;
mod parser;

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use regex::Regex;
use crate::table::{DataType, Schema, Table, Value};
use crate::task::Tracker;

pub fn parse_and_load(packet: &str, source: &[u8], tracker: &Tracker) -> Result<Table, String> {
    let Ok(ctxt) = parse_into_context(packet) else {
        return Err("The packet format could not be parsed.".into());
    };

    let Some(packet_type) = ctxt.types.get("sensorDataStruct_t") else {
        return Err("The packet format is missing a struct called 'sensorDataStruct_t'.".into());
    };

    Parser::parse_into_table(packet_type, source, tracker)
}

#[derive(Debug)]
enum ParseError {
    FloatAsConstant,
    DuplicatedName(String),
    MissingName(String),
    MismatchedTemplate
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            ParseError::FloatAsConstant => "Using floats as constants isn't supported".into(),
            ParseError::DuplicatedName(name) => format!("A name ('{name}') was repeated"),
            ParseError::MissingName(name) => format!("A name ('{name}') was missing"),
            ParseError::MismatchedTemplate => "Template arguments were not correct".into()
        })
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
enum Bits {
    I8,
    I16,
    I32,
    I64
}

struct Parser<'a> {
    pieces: Vec<ParserPiece<'a>>
}

impl<'a> Parser<'a> {
    fn parse_into_table(typ: &Type, mut data: &[u8], tracker: &Tracker) -> Result<Table, String> {
        let total_length = data.len();

        let schema = typ.create_schema()
            .ok_or_else(|| String::from("The packet format is missing a struct called 'sensorDataStruct_t'."))?;
        let mut table = Table::from_schema(schema);

        let mut parser = Parser { pieces: vec![ ] };
        typ.add_to_parser(&mut parser);

        let row_len = table.schema().columns().len();
        while data.len() > 0 {
            let mut row = Vec::with_capacity(row_len);

            for piece in &parser.pieces {
                let (value, new_data) = piece.parse(data)
                    .ok_or_else(|| String::from("The source data could not be parsed correctly by the packet format."))?;
                data = new_data;
                if let Some(val) = value {
                    row.push(val);
                }
            }
            table.add_row(row);

            let progress = ((total_length - data.len()) as f32) / (total_length as f32);
            tracker.set_progress(progress);
        }

        Ok(table)
    }

    fn parse_into_table_no_tracker(typ: &Type, mut data: &[u8]) -> Result<Table, String> {
        let schema = typ.create_schema()
            .ok_or_else(|| String::from("The packet format is missing a struct called 'sensorDataStruct_t'."))?;
        let mut table = Table::from_schema(schema);

        let mut parser = Parser { pieces: vec![ ] };
        typ.add_to_parser(&mut parser);

        let row_len = table.schema().columns().len();
        while data.len() > 0 {
            let mut row = Vec::with_capacity(row_len);

            for piece in &parser.pieces {
                // println!("{:?}, {:?}", piece, data.len());
                let (value, new_data) = piece.parse(data)
                    .ok_or_else(|| String::from("The source data could not be parsed correctly by the packet format."))?;
                data = new_data;
                if let Some(val) = value {
                    row.push(val);
                }
            }
            table.add_row(row);
        }

        Ok(table)
    }
}

#[derive(Debug)]
enum ParserPiece<'a> {
    Bool,
    U32,
    F32,
    Enum(&'a HashMap<usize, String>),
    Padding(u8)
}

fn get_bytes<const N: usize>(bytes: &[u8]) -> Option<([u8; N], &[u8])> {
    if bytes.len() >= N {
        let (used, rest) = bytes.split_at(N);
        let used: [u8; N] = used.try_into().ok()?;
        Some((used, rest))
    } else {
        None
    }
}

impl<'a> ParserPiece<'a> {
    fn parse<'b>(&self, bytes: &'b [u8]) -> Option<(Option<Value>, &'b [u8])> {
        match self {
            ParserPiece::Bool => {
                let (byte, rest) = get_bytes::<1>(bytes)?;
                let value = match byte[0] {
                    0 => Value::Bool(false),
                    1 => Value::Bool(true),
                    _ => return None,
                };
                Some((Some(value), rest))
            }
            ParserPiece::U32 => {
                let (bytes, rest) = get_bytes::<4>(bytes)?;
                let value = Value::U32(u32::from_le_bytes(bytes));
                Some((Some(value), rest))
            }
            ParserPiece::F32 => {
                let (bytes, rest) = get_bytes::<4>(bytes)?;
                let value = Value::F32(f32::from_le_bytes(bytes));
                Some((Some(value), rest))
            }
            ParserPiece::Enum(map) => {
                let (bytes, rest) = get_bytes::<4>(bytes)?;
                let idx = u32::from_le_bytes(bytes) as usize;
                let value = Value::String(map[&idx].clone());
                Some((Some(value), rest))
            }
            ParserPiece::Padding(n) => {
                if bytes.len() >= *n as usize {
                    let (_, rest) = bytes.split_at(*n as usize);
                    Some((None, rest))
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Clone)]
enum Type {
    Boolean,
    SignedInteger(Bits),
    UnsignedInteger(Bits),
    Float,
    Double,
    Array(Box<Type>, usize),
    Struct(Vec<(String, Type)>),
    Enum(HashMap<usize, String>)
}

fn align_to(n: usize, align: usize) -> usize {
    if n % align != 0 {
        align - (n % align)
    } else {
        0
    }
}

impl Type {
    fn size_of(&self) -> usize {
        match self {
            Type::Boolean => 1,
            Type::UnsignedInteger(Bits::I32) => 4,
            Type::UnsignedInteger(_) => todo!(),
            Type::SignedInteger(_) => todo!(),
            Type::Float => 4,
            Type::Double => todo!(),
            Type::Enum(_) => 4,
            Type::Array(typ, count) => typ.size_of() * count,
            Type::Struct(fields) => {
                let mut offset = 0;
                let mut largest_alignment = 1;
                for (_, field_type) in fields.iter() {
                    offset += align_to(offset, field_type.align_of());

                    offset += field_type.size_of();
                    if field_type.align_of() > largest_alignment {
                        largest_alignment = field_type.align_of();
                    }
                }

                offset += align_to(offset, largest_alignment);

                offset
            }
        }
    }

    fn align_of(&self) -> usize {
        match self {
            Type::Boolean => 1,
            Type::UnsignedInteger(Bits::I32) => 4,
            Type::UnsignedInteger(_) => todo!(),
            Type::SignedInteger(_) => todo!(),
            Type::Float => 4,
            Type::Double => todo!(),
            Type::Enum(_) => 4,
            Type::Array(typ, _) => typ.align_of(),
            Type::Struct(fields) => {
                let mut largest_alignment = 1;
                for (_, field_type) in fields.iter() {
                    if field_type.align_of() > largest_alignment {
                        largest_alignment = field_type.align_of();
                    }
                }

                largest_alignment
            }
        }
    }

    fn create_schema(&self) -> Option<Schema> {
        let Type::Struct(fields) = self else { return None; };

        let mut schema = Schema::new();
        fields.iter().for_each(|(field_name, field_type)|
            field_type.add_to_schema(field_name, &mut schema)
        );
        Some(schema)
    }

    fn add_to_schema(&self, name: &str, schema: &mut Schema) {
        match self {
            Type::Boolean => schema.with_column(name.into(), DataType::Bool),
            Type::UnsignedInteger(Bits::I32) => schema.with_column(name.into(), DataType::U32),
            Type::UnsignedInteger(_) => todo!(),
            Type::SignedInteger(_) => todo!(),
            Type::Float => schema.with_column(name.into(), DataType::F32),
            Type::Double => todo!(),
            Type::Enum(_) => schema.with_column(name.into(), DataType::String),
            Type::Array(typ, count) => {
                for i in 0..*count {
                    typ.add_to_schema(&format!("{}.{}", name, i), schema);
                };
            }
            Type::Struct(fields) => {
                for (field_name, field_type) in fields.iter() {
                    field_type.add_to_schema(&format!("{}.{}", name, field_name), schema);
                };
            }
        };
    }

    fn add_to_parser<'a>(&'a self, parser: &mut Parser<'a>) {
        match self {
            Type::Boolean => parser.pieces.push(ParserPiece::Bool),
            Type::UnsignedInteger(Bits::I32) => parser.pieces.push(ParserPiece::U32),
            Type::UnsignedInteger(_) => todo!(),
            Type::SignedInteger(_) => todo!(),
            Type::Float => parser.pieces.push(ParserPiece::F32),
            Type::Double => todo!(),
            Type::Enum(map) => parser.pieces.push(ParserPiece::Enum(map)),
            Type::Array(typ, count) => {
                for _ in 0..*count {
                    typ.add_to_parser(parser);
                }
            }
            Type::Struct(fields) => {
                let mut offset = 0;
                let mut largest_alignment = 1;
                for (_, field_type) in fields.iter() {
                    let padding = align_to(offset, field_type.align_of());
                    offset += padding;
                    if padding > 0 {
                        parser.pieces.push(ParserPiece::Padding(padding as u8));
                    }

                    let size = field_type.size_of();
                    offset += size;
                    field_type.add_to_parser(parser);

                    if field_type.align_of() > largest_alignment {
                        largest_alignment = field_type.align_of();
                    }
                }

                let padding = align_to(offset, largest_alignment);
                if padding > 0 {
                    parser.pieces.push(ParserPiece::Padding(padding as u8));
                }
            }
        };
    }
}

#[derive(Clone)]
struct Context {
    values: HashMap<String, usize>,
    types: HashMap<String, Type>,
    templates: HashMap<String, Rc<Template>>
}

impl Context {
    fn new() -> Self {
        Context {
            values: HashMap::new(),
            types: HashMap::new(),
            templates: HashMap::new()
        }
    }

    fn add_type(&mut self, name: impl Into<String>, ty: Type) -> Result<(), ParseError> {
        let name = name.into();
        match self.types.insert(name.clone(), ty) {
            Some(_) => Err(ParseError::DuplicatedName(name)),
            None => Ok(())
        }
    }

    fn get_type(&self, name: &str) -> Result<Type, ParseError> {
        self.types.get(name).map(|ty| ty.clone()).ok_or(ParseError::MissingName(name.into()))
    }

    fn add_name(&mut self, name: impl Into<String>, value: usize) -> Result<(), ParseError> {
        let name = name.into();
        match self.values.insert(name.clone(), value) {
            Some(_) => Err(ParseError::DuplicatedName(name)),
            None => Ok(())
        }
    }

    fn get_name(&self, name: &str) -> Result<usize, ParseError> {
        self.values.get(name).map(|ty| ty.clone()).ok_or(ParseError::MissingName(name.into()))
    }

    fn add_template(&mut self, name: impl Into<String>, template: Template) -> Result<(), ParseError> {
        let name = name.into();
        match self.templates.insert(name.clone(), Rc::new(template)) {
            Some(_) => Err(ParseError::DuplicatedName(name)),
            None => Ok(())
        }
    }

    fn get_template(&self, name: &str) -> Result<Rc<Template>, ParseError> {
        self.templates.get(name).map(|t| Rc::clone(t)).ok_or(ParseError::MissingName(name.into()))
    }
}

struct Template {
    params: Vec<(String, ast::TemplateParameter)>,

    struct_: ast::Struct,

    context: Context,
}

impl Template {
    fn resolve(&self, arguments: &[ast::TemplateArgument], ctxt: &Context) -> Result<Type, ParseError> {
        let mut resolve_ctxt = self.context.clone();

        for ((name, param), arg) in self.params.iter().zip(arguments.iter()) {
            match param {
                ast::TemplateParameter::TypeName(_) => {
                    let ty = interpret_as_type(arg, ctxt)?;
                    resolve_ctxt.add_type(name, ty)?;
                }
                ast::TemplateParameter::Const(_) => {
                    let val = interpret_as_const(arg, ctxt)?;
                    resolve_ctxt.add_name(name, val)?;
                }
            }
        };

        parse_struct(&self.struct_, &mut resolve_ctxt)
    }
}

fn interpret_as_type(arg: &ast::TemplateArgument, ctxt: &Context) -> Result<Type, ParseError> {
    match arg {
        ast::TemplateArgument::Type(ty) => parse_type(ty, ctxt),
        ast::TemplateArgument::Const(ast::ConstExpr::Name(name)) => ctxt.get_type(name),
        _ => Err(ParseError::MismatchedTemplate)
    }
}

fn interpret_as_const(arg: &ast::TemplateArgument, ctxt: &Context) -> Result<usize, ParseError> {
    match arg {
        ast::TemplateArgument::Const(expr) => parse_const_expr(expr, ctxt),
        ast::TemplateArgument::Type(ast::Type::Name(name)) => ctxt.get_name(name),
        _ => Err(ParseError::MismatchedTemplate)
    }
}

fn parse_file(f: &ast::File) -> Result<Context, ParseError> {
    let mut ctxt = Context::new();
    ctxt.add_type("uint32_t", Type::UnsignedInteger(Bits::I32)).ok();
    ctxt.add_type("systime_t", Type::UnsignedInteger(Bits::I32)).ok();
    ctxt.add_type("float", Type::Float).ok();
    ctxt.add_type("bool", Type::Boolean).ok();

    for top_level in &f.top_levels {
        parse_top_level(top_level, &mut ctxt)?;
    }

    Ok(ctxt)
}

fn parse_top_level(t: &ast::TopLevel, ctxt: &mut Context) -> Result<(), ParseError> {
    match t {
        ast::TopLevel::Struct(struct_) => {
            let name = struct_.name.clone();
            let struct_ = parse_struct(struct_, ctxt)?;
            ctxt.add_type(name, struct_)
        },
        ast::TopLevel::Enum(enum_) => parse_enum(enum_, ctxt),
        ast::TopLevel::TypeDef(typedef) => parse_typedef(typedef, ctxt),
        ast::TopLevel::TemplateStruct(template) => parse_template(template, ctxt),
        ast::TopLevel::Other => Ok(())
    }
}

fn parse_template(template: &ast::TemplateStruct, ctxt: &mut Context) -> Result<(), ParseError> {
    let name = template.struct_.name.clone();

    let struct_ = template.struct_.clone();
    let context = ctxt.clone();

    let params: Vec<(String, ast::TemplateParameter)> = template.parameters.iter().map(|param| {
        match param {
            ast::TemplateParameter::TypeName(name) => (name.clone(), param.clone()),
            ast::TemplateParameter::Const(name) => (name.clone(), param.clone()),
        }
    }).collect();

    let template = Template { struct_, context, params };

    ctxt.add_template(name, template)
}

fn parse_typedef(t: &ast::TypeDef, ctxt: &mut Context) -> Result<(), ParseError> {
    let old = parse_type(&t.actual, ctxt)?;
    ctxt.add_type(&t.new, old)
}

fn parse_enum(e: &ast::Enum, ctxt: &mut Context) -> Result<(), ParseError> {
    let name = e.name.clone();

    let variants: HashMap<usize, String> = e.variants.iter().enumerate().map(|(i, name)| (i, name.clone())).collect();

    ctxt.add_type(name, Type::Enum(variants))
}

fn parse_struct(t: &ast::Struct, ctxt: &mut Context) -> Result<Type, ParseError> {
    let mut fields = Vec::with_capacity(t.items.len());

    for item in &t.items {
        match item {
            ast::StructItem::Field { decl, .. } => {
                let (name, ty) = parse_decl(decl, ctxt)?;
                fields.push((name, ty));
            }
            ast::StructItem::Other => ()
        }
    };

    Ok(Type::Struct(fields))
}

fn parse_decl(decl: &ast::Decl, ctxt: &Context) -> Result<(String, Type), ParseError> {
    let name = decl.name.clone();

    let mut ty = parse_type(&decl.typ, ctxt)?;

    for array in &decl.arrays {
        let num = parse_const_expr(array, ctxt)?;
        ty = Type::Array(Box::new(ty), num);
    }

    Ok((name, ty))
}

fn parse_type(ty: &ast::Type, ctxt: &Context) -> Result<Type, ParseError> {
    match ty {
        ast::Type::Name(name) => ctxt.get_type(name),
        ast::Type::Template(template, arguments) => {
            let template = ctxt.get_template(template)?;
            template.resolve(arguments, ctxt)
        }
    }
}

fn parse_const_expr(expr: &ast::ConstExpr, ctxt: &Context) -> Result<usize, ParseError> {
    match expr {
        ast::ConstExpr::Name(name) => ctxt.get_name(name),
        ast::ConstExpr::Integer(n) => Ok(*n),
        ast::ConstExpr::Float(_) => Err(ParseError::FloatAsConstant)
    }
}

fn parse_into_context(s: impl Into<String>) -> Result<Context, String> {
    let processed = preprocess(s.into());

    let ast = parser::FileParser::new().parse(&processed).map_err(|e| e.to_string())?;

    parse_file(&ast).map_err(|e| e.to_string())
}

fn preprocess(s: String) -> String {
    let comment_regex = Regex::new(r"//.*|/\*(?s:.)*?\*/").unwrap();

    let s = comment_regex.replace_all(&s, "");

    let mut processed = String::with_capacity(s.len());

    for line in s.lines() {
        if !line.starts_with("#") {
            processed.push_str(line);
            processed.push('\n');
        }
    }

    processed
}

#[cfg(test)]
mod test {
    use std::io::Read;
    use crate::header_parser::{parse_and_load, parse_into_context, Parser};

    #[test]
    fn why() {
        let s = std::fs::read_to_string(r"C:\Users\magil\Downloads\packet.h").unwrap();
        let mut buf = Vec::new();
        std::fs::File::open(r"data30.launch").unwrap().read_to_end(&mut buf).unwrap();

        let ctxt = parse_into_context(s).unwrap();
        let ty = ctxt.get_type("sensorDataStruct_t").unwrap();

        Parser::parse_into_table_no_tracker(&ty, &buf).unwrap();
    }
}