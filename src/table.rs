#![allow(dead_code)]

use std::fmt::{Display, Formatter};
use std::iter;
use indexmap::IndexMap;

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub enum DataType {
    Bool,
    U32,
    F32,
    String,
}

impl Display for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            DataType::Bool => "bool",
            DataType::U32 => "u32",
            DataType::F32 => "f32",
            DataType::String => "string"
        })
    }
}

#[derive(PartialEq, Clone)]
pub enum Value {
    Bool(bool),
    U32(u32),
    F32(f32),
    String(String)
}

impl Value {
    pub fn dtype(&self) -> DataType {
        match self {
            Value::Bool(_) => DataType::Bool,
            Value::U32(_) => DataType::U32,
            Value::F32(_) => DataType::F32,
            Value::String(_) => DataType::String,
        }
    }
}

#[derive(Eq, PartialEq, Clone, Default)]
pub struct Schema {
    names: IndexMap<String, usize>,
    rows: Vec<DataType>
}

impl Schema {
    pub fn new() -> Schema {
        Schema::default()
    }

    pub fn with_column(&mut self, name: String, typ: DataType) {
        let idx = self.rows.len();
        if let Some(_) = self.names.insert(name, idx) {
            panic!("Name already used");
        }
        self.rows.push(typ);
    }

    pub fn columns(&self) -> Vec<&str> {
        self.names.keys().map(|r| r as &str).collect()
    }

    pub fn column_types(&self) -> Vec<(&str, DataType)> {
        self.names.iter().map(|(name, idx)| (name as &str, self.rows[*idx])).collect()
    }
}

impl FromIterator<(String, DataType)> for Schema {
    fn from_iter<T: IntoIterator<Item=(String, DataType)>>(iter: T) -> Self {
        let mut schema = Schema::new();
        for (name, typ) in iter {
            schema.with_column(name, typ);
        }
        schema
    }
}

pub struct Row<'s> {
    schema: &'s Schema
}

pub struct ColumnView<'a> {
    view: &'a Column
}

impl<'a> ColumnView<'a> {
    pub fn as_bool(&self) -> &[bool] {
        match self.view {
            Column::Bool(vec) => vec,
            _ => panic!()
        }
    }

    pub fn as_u32(&self) -> &[u32] {
        match self.view {
            Column::U32(vec) => vec,
            _ => panic!()
        }
    }

    pub fn as_f64(&self) -> Box<dyn Iterator<Item=f64> + 'a> {
        match self.view {
            Column::F32(vec) => Box::new(vec.iter().map(|v| *v as f64)),
            Column::U32(vec) => Box::new(vec.iter().map(|v| *v as f64)),
            Column::Bool(vec) => Box::new(vec.iter().map(|v| *v as u8 as f64)),
            Column::String(vec) => Box::new(iter::repeat(0.0).take(vec.len())),
        }
    }

    pub fn as_strings(&self) -> &[String] {
        match self.view {
            Column::String(vec) => vec,
            _ => panic!()
        }
    }
}

enum Column {
    Bool(Vec<bool>),
    U32(Vec<u32>),
    F32(Vec<f32>),
    String(Vec<String>)
}

impl Column {
    fn from_dtype(typ: DataType) -> Column {
        match typ {
            DataType::Bool => Column::Bool(vec![]),
            DataType::U32 => Column::U32(vec![]),
            DataType::F32 => Column::F32(vec![]),
            DataType::String => Column::String(vec![]),
        }
    }

    fn push(&mut self, value: Value) {
        match (self, value) {
            (Column::Bool(vec), Value::Bool(value)) => vec.push(value),
            (Column::U32(vec), Value::U32(value)) => vec.push(value),
            (Column::F32(vec), Value::F32(value)) => vec.push(value),
            (Column::String(vec), Value::String(value)) => vec.push(value),
            _ => panic!()
        }
    }
}

pub struct Table {
    schema: Schema,
    columns: Vec<Column>
}

impl Table {
    pub fn from_schema(schema: Schema) -> Table {
        let columns: Vec<Column> = schema.rows.iter().map(|dtype| Column::from_dtype(*dtype)).collect();
        Table {
            schema,
            columns
        }
    }

    pub fn schema(&self) -> &Schema {
        &self.schema
    }

    pub fn add_row(&mut self, row: Vec<Value>) {
        if self.columns.len() != row.len() {
            panic!()
        }

        for (column, value) in self.columns.iter_mut().zip(row) {
            column.push(value)
        }
    }

    pub fn column(&self, name: &str) -> ColumnView {
        ColumnView { view: &self.columns[self.schema.names[name]] }
    }
}

