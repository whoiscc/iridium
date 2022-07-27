use eval::{Addr, Value};

pub mod eval;
pub mod parse;
pub mod store;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(pub Vec<String>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Stub,

    ConstSlice(Type, Addr, usize),
    Static(Value),
    ScopedId(String),
    Call(Box<Expr>, Vec<Expr>),
    Scope(Vec<Stmt>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Expr(Expr),
    Loop(Expr),
    Break,
    Continue,
    Return(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    U8,
    U64,
    Ref(Box<Type>),
    Array(Box<Type>, usize),
    Slice(Box<Type>),
    Procedure(Vec<Type>, Box<Type>),
}

impl Expr {
    pub fn type_attr(&self) -> Type {
        use Expr::*;
        match self {
            Stub => unreachable!(),
            ConstSlice(inner, ..) => Type::Slice(Box::new(inner.clone())),
            Static(value) => value.type_attr.clone(),
            ScopedId(_) => unreachable!(),
            Call(procedure, _) => match procedure.type_attr() {
                Type::Procedure(_, returned_type) => *returned_type,
                _ => unreachable!(),
            },
            Scope(_, expr) => expr.type_attr(),
        }
    }
}

impl Type {
    pub fn alloc_size(&self) -> usize {
        use Type::*;
        match self {
            Unit => 0,
            U8 => 1,
            U64 => 8,
            Ref(_) => 8,
            Array(inner, count) => inner.alloc_size() * count,
            Slice(_) => 16,
            // we don't really store "machine code" in memory representation,
            // instead procedure has special hacking storage. set its size to 1
            // to give it a unique address.
            Procedure(..) => 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Procedure {
    pub id: Id,
    pub type_attr: Type, // assert to be Type::Procedure
    pub params: Vec<String>,
    pub expr: Expr,
}

impl Id {
    pub fn root() -> Self {
        Id(vec![String::new()])
    }

    pub fn as_scoped(&self) -> Option<&str> {
        if let [id] = &self.0[..] {
            Some(id)
        } else {
            None
        }
    }

    pub fn is_absolute(&self) -> bool {
        self.0[0].is_empty()
    }

    pub fn into_absolute(self, namespace: &Self) -> Self {
        if self.is_absolute() {
            self
        } else {
            Self([&*namespace.0, &*self.0].concat())
        }
    }
}
