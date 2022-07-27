use std::collections::HashMap;

use crate::{
    eval::{Addr, Mem, Value},
    Expr, Id, Procedure, Type,
};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Store {
    mem: Mem,
    cross_refs: HashMap<Id, Value>,
    procedures: Vec<(Addr, Procedure)>,
    strings: HashMap<String, Expr>,
}

impl Store {
    pub fn insert_cross_ref(&mut self, type_attr: Type, id: Id) -> Addr {
        let addr = self.mem.push_static(&type_attr);
        self.cross_refs.insert(id, Value { type_attr, addr });
        addr
    }

    pub fn get_cross_ref(&self, id: &Id) -> Expr {
        Expr::Static(self.cross_refs[id].clone())
    }

    pub fn get_main(&self) -> Value {
        let value = &self.cross_refs[&Id(vec![String::from(""), String::from("main")])];
        assert_eq!(
            &value.type_attr,
            &Type::Procedure(vec![], Box::new(Type::Unit))
        );
        value.clone()
    }

    pub fn push_procedure(&mut self, procedure: Procedure) {
        let addr = if let Some(Value { type_attr, addr }) = self.cross_refs.get(&procedure.id) {
            assert_eq!(&procedure.type_attr, type_attr);
            *addr
        } else {
            self.mem.push_static(&procedure.type_attr)
        };
        self.procedures.push((addr, procedure));
    }

    pub fn insert_str(&mut self, str: &str) -> Expr {
        self.strings
            .entry(str.to_string())
            .or_insert_with(|| {
                let len = str.as_bytes().len();
                let type_attr = Type::Array(Box::new(Type::U8), len);
                let addr = self.mem.push_static(&type_attr);
                self.mem
                    .store_byte_slice(&Value { type_attr, addr }, str.as_bytes());
                Expr::ConstSlice(Type::U8, addr, len)
            })
            .clone()
    }

    pub fn into_mem(self) -> Mem {
        let mut mem = self.mem;
        mem.set_procedures(self.procedures.into_iter());
        mem
    }
}
