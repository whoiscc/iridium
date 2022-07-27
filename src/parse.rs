use std::mem::replace;

use crate::{eval::Value, store::Store, Expr, Id, Procedure, Stmt, Type};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token<'a> {
    Special(&'a str),
    Id(Id),
    LiteralStr(String),
}

struct TokenIter<'a> {
    inner: &'a str,
    // not using Peekable from standard library because that is blackbox
    peek: Option<Token<'a>>,
}

impl<'a> TokenIter<'a> {
    fn new(inner: &'a str) -> Self {
        Self {
            inner: inner.trim_start(),
            peek: None,
        }
    }

    fn next_token(&mut self) -> Option<Token<'a>> {
        while self.inner.starts_with('#') {
            let line = self.inner.lines().next().unwrap();
            self.inner = self.inner[line.len()..].trim_start();
        }
        if self.inner.is_empty() {
            return None;
        }

        if self.inner.starts_with('"') {
            let mut escape = false;
            let (literal, inner) = self
                .inner
                .strip_prefix('"')
                .unwrap()
                .split_once(|c| {
                    escape = if c == '\\' { !escape } else { false };
                    c == '"' && !escape
                })
                .unwrap();
            self.inner = inner.trim_start();
            // TODO complete escape
            let literal = literal.replace("\\n", "\n");
            return Some(Token::LiteralStr(literal));
        }

        if self.inner.starts_with(char::is_alphabetic)
            || self.inner.starts_with('_')
            || self.inner.starts_with("//")
        {
            let mut id = vec![];
            if self.inner.starts_with("//") {
                id.push(String::new());
                self.inner = self.inner.strip_prefix("//").unwrap();
            }
            let id = loop {
                // is it a good choice to allow id part start with numeric?
                let inner = self
                    .inner
                    .trim_start_matches(|c: char| c.is_alphanumeric() || c == '_');
                assert!(inner.len() < self.inner.len());
                id.push(self.inner[..self.inner.len() - inner.len()].to_string());
                if let Some(inner) = inner.strip_prefix('/') {
                    self.inner = inner;
                } else {
                    self.inner = inner.trim_start();
                    break Id(id);
                }
            };
            if let Some(id) = id.as_scoped() {
                for special in ["procedure", "begin", "end", "unit", "u8", "u64"] {
                    if id == special {
                        return Some(Token::Special(special));
                    }
                }
            }
            return Some(Token::Id(id));
        }

        for special in ["(", ")", "[", "]", "->", ";", ",", "&"] {
            if self.inner.starts_with(special) {
                self.inner = self.inner.strip_prefix(special).unwrap().trim_start();
                return Some(Token::Special(special));
            }
        }

        panic!()
    }

    fn peek(&mut self) -> &Option<Token<'a>> {
        if self.peek.is_none() {
            self.peek = self.next_token();
        }
        &self.peek
    }

    fn next(&mut self) -> Option<Token<'a>> {
        if let Some(token) = self.peek.take() {
            Some(token)
        } else {
            self.next_token()
        }
    }

    fn skip(&mut self, expect: Token<'_>) {
        assert_eq!(self.next(), Some(expect));
    }
}

pub struct Parse<'a, T> {
    token: TokenIter<'a>,
    namespace: Id,
    store: T,
}

impl<'a> Parse<'a, &'a mut Store> {
    pub fn new(inner: &'a str, namespace: Id, store: &'a mut Store) -> Self {
        Self {
            token: TokenIter::new(inner),
            namespace,
            store,
        }
    }
}

impl<'a> Parse<'a, ()> {
    pub fn new_cross_ref(inner: &'a str, namespace: Id) -> Self {
        Self {
            token: TokenIter::new(inner),
            namespace,
            store: (),
        }
    }
}

pub trait ParseStore {
    fn insert_str(&mut self, str: &str) -> Expr;
    fn cross_ref(&self, id: &Id) -> Expr;
}

impl ParseStore for Parse<'_, &'_ mut Store> {
    fn insert_str(&mut self, str: &str) -> Expr {
        self.store.insert_str(str)
    }
    fn cross_ref(&self, id: &Id) -> Expr {
        self.store.get_cross_ref(id)
    }
}

impl ParseStore for Parse<'_, ()> {
    fn insert_str(&mut self, _: &str) -> Expr {
        Expr::Stub
    }
    fn cross_ref(&self, _: &Id) -> Expr {
        Expr::Stub
    }
}

impl<T> Parse<'_, T>
where
    Self: ParseStore,
{
    fn parse_expr(&mut self) -> Expr {
        self.parse_infix_expr()
    }

    fn parse_infix_expr(&mut self) -> Expr {
        self.parse_prefix_expr()
    }

    fn parse_prefix_expr(&mut self) -> Expr {
        self.parse_postfix_expr()
    }

    fn parse_postfix_expr(&mut self) -> Expr {
        let mut expr = self.parse_core_expr();
        loop {
            if self.token.peek() == &Some(Token::Special("(")) {
                expr = self.parse_call(expr);
            } else if self.token.peek() == &Some(Token::Special("[")) {
                todo!()
            } else {
                return expr;
            }
        }
    }

    fn parse_call(&mut self, procedure: Expr) -> Expr {
        self.token.skip(Token::Special("("));
        let mut args = vec![];
        while self
            .token
            .peek()
            .as_ref()
            .filter(|&token| token != &Token::Special(")"))
            .is_some()
        {
            args.push(self.parse_expr());
            if self.token.peek() != &Some(Token::Special(")")) {
                self.token.skip(Token::Special(","));
            }
        }
        self.token.skip(Token::Special(")"));
        Expr::Call(Box::new(procedure), args)
    }

    fn parse_core_expr(&mut self) -> Expr {
        match self.token.next().unwrap() {
            Token::LiteralStr(literal) => self.insert_str(&literal),
            Token::Id(id) => {
                // scoped
                let expr = self.cross_ref(&id.into_absolute(&self.namespace));
                expr
            }
            _ => panic!(),
        }
    }

    fn parse_type(&mut self) -> Type {
        match self.token.next().unwrap() {
            Token::Special("unit") => Type::Unit,
            Token::Special("u8") => Type::U8,
            Token::Special("u64") => Type::U64,
            Token::Special("&") => {
                if self.token.peek() != &Some(Token::Special("[")) {
                    Type::Ref(Box::new(self.parse_type()))
                } else {
                    self.token.next();
                    let inner = self.parse_type();
                    if self.token.peek() == &Some(Token::Special("]")) {
                        self.token.next();
                        Type::Slice(Box::new(inner))
                    } else {
                        self.token.skip(Token::Special(";"));
                        todo!() // array
                    }
                }
            }
            _ => panic!(),
        }
    }

    fn parse_scope(&mut self, close: &[Token]) -> Expr {
        let mut expr = Expr::Static(Value::UNIT);
        let mut stmts = vec![];
        while self
            .token
            .peek()
            .as_ref()
            .filter(|&token| close.iter().all(|close| close != token))
            .is_some()
        {
            match self.token.peek().as_ref().unwrap() {
                Token::Special("variable") => todo!(),
                Token::Special("while") => todo!(),
                _ => {
                    assert_eq!(expr, Expr::Static(Value::UNIT));
                    expr = self.parse_expr();
                }
            }
            while self.token.peek() == &Some(Token::Special(";")) {
                self.token.next();
                if expr != Expr::Static(Value::UNIT) {
                    stmts.push(Stmt::Expr(replace(&mut expr, Expr::Static(Value::UNIT))));
                }
            }
        }
        Expr::Scope(stmts, Box::new(expr))
    }

    pub fn parse_procedure(&mut self) -> Procedure {
        self.token.skip(Token::Special("procedure"));
        let id = if let Some(Token::Id(id)) = self.token.next() {
            id
        } else {
            panic!()
        };
        self.token.skip(Token::Special("("));
        let mut param_types = vec![];
        let mut params = vec![];
        while self
            .token
            .peek()
            .as_ref()
            .filter(|&token| token != &Token::Special(")"))
            .is_some()
        {
            param_types.push(self.parse_type());
            if let Token::Id(id) = self.token.next().unwrap() {
                params.push(id.as_scoped().unwrap().to_string());
            } else {
                panic!();
            }
            if self.token.peek() != &Some(Token::Special(")")) {
                self.token.skip(Token::Special(","));
            }
        }
        self.token.skip(Token::Special(")"));
        self.token.skip(Token::Special("->"));
        let returned_type = self.parse_type();

        let expr = if self.token.peek() == &Some(Token::Special(";")) {
            self.token.next();
            Expr::Stub
        } else {
            self.token.skip(Token::Special("begin"));
            let expr = self.parse_scope(&[Token::Special("end")]);
            self.token.skip(Token::Special("end"));
            self.token.skip(Token::Special("procedure"));
            expr
        };
        Procedure {
            id: id.into_absolute(&self.namespace),
            type_attr: Type::Procedure(param_types, Box::new(returned_type)),
            params,
            expr,
        }
    }
}

impl Iterator for Parse<'_, ()> {
    type Item = (Type, Id);

    fn next(&mut self) -> Option<Self::Item> {
        match self.token.peek() {
            None => None,
            Some(Token::Special("procedure")) => {
                let Procedure { id, type_attr, .. } = self.parse_procedure();
                Some((type_attr, id))
            }
            _ => panic!(),
        }
    }
}

impl Parse<'_, &'_ mut Store> {
    pub fn make(mut self) {
        loop {
            match self.token.peek() {
                None => return,
                Some(Token::Special("procedure")) => {
                    let procedure = self.parse_procedure();
                    self.store.push_procedure(procedure);
                }
                _ => panic!(),
            }
        }
    }
}
