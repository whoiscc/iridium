use std::{
    collections::{BTreeMap, HashMap},
    io::{stdout, Write},
};

use crate::{parse::Parse, store::Store, Expr, Id, Procedure, Stmt, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Addr {
    Static(usize),
    Stack(usize),
    Heap(usize),
}

impl From<Addr> for u64 {
    fn from(addr: Addr) -> Self {
        const KIND_OFFSET: u32 = usize::BITS - 2;
        (match addr {
            Addr::Static(i) => {
                assert_eq!(i & 0x11 << KIND_OFFSET, 0);
                i | 0x01 << KIND_OFFSET
            }
            Addr::Stack(i) => {
                assert_eq!(i & 0x11 << KIND_OFFSET, 0);
                i | 0x10 << KIND_OFFSET
            }
            Addr::Heap(i) => {
                assert_eq!(i & 0x11 << KIND_OFFSET, 0);
                i | 0x11 << KIND_OFFSET
            }
        }) as _
    }
}

impl Addr {
    const fn from(addr: u64) -> Self {
        // let addr = addr as usize;
        let (kind, addr) = (addr >> (u64::BITS - 2), (addr & u64::MAX >> 2) as usize);
        match kind {
            0x1 => Self::Static(addr),
            0x2 => Self::Stack(addr),
            0x3 => Self::Heap(addr),
            _ => unreachable!(), // zero memory is not a valid pointer
        }
    }
}

// any evaluate result, including immediate ones e.g. in 1 + 2 * 3, has an
// address. so it must be stored in memory at some point. in another word, this
// VM has no register
// seems like a reasonable tradeoff between performance and simplicity, since
// this is just the VM implementation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    pub type_attr: Type,
    pub addr: Addr,
}
impl Value {
    pub const UNIT: Self = Self {
        type_attr: Type::Unit,
        addr: Addr::from(u64::MAX), // is this good choice?
    };
}

#[derive(Clone, Default)]
pub struct Eval {
    pub mem: Mem,
    intrinsics: HashMap<Addr, (Procedure, Intrinsic)>,
    frames: Vec<Frame>,
}
type Intrinsic = fn(&mut Eval) -> Value;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Frame {
    scopes: Vec<HashMap<String, Value>>,
    control: Control,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Control {
    Run,
    Break,
    Continue,
    Return(Value),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Mem {
    static_section: Vec<u8>,
    procedures: HashMap<Addr, Procedure>,
    stack: Vec<u8>,
    heap: BTreeMap<usize, Vec<u8>>,
}

impl Eval {
    const INTRINSICS: [(&'static str, Intrinsic); 1] = [(
        "procedure write_stdout(&[u8] buf) -> u64;",
        Self::intrinsic_write_stdout,
    )];
}

impl Mem {
    pub fn push_static(&mut self, type_attr: &Type) -> Addr {
        let addr = Addr::Static(self.static_section.len());
        self.static_section.extend(vec![0; type_attr.alloc_size()]);
        addr
    }

    pub fn alloc_stack(&mut self, type_attr: &Type) -> Value {
        let addr = Addr::Stack(self.stack.len());
        self.stack.extend(vec![0; type_attr.alloc_size()]);
        Value {
            type_attr: type_attr.clone(),
            addr,
        }
    }

    pub fn shrink_stack(&mut self, top: usize) {
        self.stack.drain(top..);
    }

    // heap

    pub fn load_byte_slice(&self, value: &Value) -> &[u8] {
        let len = value.type_attr.alloc_size();
        if len == 0 {
            return &[];
        }
        // self.assert_type(addr, type_attr, len);
        match value.addr {
            Addr::Static(i) => &self.static_section[i..i + len],
            Addr::Stack(i) => &self.stack[i..i + len],
            Addr::Heap(i) => {
                let (chuck_start, chuck) = self.heap.range(..=i).last().unwrap();
                &chuck[i - chuck_start..i + len - chuck_start]
            }
        }
    }

    pub fn store_byte_slice(&mut self, value: &Value, bytes: &[u8]) {
        let len = value.type_attr.alloc_size();
        // self.assert_type(addr, &type_attr, len);
        match value.addr {
            Addr::Static(i) => &mut self.static_section[i..i + len],
            Addr::Stack(i) => &mut self.stack[i..i + len],
            Addr::Heap(i) => {
                let (chuck_start, chuck) = self.heap.range_mut(..=i).last().unwrap();
                &mut chuck[i - chuck_start..i + len - chuck_start]
            }
        }
        .copy_from_slice(bytes);
    }

    pub fn set_procedures(&mut self, procedures: impl Iterator<Item = (Addr, Procedure)>) {
        self.procedures = procedures.collect();
        // mark "procedure bytes" in static section specially?
    }

    pub fn load_u8(&self, value: &Value) -> u8 {
        assert_eq!(&value.type_attr, &Type::U8);
        self.load_byte_slice(value)[0]
    }

    pub fn store_u8(&mut self, value: &Value, content: u8) {
        assert_eq!(&value.type_attr, &Type::U8);
        self.store_byte_slice(value, &[content]);
    }

    pub fn load_u64(&self, value: &Value) -> u64 {
        assert_eq!(&value.type_attr, &Type::U8);
        u64::from_ne_bytes(self.load_byte_slice(value).try_into().unwrap())
    }

    pub fn store_u64(&mut self, value: &Value, content: u64) {
        assert_eq!(&value.type_attr, &Type::U64);
        self.store_byte_slice(value, &content.to_ne_bytes()[..]);
    }

    pub fn load_addr(&self, value: &Value) -> Addr {
        assert!(matches!(value.type_attr, Type::Ref(_)));
        Addr::from(u64::from_ne_bytes(
            self.load_byte_slice(value).try_into().unwrap(),
        ))
    }

    pub fn store_addr(&mut self, value: &Value, addr: Addr) {
        assert!(matches!(value.type_attr, Type::Ref(_)));
        self.store_byte_slice(value, &u64::from(addr).to_ne_bytes()[..]);
    }

    pub fn load_slice(&self, value: &Value) -> (Addr, usize) {
        assert!(matches!(value.type_attr, Type::Slice(_)));
        let raw_slice = self.load_byte_slice(value);
        (
            Addr::from(u64::from_ne_bytes(raw_slice[..8].try_into().unwrap())),
            u64::from_ne_bytes(raw_slice[8..].try_into().unwrap()) as _,
        )
    }

    pub fn store_slice(&mut self, value: &Value, (data, len): (Addr, usize)) {
        assert!(matches!(value.type_attr, Type::Slice(_)));
        let mut raw_slice = [0; 16];
        raw_slice[..8].copy_from_slice(&u64::from(data).to_ne_bytes()[..]);
        raw_slice[8..].copy_from_slice(&(len as u64).to_ne_bytes()[..]);
        self.store_byte_slice(value, &raw_slice[..]);
    }
}

impl Eval {
    pub fn insert_intrinsics(&mut self, store: &mut Store) {
        for (interface, intrinsic) in Self::INTRINSICS {
            let interface = Parse::new_cross_ref(interface, Id::root()).parse_procedure();
            let addr = store.insert_cross_ref(interface.type_attr.clone(), interface.id.clone());
            self.intrinsics.insert(addr, (interface, intrinsic));
        }
    }

    pub fn set_mem(&mut self, mem: Mem) {
        self.mem = mem;
    }

    pub fn eval_main(&mut self, main: Value) {
        assert_eq!(
            &main.type_attr,
            &Type::Procedure(vec![], Box::new(Type::Unit))
        );
        let value = self.eval_call(&Expr::Static(main), &[]);
        assert_eq!(value.type_attr, Type::Unit);
    }

    fn eval_expr(&mut self, expr: &Expr) -> Value {
        use Expr::*;
        match expr {
            Stub => unreachable!(),
            ConstSlice(_, data, len) => {
                let value = self.mem.alloc_stack(&expr.type_attr());
                self.mem.store_slice(&value, (*data, *len));
                value
            }
            Static(value) => value.clone(),
            ScopedId(id) => self.eval_scoped_id(id),
            Call(procedure, args) => self.eval_call(procedure, args),
            Scope(stmts, expr) => self.eval_scope(stmts, expr),
        }
    }

    fn eval_scoped_id(&self, id: &str) -> Value {
        for scope in self.frames.last().unwrap().scopes.iter().rev() {
            if let Some(value) = scope.get(id) {
                return value.clone();
            }
        }
        unreachable!()
    }

    fn eval_call(&mut self, procedure: &Expr, args: &[Expr]) -> Value {
        let Value { type_attr, addr } = self.eval_expr(procedure);
        assert!(matches!(type_attr, Type::Procedure(..)));
        let args = args
            .iter()
            .map(|arg| self.eval_expr(arg))
            .collect::<Vec<_>>();
        let frame_base = self.mem.stack.len();
        self.frames.push(Frame {
            scopes: vec![HashMap::new()],
            control: Control::Run,
        });

        fn alloc_args(mem: &mut Mem, frame: &mut Frame, args: &[Value], params: &[String]) {
            for (arg, id) in args.iter().zip(params.iter()) {
                let arg_bytes = mem.load_byte_slice(arg).to_vec();
                let arg = mem.alloc_stack(&arg.type_attr);
                mem.store_byte_slice(&arg, &arg_bytes[..]);
                frame.scopes.last_mut().unwrap().insert(id.clone(), arg);
            }
        }

        let returned = if let Some(procedure) = self.mem.procedures.get(&addr).cloned() {
            assert_eq!(&procedure.type_attr, &type_attr);
            alloc_args(
                &mut self.mem,
                self.frames.last_mut().unwrap(),
                &args,
                &procedure.params,
            );
            let mut value = self.eval_expr(&procedure.expr);
            if let Control::Return(returned_value) = self.frames.pop().unwrap().control {
                assert_eq!(value, Value::UNIT);
                value = returned_value;
            }
            value
        } else {
            let (procedure, intrinsic) = &self.intrinsics[&addr];
            assert_eq!(&procedure.type_attr, &type_attr);
            alloc_args(
                &mut self.mem,
                self.frames.last_mut().unwrap(),
                &args,
                &procedure.params,
            );
            let value = intrinsic(self);
            self.frames.pop();
            value
        };

        let returned_bytes = self.mem.load_byte_slice(&returned).to_vec();
        self.mem.shrink_stack(frame_base);
        let returned = self.mem.alloc_stack(&returned.type_attr);
        assert_eq!(returned.addr, Addr::Stack(frame_base));
        self.mem.store_byte_slice(&returned, &returned_bytes);
        returned
    }

    fn eval_scope(&mut self, stmts: &[Stmt], expr: &Expr) -> Value {
        for stmt in stmts {
            self.eval_stmt(stmt);
            if self.frames.last().unwrap().control != Control::Run {
                return Value::UNIT;
            }
        }
        self.eval_expr(expr)
    }

    fn eval_stmt(&mut self, stmt: &Stmt) {
        use Control::*;
        match stmt {
            Stmt::Expr(expr) => {
                self.eval_expr(expr); // discard result
            }
            Stmt::Loop(expr) => self.eval_loop(expr),
            Stmt::Break => self.set_control(Some(Run), Break),
            Stmt::Continue => self.set_control(Some(Run), Continue),
            Stmt::Return(expr) => {
                let value = self.eval_expr(expr);
                self.set_control(Some(Run), Return(value));
            }
        }
    }

    fn set_control(&mut self, expected: Option<Control>, control: Control) {
        let control_mut = &mut self.frames.last_mut().unwrap().control;
        if let Some(expected) = expected {
            assert_eq!(control_mut, &expected);
        }
        *control_mut = control;
    }

    fn eval_loop(&mut self, expr: &Expr) {
        loop {
            self.eval_expr(expr); // discard result
            use Control::*;
            match &self.frames.last().unwrap().control {
                Return(_) => return,
                Run | Continue => self.set_control(None, Run),
                Break => {
                    self.set_control(None, Run);
                    break;
                }
            }
        }
    }

    fn intrinsic_write_stdout(&mut self) -> Value {
        let buf = self.eval_scoped_id("buf");
        let (addr, len) = self.mem.load_slice(&buf);
        let buf = self.mem.load_byte_slice(&Value {
            type_attr: Type::Array(Box::new(Type::U8), len),
            addr,
        });
        stdout().write_all(buf).unwrap();
        let len = buf.len();
        let result = self.mem.alloc_stack(&Type::U64);
        self.mem.store_u64(&result, len as _);
        result
    }
}
