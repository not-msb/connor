use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Ast {
    Block {
        ty: Type,
        nodes: Vec<Ast>,
    },
    Literal {
        ty: Type,
        storage: Storage,
        content: String,
    },
    BinOp {
        bin_op: BinOp,
        left: Box<Ast>,
        right: Box<Ast>,
    },
    Assign {
        ty: Type,
        variable: String,
        expr: Box<Ast>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    // Meta Types
    Var,
    // Real Types
    Void,
    U32,
    Bool,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy)]
pub enum Storage {
    Direct,
    Stack,
}

#[derive(Debug, Default, Clone)]
pub struct Env {
    pub stack: HashMap<String, usize>,
    pub ssize: usize,
    pub types: HashMap<String, Type>,
}
