use crate::parser::ast::{Param, Expr, Clause};

use super::{term::Term, defvar::DefVar};


#[derive(Clone, Debug)]
pub enum Def {
    Fn {
        name: Box<DefVar>,
        telescope: Vec<Param<Term>>,
        result: Term,
        body: Result<Term, Vec<Clause>>,
    },
    Data {
        name: Box<DefVar>,
        telescope: Vec<Param<Term>>,
        cons: Vec<Def>,
    },
    Cons {
        //TODO:name: Box<DefVar>,
        name: String,
        owner: Box<DefVar>,
        tele: Vec<Param<Expr>>,//TODO:should be Param<Term>
    },
    Print {
        telescope: Vec<Param<Term>>,
        result: Term,
        body: Term,
    }
}

#[derive(Clone, Debug)]
pub struct Signature {
    pub is_data: bool,
    pub telescope: Vec<Param<Term>>,
    pub result: Term,
}
