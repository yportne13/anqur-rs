/*use crate::parser::ast::{Param, Expr, Clause};

use super::{term::{Term, ParamTerm}, defvar::DefVar};


#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    Fn {
        name: Box<DefVar>,
        telescope: Vec<ParamTerm>,
        result: Term,
        body: Result<Term, Vec<Clause<Term>>>,
    },
    Data {
        name: Box<DefVar>,
        telescope: Vec<ParamTerm>,
        cons: Vec<Def>,
    },
    Cons {
        //TODO:name: Box<DefVar>,
        name: String,
        owner: Box<DefVar>,
        tele: Vec<ParamTerm>,
    },
    Print {
        telescope: Vec<ParamTerm>,
        result: Term,
        body: Term,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Signature {
    pub is_data: bool,
    pub telescope: Vec<ParamTerm>,
    pub result: Term,
}
*/