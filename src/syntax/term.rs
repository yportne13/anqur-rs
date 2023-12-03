use crate::{util::anyvar::LocalVar, parser::ast::Param};

use super::defvar::DefVar;



pub enum Term {
    Error{msg: String},
    Ref{var: LocalVar},
    FnCall{fun: Box<DefVar>, args: Vec<Term>},
    DataCall{fun: Box<DefVar>, args: Vec<Term>},
    ConCall{fun: Box<DefVar>, args: Vec<Term>},
    Two{is_app: bool, f: Box<Term>, a: Box<Term>},
    Proj{t: Box<Term>, is_one: bool},
    Lam{x: LocalVar, body: Box<Term>},
    DT{is_pi: bool, param: Param, cod: Box<Term>},
    UI,
}
