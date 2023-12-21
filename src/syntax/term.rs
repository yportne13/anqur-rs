use std::collections::HashMap;

use crate::{parser::ast::{Param, Expr, Locate}, tyck::{normalizer::Normalizer, elaborator::LocalVar}};

use super::defvar::DefVar;

#[derive(Clone, Debug)]
pub struct ParamTerm {
    pub id: LocalVar,
    pub ty: Box<Term>,
    pub loc: Locate,
}

#[derive(Clone, Debug)]
pub enum Term {
    Error{msg: String},
    Ref{var: LocalVar},
    FnCall{fun: Box<DefVar>, args: Vec<Term>},
    DataCall{fun: Box<DefVar>, args: Vec<Term>},
    ConCall{fun: Box<DefVar>, args: Vec<Term>},
    Two{is_app: bool, f: Box<Term>, a: Box<Term>},
    Proj{t: Box<Term>, is_one: bool},
    Lam{x: LocalVar, body: Box<Term>},
    DT{is_pi: bool, param: ParamTerm, cod: Box<Term>},
    UI,
}

impl Term {
    pub fn subst(&self, x: LocalVar, t: Term) -> Term {
        Normalizer(std::iter::once((x, t)).collect())
            .term(self)
    }
    pub fn subst_empty(&self) -> Term {
        Normalizer(Default::default())
            .term(self)
    }
    pub fn codomain(&self, term: Term) -> Term {
        match self {
            Term::DT { is_pi, param, cod } => todo!(),
            _ => unreachable!(),
        }
    }
}
