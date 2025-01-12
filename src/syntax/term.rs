/*use std::fmt::Debug;

use crate::{parser::ast::Locate, tyck::{normalizer::Normalizer}};

use super::defvar::DefVar;

#[derive(Clone, Debug, PartialEq)]
pub struct ParamTerm {
    pub id: LocalVar,
    pub ty: Box<Term>,
    pub loc: Locate,
}

#[derive(Clone, PartialEq)]
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

impl Debug for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Error{msg} => write!(f, "Error({})", msg),
            Term::Ref{var} => write!(f, "Ref({:?})", var),
            Term::FnCall{fun, args} => write!(f, "{}({:?})", fun.name, args),
            Term::DataCall{fun, args} => write!(f, "{}({:?})", fun.name, args),
            Term::ConCall{fun, args} => write!(f, "{}({:?})", fun.name, args),
            Term::Two{is_app: true, f: fun, a} => write!(f, "({:?} {:?})", fun, a),
            Term::Two{is_app: false, f: fun, a} => write!(f, "({:?} {:?})", fun, a),
            Term::Proj { t, is_one: true } => write!(f, "Proj({:?})", t),
            Term::Proj { t, is_one: false } => write!(f, "Proj({:?})", t),
            Term::Lam { x, body } => write!(f, "Lam({:?}, {:?})", x, body),
            Term::DT{is_pi: true, param, cod} => write!(f, "pi({:?}, {:?})", param.id, cod),
            Term::DT{is_pi: false, param, cod} => write!(f, "si({:?}, {:?})", param.id, cod),
            Term::UI => write!(f, "UI"),
        }
    }
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
            Term::DT { is_pi: _, param, cod } => {
                Normalizer(std::iter::once((param.id, term.clone())).collect())
                    .term(cod)
            },
            _ => unreachable!(),
        }
    }
    pub fn app(&self, args: Vec<Term>) -> Term {
        args.into_iter()
            .fold(self.clone(), |t, a| {
                match t {
                    Term::Lam { x , body } => {
                        body.subst(x, a.clone())
                    },
                    _ => {
                        Term::Two { is_app: true, f: Box::new(t), a: Box::new(a.clone()) }
                    }
                }
            })
    }
}

impl Term {
    pub fn map_term_id<F>(&self, f: F) -> Term
    where
        F: Fn(u32) -> u32 + Copy,
    {
        match self {
            Term::Error { msg } => Term::Error { msg: msg.clone() },
            Term::Ref { var } => Term::Ref { var: LocalVar { id: f(var.id), name: var.name } },
            Term::FnCall { fun, args } => todo!(),
            Term::DataCall { fun, args } => todo!(),
            Term::ConCall { fun, args } => todo!(),
            Term::Two { is_app, f, a } => todo!(),
            Term::Proj { t, is_one } => todo!(),
            Term::Lam { x, body } => Term::Lam {
                x: LocalVar { id: f(x.id), name: x.name },
                body: Box::new(body.map_term_id(f)),
            },
            Term::DT { is_pi, param, cod } => Term::DT {
                is_pi: *is_pi,
                param: ParamTerm {
                    id: LocalVar { id: f(param.id.id), name: param.id.name },
                    ty: Box::new(param.ty.map_term_id(f)),
                    loc: param.loc,
                },
                cod: Box::new(cod.map_term_id(f))
            },
            Term::UI => Term::UI,
        }
    }
}
*/