use std::collections::HashMap;

use crate::{parser::ast::{Decl, Expr}, syntax::{def::Def, term::Term}};



pub struct Elaborator {
    pub sigma: HashMap<String, String>,
    pub ggama: HashMap<String, String>,
}

impl Elaborator {
    pub fn inherit(&mut self, expr: Expr, ty: Term) -> Term {
        match expr {
            Expr::Lam(_, _) => todo!(),
            Expr::Two(_, _) => todo!(),
            Expr::Fst(_) => todo!(),
            Expr::Snd(_) => todo!(),
            Expr::Univ => todo!(),
            Expr::Arrow(_, _) => todo!(),
            Expr::Times(_, _) => todo!(),
            Expr::Pi(_, _) => todo!(),
            Expr::Sig(_, _) => todo!(),
            Expr::Pair(_, _) => todo!(),
            Expr::Ref(_) => todo!(),
            Expr::Paren(_) => todo!(),
        }
    }
    pub fn def(&mut self, def: Decl) -> Def {
        match def {
            Decl::Def{name, tele, result, body} => {
                let mut result = self.inherit(result, Term::UI);

                todo!()
            },
            Decl::Print{tele, result, body} => todo!(),
            Decl::Data{name, tele, cons} => todo!(),
        }
    }
}
