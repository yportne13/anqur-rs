use std::collections::HashMap;

use crate::{parser::ast::{Decl, Expr, Param, Id, Clause}, syntax::{def::{Def, Signature}, term::Term, defvar::DefVar}, Error};

pub struct Synth {
    pub well_typed: Term,
    pub ty: Term,
}

pub struct Elaborator {
    pub sigma: HashMap<String, String>,
    pub gamma: HashMap<String, Term>,
}

impl Elaborator {
    pub fn inherit(&mut self, expr: &Expr, ty: &Term) -> Term {
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
    pub fn unify(&mut self, ty: Term, on: (), actual: Term, pos: ()) -> Result<(), String> {
        todo!()
    }
    fn synth(&mut self, expr: Expr) -> Synth {
        todo!()
    }
    pub fn def(&mut self, def: &mut Decl) -> Result<Def, Error> {
        let telescope = self.telescope(def.tele());
        match def {
            Decl::Def{name, tele: _, result, body} => {
                let result = self.inherit(result, &Term::UI);
                let defvar = DefVar {
                    core: None,
                    signature: Signature {
                        is_data: false,
                        telescope: telescope.clone(),
                        result: result.clone(),
                    },
                    name: name.0.clone(),
                };
                let body = match body {
                    crate::parser::ast::FnBody::Expr(e) => Ok(self.inherit(e, &result)),
                    crate::parser::ast::FnBody::Clause(c) => Err(self.tyck_fun_body(&telescope, &result, c)),
                };
                telescope.iter()
                    .for_each(|t| { self.gamma.remove(&t.0.0); });
                Ok(Def::Fn { name: Box::new(defvar), telescope, result, body })
            },
            Decl::Print{tele, result, body} => {
                let result = self.inherit(result, &Term::UI);
                let body = self.inherit(body, &result);
                telescope.iter()
                    .for_each(|t| { self.gamma.remove(&t.0.0); });
                Ok(Def::Print { telescope, result, body })
            },
            //TODO: Decl::Cons => Err(e)
            Decl::Data{name, tele: _, cons} => {
                let name = DefVar {
                    core: None,
                    signature: Signature { is_data: true, telescope: telescope.clone(), result: Term::UI },
                    name: name.0.clone(),
                };
                Ok(Def::Data {
                    name: Box::new(name.clone()),
                    telescope,
                    cons: cons.iter().map(|c| {
                        Def::Cons { name: c.name.0.clone(), owner: Box::new(name.clone()), tele: c.tele.clone() }
                    }).collect()
                })
            },
        }
    }
    fn tyck_fun_body(
        &mut self,
        telescope: &[Param<Term>],
        result: &Term,
        clause_set: &[Clause]
    ) -> Vec<Clause> {
        let clause = clause_set.iter()
            .map(|x| self.classify(telescope, &result, x))
            .collect::<Vec<_>>();
        crate::tyck::classifier::classify(&clause, telescope);
        clause
    }
    pub fn telescope(&mut self, tele: &[Param<Expr>]) -> Vec<Param<Term>> {
        let mut ret = vec![];
        for param in tele {
            let ty = self.inherit(&param.1, &Term::UI);
            ret.push(Param(param.0.clone(), Box::new(ty.clone())));
            self.gamma.insert(param.0.0.clone(), ty);
        }
        ret
    }
}

impl Elaborator {
    fn classify(&self, params: &[Param<Term>], result: &Term, clause: &Clause) -> Clause {
        todo!()
    }
}
