use std::collections::HashMap;

use crate::{parser::ast::{Decl, Expr, Param, Clause, Locate}, syntax::{def::{Def, Signature}, term::{Term, ParamTerm}, defvar::DefVar}, Error, Diagnostic};

use super::unifier::untyped;

#[derive(Clone, Debug)]
pub struct Synth {
    pub well_typed: Term,
    pub ty: Term,
}

pub type LocalVar = u32;

#[derive(Clone, Debug)]
pub struct Elaborator {
    pub name_id: HashMap<String, LocalVar>,
    pub unnamed_num: u32,
    pub sigma: HashMap<String, String>,
    pub gamma: HashMap<LocalVar, Term>,
}

fn normalize(term: &Term) -> Term {
    term.subst_empty()
}

impl Elaborator {
    pub fn inherit(&mut self, expr: &Expr, ty: &Term) -> Result<Term, Error> {
        match expr {
            Expr::Lam(x, a) => {
                if let dt @ Term::DT { is_pi: true, param, cod: _ } = &normalize(ty) {
                    let len = self.name_id.len() as u32 + self.unnamed_num;
                    let id = self.name_id.entry(x.0.to_owned())//TODO
                        .or_insert(len);
                    self.gamma.insert(*id, *param.ty.clone());
                    let id = *id;
                    let body = self.inherit(
                        a,
                        &dt.codomain(Term::Ref { var: id })
                    )?;
                    self.gamma.remove(&id);
                    Ok(Term::Lam { x: id, body: Box::new(body) })
                } else {
                    Err(Diagnostic {
                        pos: x.1,
                        msg: format!("Expects a right adjoint for {expr:?} got {ty:?}")
                    })
                }
            },
            Expr::Pair(f, a) => {
                if let dt @ Term::DT { is_pi: true, param, cod: _ } = &normalize(ty) {
                    let lhs = self.inherit(f, &param.ty)?;
                    let rhs = self.inherit(a, &dt.codomain(lhs.clone()))?;
                    Ok(Term::Two { is_app: false, f: Box::new(lhs), a: Box::new(rhs) })
                } else {
                    Err(Diagnostic {
                        pos: f.pos() + a.pos(),
                        msg: format!("Expects a left adjoint for {expr:?}, got {ty:?}")
                    })
                }
            },
            //TODO:hole
            _ => {
                let synth = self.synth(expr)?;
                self.unify(normalize(ty), &synth.well_typed, synth.ty, expr.pos())?;
                Ok(synth.well_typed)
            },
        }
    }
    pub fn unify(&mut self, ty: Term, on: &Term, actual: Term, pos: Locate) -> Result<(), Error> {
        //TODO: on: Docile
        if untyped(&actual, &ty)? {
            Ok(())
        } else {
            Err(Diagnostic {
                pos,
                msg: format!("Umm, {ty:?} != {actual:?} on {on:?}"),//TODO:In particular
            })
        }
    }
    fn synth(&mut self, expr: &Expr) -> Result<Synth, Error> {
        let synth = match expr {
            Expr::Univ => {
                Synth {
                    well_typed: Term::UI,
                    ty: Term::UI,
                }
            },
            Expr::RefResolved(x) => {
                //TODO:maybe wrong
                let localvar = self.name_id.get(&x.0)
                    .ok_or(Diagnostic {
                        pos: x.1,
                        msg: format!("error ref: {}", x.0),
                    })?;
                let ty = self.gamma.get(localvar)
                    .ok_or(Diagnostic {
                        pos: x.1,
                        msg: format!("error ref ty: {}", x.0),
                    })?;
                ///////////////
                /*while let Term::Ref { var } = ty {
                    ty = self.gamma.get(var).ok_or(Diagnostic {
                        pos: x.1,
                        msg: format!("error ref ty: {}", x.0),
                    })?;
                }*/
                ///////////////
                Synth {
                    well_typed: Term::Ref { var: *localvar },
                    ty: ty.clone()
                }
            },
            Expr::Fst(x) => {
                let t = self.synth(x)?;
                if let Term::DT { is_pi: false, param, cod: _ } = t.ty {
                    let mut ty = param.ty.as_ref();
                    while let Term::Ref { var } = ty {
                        ty = self.gamma.get(var).ok_or(Diagnostic {
                            pos: x.pos(),
                            msg: format!("error ref ty: {}", var),
                        })?;
                    }
                    Synth {
                        well_typed: Term::Proj { t: Box::new(t.well_typed), is_one: true },
                        ty: ty.clone(),
                    }
                } else {
                    return Err(Diagnostic {
                        pos: expr.pos(),
                        msg: format!("Expects a left adjoint, got {:?}", t.ty),
                    })
                }
            },
            Expr::Snd(x) => {
                let t = self.synth(x)?;
                if let Term::DT { is_pi: false, param, cod: _ } = t.ty {
                    let mut ty = param.ty.as_ref();
                    while let Term::Ref { var } = ty {
                        ty = self.gamma.get(var).ok_or(Diagnostic {
                            pos: x.pos(),
                            msg: format!("error ref ty: {}", var),
                        })?;
                    }
                    Synth {
                        well_typed: Term::Proj { t: Box::new(t.well_typed), is_one: false },
                        ty: ty.clone(),
                    }
                } else {
                    return Err(Diagnostic {
                        pos: expr.pos(),
                        msg: format!("Expects a left adjoint, got {:?}", t.ty),
                    })
                }
            },
            Expr::Two(f, a) => {
                let f = self.synth(f)?;
                match &f.ty {
                    dt @ Term::DT { is_pi: true, param, cod: _} => {
                        self.gamma.insert(param.id, *param.ty.clone());
                        let a = self.inherit(a, &param.ty)?;
                        self.gamma.remove(&param.id);
                        Synth {
                            well_typed: f.well_typed,
                            ty: dt.codomain(a),
                        }
                    },
                    _ => {
                        return Err(Diagnostic {
                            pos: expr.pos(),
                            msg: format!("Expects pi, got {:?} when checking {:?}", f.ty, expr),
                        })
                    }
                }
            },
            Expr::Pair(f, a) => {
                let f_pos = f.pos();
                let f = self.synth(f)?;
                let a = self.synth(a)?;
                self.unnamed_num += 1;
                Synth {
                    well_typed: Term::Two { is_app: false, f: Box::new(f.well_typed), a: Box::new(a.well_typed) },
                    ty: Term::DT {
                        is_pi: false,
                        param: ParamTerm {
                            id: self.name_id.len() as u32 + self.unnamed_num - 1,
                            ty: Box::new(f.ty),
                            loc: f_pos
                        },
                        cod: Box::new(a.ty),
                    }
                }
            },
            Expr::Dt(is_pi, param, cod) => {
                let x = param.0.0.clone();
                let param = self.synth(&param.1)?;
                let id = if &x == "_" {
                    self.name_id.len() as u32 + self.unnamed_num
                } else {
                    let len = self.name_id.len() as u32;
                    *self.name_id.entry(x)//TODO
                        .or_insert(len + self.unnamed_num)
                };
                self.gamma.insert(id, param.well_typed.clone());
                self.unnamed_num += 1;
                let cod = self.synth(cod)?;
                self.gamma.remove(&id);
                Synth {
                    well_typed: Term::DT {
                        is_pi: *is_pi,
                        param: ParamTerm {
                            id,
                            ty: Box::new(param.well_typed),
                            loc: expr.pos(),
                        },
                        cod: Box::new(cod.well_typed),
                    },
                    ty: cod.ty,
                }
            },
            Expr::Lam(_, _) | Expr::Ref(_) | Expr::Paren(_) => {
                return Err(Diagnostic {
                    pos: expr.pos(),
                    msg: format!("Synthesis failed for {expr:?}"),
                })
            },
        };
        let ty = normalize(&synth.ty);
        Ok(Synth { well_typed: synth.well_typed, ty })
    }
    pub fn def(&mut self, def: &mut Decl) -> Result<Def, Error> {
        let (telescope, to_remove) = self.telescope(def.tele())?;
        match def {
            Decl::Def{name, tele: _, result, body} => {
                let result = self.inherit(result, &Term::UI)?;
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
                    crate::parser::ast::FnBody::Expr(e) => Ok(self.inherit(e, &result)?),
                    crate::parser::ast::FnBody::Clause(c) => Err(self.tyck_fun_body(&telescope, &result, c)?),
                };
                to_remove.iter()
                    .for_each(|t| { self.gamma.remove(t); });
                Ok(Def::Fn { name: Box::new(defvar), telescope, result, body })
            },
            Decl::Print{tele: _, result, body} => {
                let result = self.inherit(result, &Term::UI)?;
                let body = self.inherit(body, &result)?;
                to_remove.iter()
                    .for_each(|t| { self.gamma.remove(t); });
                Ok(Def::Print { telescope, result, body })
            },
            //TODO: Decl::Cons => Err(e)
            Decl::Data{name, tele: _, cons} => {
                let name = DefVar {
                    core: None,
                    signature: Signature { is_data: true, telescope: telescope.clone(), result: Term::UI },
                    name: name.0.clone(),
                };
                let mut cons_ret = vec![];
                for c in cons {
                    let tele = self.telescope(&c.tele)?;
                    cons_ret.push(Def::Cons { name: c.name.0.clone(), owner: Box::new(name.clone()), tele: tele.0 })
                }
                Ok(Def::Data {
                    name: Box::new(name.clone()),
                    telescope,
                    cons: cons_ret,
                })
            },
        }
    }
    fn tyck_fun_body(
        &mut self,
        telescope: &[ParamTerm],
        result: &Term,
        clause_set: &[Clause<Expr>]
    ) -> Result<Vec<Clause<Term>>, Diagnostic> {
        let mut clause = vec![];
        for x in clause_set {
            clause.push(self.clause(telescope, result, x)?);
        }
        crate::tyck::classifier::classify(&clause, telescope)?;
        Ok(clause)
    }
    pub fn telescope(&mut self, tele: &[Param]) -> Result<(Vec<ParamTerm>, Vec<u32>), Error> {
        let mut ret = vec![];
        let mut ids = vec![];
        for param in tele {
            let ty = self.inherit(&param.1, &Term::UI)?;
            let len = self.name_id.len() as u32 + self.unnamed_num;
            let id = self.name_id.entry(param.0.0.to_owned())
                .or_insert(len);
            ret.push(ParamTerm {
                id: *id,
                ty: Box::new(ty.clone()),
                loc: param.0.1,
            });
            ids.push(*id);
            self.gamma.insert(*id, ty);//TODO: should be insert to gamma_temp
        }
        Ok((ret, ids))
    }
}

impl Elaborator {
    fn clause(&mut self, params: &[ParamTerm], result: &Term, clause: &Clause<Expr>) -> Result<Clause<Term>, Diagnostic> {
        for (ty, pat) in params.iter().zip(clause.0.iter()) {
            match pat {
                crate::parser::ast::Pattern::Id(_) => {
                    //TODO:
                },
                crate::parser::ast::Pattern::Pat(_, _) => {
                    //TODO
                },
            }
        }
        Ok(Clause(clause.0.clone(), self.inherit(&clause.1, result)?))
    }
}
