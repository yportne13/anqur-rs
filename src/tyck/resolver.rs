use std::collections::HashMap;

use crate::{parser::ast::{Decl, Expr, Id, Param, FnBody, Clause, Pattern, ConsDecl}, Error, Diagnostic};

pub struct TeleCache {
    pub recover: Vec<Id>,
    pub remove: Vec<Id>,
}

impl TeleCache {
    pub fn add(&mut self, resolver: &mut Resolver, x: Id) {
        let put = resolver.env.insert(x.0.clone(), x.clone());
        match put {
            Some(_) => self.recover.push(x),
            None => self.remove.push(x),
        }
    }
    pub fn purge(&self, resolver: &mut Resolver) {
        self.remove.iter()
            .for_each(|x| { resolver.env.remove(&x.0); });
        self.recover.iter()
            .for_each(|x| { resolver.env.insert(x.0.clone(), x.clone()); })
    }
}

pub struct Resolver {
    pub env: HashMap<String, Id>,
}

impl Resolver {
    fn param(&mut self, param: &Param) -> Result<Param, Error> {
        Ok(Param(param.0.clone(), Box::new(self.expr(&param.1)?)))
    }
    pub fn def(&mut self, def: &Decl) -> Result<Decl, Error> {
        let teles = self.tele(def)?;
        match def {
            Decl::Def { name, tele: _, result, body } => {
                self.env.insert(name.0.clone(), name.clone());
                let result = self.expr(result)?;
                let ret = Decl::Def {
                    name: name.clone(),
                    tele: teles.0,
                    result,
                    body: match body {
                        crate::parser::ast::FnBody::Expr(e) => FnBody::Expr(self.expr(e)?),
                        crate::parser::ast::FnBody::Clause(c) => FnBody::Clause(
                            {
                                let mut ret = vec![];
                                for c in c {
                                    ret.push(self.clause(c)?);
                                }
                                ret
                            }
                        ),
                    }
                };
                teles.1.purge(self);
                Ok(ret)
            },
            Decl::Print { tele: _, result, body } => {
                let body = self.expr(body)?;
                let result = self.expr(result)?;
                teles.1.purge(self);
                Ok(Decl::Print { tele: teles.0, result, body })
            },
            Decl::Data { name, tele: _, cons } => {
                self.env.insert(name.0.clone(), name.clone());
                let mut ret_cons = vec![];
                for c in cons {
                    ret_cons.push(self.cons(c)?);
                }
                teles.1.purge(self);
                Ok(Decl::Data { name: name.clone(), tele: teles.0, cons: ret_cons })
            },
        }
    }
    fn cons(&mut self, cons: &ConsDecl) -> Result<ConsDecl, Error> {
        let tele = self.tele_cons(cons)?;
        tele.1.purge(self);
        self.env.insert(cons.name.0.clone(), cons.name.clone());
        Ok(ConsDecl { name: cons.name.clone(), tele: tele.0 })
    }
    fn tele(&mut self, def: &Decl) -> Result<(Vec<Param>, TeleCache), Error> {
        let mut cache = TeleCache {
            recover: vec![],
            remove: vec![],
        };
        let mut telescope = Vec::new();
        for param in def.tele() {
            telescope.push(Param(param.0.clone(), Box::new(self.expr(&param.1)?)));
            cache.add(self, param.0.clone());
        }
        Ok((telescope, cache))
    }
    fn tele_cons(&mut self, def: &ConsDecl) -> Result<(Vec<Param>, TeleCache), Error> {
        let mut cache = TeleCache {
            recover: vec![],
            remove: vec![],
        };
        let mut telescope = Vec::new();
        for param in def.tele.iter() {
            telescope.push(Param(param.0.clone(), Box::new(self.expr(&param.1)?)));
            cache.add(self, param.0.clone());
        }
        Ok((telescope, cache))
    }
    fn expr(&mut self, expr: &Expr) -> Result<Expr, Error> {
        match expr {
            Expr::Paren(x) => self.expr(x),
            Expr::Dt(is_pi, param, e) => {
                let param = self.param(param)?;
                self.env.insert(param.0.0.clone(), param.0.clone());
                let body = self.expr(e)?;
                self.env.remove(&param.0.0);
                Ok(Expr::Dt(
                    *is_pi,
                    Param(param.0.clone(), Box::new(self.expr(&param.1)?)),
                    Box::new(body),
                ))
            },
            Expr::Two(f, a) => Ok(Expr::Two(Box::new(self.expr(f)?), Box::new(self.expr(a)?))),
            Expr::Pair(f, a) => Ok(Expr::Pair(Box::new(self.expr(f)?), Box::new(self.expr(a)?))),
            Expr::Lam(x, a) => {
                self.env.insert(x.0.clone(), x.clone());
                let body = self.expr(a)?;
                self.env.remove(&x.0);
                Ok(Expr::Lam(x.clone(), Box::new(body)))
            },
            Expr::Univ => Ok(Expr::Univ),
            Expr::Ref(x) => {
                match self.env.get(&x.0) {
                    Some(new_id) => {
                        Ok(Expr::RefResolved(Id(new_id.0.clone(), x.1)))
                    },
                    None => {
                        Err(Diagnostic {
                            pos: x.1,
                            msg: format!("Unresolved: {:?}", x.0),
                        })
                    },
                }
            },
            Expr::RefResolved(x) => Ok(Expr::RefResolved(x.clone())),
            Expr::Fst(t) => Ok(Expr::Fst(Box::new(self.expr(t)?))),
            Expr::Snd(t) => Ok(Expr::Snd(Box::new(self.expr(t)?))),
        }
    }
    fn clause(&mut self, u: &Clause<Expr>) -> Result<Clause<Expr>, Error> {
        //TODO:
        Ok(Clause(u.0.clone(), self.expr(&u.1)?))
    }
    fn pattern(&mut self, u: Pattern, cache: &mut TeleCache) {
        let var = self.env.get(&u.name().0);
        if (var.is_none() && u.pats().is_empty()) {
            todo!()
        } else {
            todo!()
        }
    }
}
