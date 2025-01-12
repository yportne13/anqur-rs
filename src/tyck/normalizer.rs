/*use std::collections::HashMap;

use crate::syntax::term::{ParamTerm, Term};

//use super::elaborator::LocalVar;

pub struct Normalizer(pub HashMap<LocalVar, Term>);

impl Normalizer {
    pub fn term(&mut self, term: &Term) -> Term {
        match term {
            Term::Ref { var } => {
                self.0.get(var)
                    .map(|x| Renamer(HashMap::new()).term(x))
                    .map(|x| self.term(&x))
                    .unwrap_or(Term::Ref { var: *var })
            },
            Term::UI => Term::UI,
            Term::Lam { x, body } => {
                Term::Lam { x: *x, body: Box::new(self.term(body)) }
            },
            Term::DT { is_pi, param, cod } => {
                Term::DT { is_pi: *is_pi, param: self.param(param), cod: Box::new(self.term(cod)) }
            },
            Term::Two { is_app, f, a } => {
                let f = self.term(f);
                let a = self.term(a);
                match &f {
                    Term::Lam { x, body } => {
                        if *is_app {
                            Term::Two { is_app: *is_app, f: Box::new(f), a: Box::new(a) }
                        } else {
                            self.0.insert(*x, a);
                            let body = self.term(body);
                            self.0.remove(x);
                            body
                        }
                    },
                    _ => {
                        Term::Two { is_app: *is_app, f: Box::new(f), a: Box::new(a) }
                    }
                }
            },
            Term::Proj { t, is_one } => {
                let t = self.term(t);
                match t {
                    Term::Two { is_app, f, a } => {
                        assert!(!is_app);
                        if *is_one {
                            *f.clone()
                        } else {
                            *a.clone()
                        }
                    },
                    _ => {
                        Term::Proj { t: Box::new(t), is_one: *is_one }
                    }
                }
            },
            Term::FnCall { fun, args } => {
                Term::FnCall {
                    fun: fun.clone(),
                    args: args.iter()
                        .map(|x| self.term(x))
                        .collect()
                }
            },
            Term::ConCall { fun, args } => {
                Term::ConCall {
                    fun: fun.clone(),
                    args: args.iter()
                        .map(|x| self.term(x))
                        .collect()
                }
            },
            Term::DataCall { fun, args } => {
                Term::DataCall {
                    fun: fun.clone(),
                    args: args.iter()
                        .map(|x| self.term(x))
                        .collect(),
                }
            },
            Term::Error { msg } => Term::Error { msg: msg.clone() },
        }
    }
    fn param(&mut self, param: &ParamTerm) -> ParamTerm {
        ParamTerm {
            id: param.id,
            ty: Box::new(self.term(&param.ty)),
            loc: param.loc,
        }
    }
}

pub struct Renamer(pub HashMap<LocalVar, LocalVar>);

impl Renamer {
    pub fn term(&mut self, term: &Term) -> Term {
        match term {
            Term::Lam { x, body } => {
                Term::Lam { x: self.paramvar(*x), body: Box::new(self.term(body)) }
            },
            Term::UI => Term::UI,
            Term::Ref { var } => Term::Ref { var: *self.0.get(var).unwrap_or(var) },
            Term::DT { is_pi, param, cod } => {
                Term::DT { is_pi: *is_pi, param: self.param(param), cod: Box::new(self.term(cod)) }
            },
            Term::Two { is_app, f, a } => {
                Term::Two { is_app: *is_app, f: Box::new(self.term(f)), a: Box::new(self.term(a)) }
            },
            Term::Proj { t, is_one } => {
                Term::Proj { t: Box::new(self.term(t)), is_one: *is_one }
            },
            Term::FnCall { fun, args } => {
                Term::FnCall { fun: fun.clone(), args: args.iter().map(|x| self.term(x)).collect() }
            },
            Term::DataCall { fun, args } => {
                Term::DataCall { fun: fun.clone(), args: args.iter().map(|x| self.term(x)).collect() }
            },
            Term::ConCall { fun, args } => {
                Term::ConCall { fun: fun.clone(), args: args.iter().map(|x| self.term(x)).collect() }
            },
            Term::Error { msg } => Term::Error { msg: msg.clone() },
        }
    }
    fn param(&mut self, param: &ParamTerm) -> ParamTerm {
        ParamTerm {
            id: param.id,
            ty: Box::new(self.term(&param.ty)),
            loc: param.loc,
        }
    }
    fn paramvar(&mut self, id: LocalVar) -> LocalVar {
        let ret = LocalVar {
            id: id.id + 114514,
            name: id.name,
        };
        self.0.insert(id, ret);
        ret
    }
}
*/