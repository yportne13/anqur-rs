use std::collections::HashMap;

use crate::syntax::term::Term;

use super::elaborator::LocalVar;

pub struct Normalizer(pub HashMap<LocalVar, Term>);

impl Normalizer {
    pub fn term(&self, term: &Term) -> Term {
        match term {
            Term::Error { msg } => todo!(),
            Term::Ref { var } => todo!(),
            Term::FnCall { fun, args } => todo!(),
            Term::DataCall { fun, args } => todo!(),
            Term::ConCall { fun, args } => todo!(),
            Term::Two { is_app, f, a } => todo!(),
            Term::Proj { t, is_one } => todo!(),
            Term::Lam { x, body } => todo!(),
            Term::DT { is_pi, param, cod } => todo!(),
            Term::UI => todo!(),
        }
    }
}
