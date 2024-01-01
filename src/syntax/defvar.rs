use crate::parser::ast::Id;

use super::def::{Def, Signature};



#[derive(Clone, Debug, PartialEq)]
pub struct DefVar {
    pub core: Option<Def>,
    pub signature: Signature,
    pub name: String,
}
