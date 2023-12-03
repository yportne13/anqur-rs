use crate::parser::ast::Param;

use super::term::Term;



pub enum Def {
    Fn {
        name: String,
        telescope: Vec<Param>,
        result: Term,
        //body: Term | pat
    },
    Data {
        name: String,
        telescope: Vec<Param>,
        result: Term,
    },
    Cons {
        name: String,
        //owner: 
        tele: Vec<Param>,
    },
    Print {
        telescope: Vec<Param>,
        result: Term,
        body: Term,
    }
}

pub struct Signature {
    is_data: bool,
    telescope: Vec<Param>,
    result: Term,
}
