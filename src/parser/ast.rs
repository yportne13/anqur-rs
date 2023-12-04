
#[derive(Debug, Clone)]
pub struct Param(pub Vec<Id>, pub Box<Expr>);

#[derive(Copy, Clone, Default, Debug, PartialEq)]
pub struct Locate {
    pub offset: usize,
    pub line: u32,
    pub len: usize,
}

/// decl
/// : 'def' ID param* ':' expr fnBody # fnDecl
/// | 'print' param* ':' expr ARROW2 expr # printDecl
/// | 'data' ID param* consDecl* # dataDecl
/// ;
#[derive(Debug, Clone)]
pub enum Decl {
    Def {
        name: Id,//TODO:DefVar
        tele: Vec<Param>,
        result: Expr,
        body: FnBody,
    },
    Print{
        tele: Vec<Param>,
        result: Expr,
        body: Expr,
    },
    Data{
        name: Id,//TODO:DefVar
        tele: Vec<Param>,
        cons: Vec<ConsDecl>,
    },
}

/// fnBody : ARROW2 expr | clause*;
#[derive(Debug, Clone)]
pub enum FnBody {
    Expr(Expr),
    Clause(Vec<Clause>),
}

/// pattern : ID | '(' ID pattern* ')';
#[derive(Debug, Clone)]
pub enum Pattern {
    Id(Id),
    Pat(Id, Vec<Pattern>),
}

/// clause : '|' pattern+ ARROW2 expr;
#[derive(Debug, Clone)]
pub struct Clause(pub Vec<Pattern>, pub Expr);

#[derive(Debug, Clone)]
pub struct ConsDecl {
    pub name: Id,
    pub tele: Vec<Param>
}

#[derive(Clone, Debug)]
pub struct Id(pub String, pub Locate);

#[derive(Debug, Clone)]
pub enum Expr {
    // Elimination lures
    Two(Box<Expr>, Box<Expr>),
    Fst(Box<Expr>),
    Snd(Box<Expr>),

    // Type formers
    Univ,
    Arrow(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
    Pi(Param, Box<Expr>),
    Sig(Param, Box<Expr>),

    // Introduction lures
    Lam(Vec<Id>, Box<Expr>),
    Pair(Box<Expr>, Box<Expr>),

    // Others
    Ref(Id),
    Paren(Box<Expr>),
}
