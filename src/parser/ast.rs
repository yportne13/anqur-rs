use std::fmt::Debug;


#[derive(Debug, Clone, PartialEq)]
pub struct Param(pub Id, pub Box<Expr>);

#[derive(Copy, Clone, Default, Debug, PartialEq)]
pub struct Locate {
    pub offset: usize,
    pub line: u32,
    pub len: usize,
}

impl std::ops::Add for Locate {
    type Output = Locate;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            offset: self.offset,
            line: self.line,
            len: rhs.offset + rhs.len - self.offset,
        }
    }
}

/// decl
/// : 'def' ID param* ':' expr fnBody # fnDecl
/// | 'print' param* ':' expr ARROW2 expr # printDecl
/// | 'data' ID param* consDecl* # dataDecl
/// ;
#[derive(Debug, Clone)]
pub enum Decl {
    Def {
        name: Id,
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
        name: Id,
        tele: Vec<Param>,
        cons: Vec<ConsDecl>,
    },
}

impl Decl {
    pub fn tele(&self) -> &[Param] {
        match self {
            Decl::Def { name: _, tele, result: _, body: _ }
                | Decl::Print { tele, result: _, body: _ }
                | Decl::Data { name: _, tele, cons: _ } => {
                    tele
                },
        }
    }
}

/// fnBody : ARROW2 expr | clause*;
#[derive(Debug, Clone)]
pub enum FnBody {
    Expr(Expr),
    Clause(Vec<Clause<Expr>>),
}

/// pattern : ID | '(' ID pattern* ')';
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Id(Id),
    Pat(Id, Vec<Pattern>),
}

impl Pattern {
    pub fn name(&self) -> &Id {
        match self {
            Pattern::Id(x) => x,
            Pattern::Pat(x, _) => x,
        }
    }
    pub fn pats(&self) -> &[Pattern] {
        match self {
            Pattern::Id(_) => &[],
            Pattern::Pat(_, x) => x,
        }
    }
}

/// clause : '|' pattern+ ARROW2 expr;
#[derive(Debug, Clone, PartialEq)]
pub struct Clause<T>(pub Vec<Pattern>, pub T);

#[derive(Debug, Clone)]
pub struct ConsDecl {
    pub name: Id,
    pub tele: Vec<Param>
}

#[derive(Clone, PartialEq)]
pub struct Id(pub String, pub Locate);

impl Debug for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Id \"{}\" @ offset {}, line {}, len: {}", self.0, self.1.offset, self.1.line, self.1.len)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Elimination lures
    Two(Box<Expr>, Box<Expr>),
    Fst(Box<Expr>),
    Snd(Box<Expr>),

    // Type formers
    Univ,
    Dt(bool, Param, Box<Expr>),
    //Pi(Param, Box<Expr>),
    //Sig(Param, Box<Expr>),

    // Introduction lures
    Lam(Id, Box<Expr>),
    Pair(Box<Expr>, Box<Expr>),

    // Others
    Ref(Id),
    RefResolved(Id),
    Paren(Box<Expr>),
}

impl Expr {
    pub fn pos(&self) -> Locate {
        match self {
            Expr::Two(a, b) => a.pos() + b.pos(),
            Expr::Fst(a) => a.pos(),
            Expr::Snd(a) => a.pos(),
            Expr::Univ => Locate::default(),//TODO:
            Expr::Dt(_, p, _) => p.0.1,
            Expr::Lam(id, _) => id.1,
            Expr::Pair(a, b) => a.pos() + b.pos(),
            Expr::Ref(id) => id.1,
            Expr::RefResolved(id) => id.1,
            Expr::Paren(a) => a.pos(),
        }
    }
}
