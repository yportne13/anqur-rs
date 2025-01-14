use std::fmt::Debug;

#[derive(Debug, Clone, PartialEq)]
pub struct Param<T>(pub Id<String>, pub Box<Expr<T>>);

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
pub enum Decl<T> {
    Def {
        name: Id<String>,
        tele: Vec<Param<T>>,
        result: Expr<T>,
        body: Expr<T>,
    },
    Print {
        tele: Vec<Param<T>>,
        result: Expr<T>,
        body: Expr<T>,
    },
    Data {
        name: Id<String>,
        tele: Vec<Param<T>>,
        cons: Vec<ConsDecl<T>>,
    },
}

impl<T> Decl<T> {
    pub fn tele(&self) -> &[Param<T>] {
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
pub enum FnBody<T> {
    Expr(Expr<T>),
    Clause(Vec<Clause<T, Expr<T>>>),
}

/// pattern : ID | '(' ID pattern* ')';
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern<T> {
    Id(Id<T>),
    Pat(Id<T>, Vec<Pattern<T>>),
}

impl<T> Pattern<T> {
    pub fn name(&self) -> &Id<T> {
        match self {
            Pattern::Id(x) => x,
            Pattern::Pat(x, _) => x,
        }
    }
    pub fn pats(&self) -> &[Pattern<T>] {
        match self {
            Pattern::Id(_) => &[],
            Pattern::Pat(_, x) => x,
        }
    }
}

/// clause : '|' pattern+ ARROW2 expr;
#[derive(Debug, Clone, PartialEq)]
pub struct Clause<T, U>(pub Vec<Pattern<T>>, pub U);

#[derive(Debug, Clone)]
pub struct ConsDecl<T> {
    pub name: Id<T>,
    pub tele: Vec<Param<T>>,
}

#[derive(Clone, Copy, PartialEq)]
pub struct Id<T>(pub T, pub Locate);

impl<T: Debug> Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Id \"{:?}\" @ offset {}, line {}, len: {}", self.0, self.1.offset, self.1.line, self.1.len)
    }
}

/*#[derive(Debug, Clone, PartialEq)]
pub enum LocalVar {
    Local(u32),
    Free(String),
}*/

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<T> {
    // Elimination lures
    Two(Box<Expr<T>>, Box<Expr<T>>),
    Fst(Box<Expr<T>>),
    Snd(Box<Expr<T>>),

    // Type formers
    Univ,
    Dt(bool, Param<T>, Box<Expr<T>>),
    //Pi(Param<T>, Box<Expr<T>>),
    //Sig(Param<T>, Box<Expr<T>>),

    Pair(Box<Expr<T>>, Box<Expr<T>>),

    // Introduction lures
    Lam(Id<String>, Box<Expr<T>>),
    App(Box<Expr<T>>, Box<Expr<T>>),

    // Others
    Ref(Id<T>),
    Paren(Box<Expr<T>>),

    Match(Box<Expr<T>>, Vec<Clause<T, Expr<T>>>),

    Let(Id<String>, Box<Expr<T>>, Box<Expr<T>>),
}

impl<T> Expr<T> {
    pub fn pos(&self) -> Locate {
        match self {
            Expr::Two(a, b) => a.pos() + b.pos(),
            Expr::Fst(a) => a.pos(),
            Expr::Snd(a) => a.pos(),
            Expr::Univ => Locate::default(),//TODO:
            Expr::Dt(_, p, _) => p.0.1,
            Expr::Pair(a, b) => a.pos() + b.pos(),
            Expr::Lam(id, _) => id.1,
            Expr::App(a, b) => a.pos() + b.pos(),
            Expr::Ref(id) => id.1,
            Expr::Paren(a) => a.pos(),
            Expr::Match(a, b) => a.pos() + b.last().unwrap().1.pos(),
            Expr::Let(name, _, body) => name.1 + body.pos(),
        }
    }
}
