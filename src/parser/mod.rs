
pub mod ast;

use nom::{bytes::complete::*, combinator::*, error::{ErrorKind, make_error}, character::complete::multispace0, branch::alt, multi::{many1, many0}, sequence::tuple, Err};
use nom_greedyerror::GreedyError;

use self::ast::{Locate, Id, Expr, Param, ConsDecl, Pattern, Clause, FnBody, Decl};


pub type Span<'a> = nom_locate::LocatedSpan<&'a str>;
pub type IResult<'a, T, U> = nom::IResult<T, U, GreedyError<T, ErrorKind>>;

pub fn parser(s: &str) -> Result<Vec<Decl>, String> {
    match program(Span::new(s)) {
        Ok(x) => if x.0.fragment().is_empty() {
            Ok(x.1)
        } else {
            Err(format!("parse not finish at {}", x.0.fragment()))
        },
        Err(e) => Err(format!("{:?}", e)),
    }
}

/// program : decl+;
fn program(s: Span) -> IResult<Span, Vec<Decl>> {
    many1(decl)(s)
}

/// decl
/// : 'def' ID param* ':' expr fnBody # fnDecl
/// | 'print' param* ':' expr ARROW2 expr # printDecl
/// | 'data' ID param* consDecl* # dataDecl
/// ;
fn decl(s: Span) -> IResult<Span, Decl> {
    alt((
        map(
            tuple((ws(tag("def")), id, many0(param), ws(tag(":")), expr, fn_body)),
            |(_, name, tele, _, result, body)| Decl::Def{name, tele, result, body}
        ),
        map(
            tuple((ws(tag("print")), many0(param), ws(tag(":")), expr, arrow2, expr)),
            |(_, tele, _, result, _, body)| Decl::Print{tele, result, body},
        ),
        map(
            tuple((ws(tag("data")), id, many0(param), many0(cons_decl))),
            |(_, name, tele, cons)| Decl::Data{name, tele, cons}
        )
    ))(s)
}

/// fnBody : ARROW2 expr | clause*;
fn fn_body(s: Span) -> IResult<Span, FnBody> {
    alt((
        map(tuple((arrow2, expr)), |(_, expr)| FnBody::Expr(expr)),
        map(many0(clause), FnBody::Clause),
    ))(s)
}

/// pattern : ID | '(' ID pattern* ')';
fn pattern(s: Span) -> IResult<Span, Pattern> {
    alt((
        map(id, Pattern::Id),
        map(tuple((
            ws(tag("(")),
            id,
            many0(pattern),
            ws(tag(")"))
        )), |(_, id, patterns, _)| Pattern::Pat(id, patterns))
    ))(s)
}

/// clause : '|' pattern+ ARROW2 expr;
fn clause(s: Span) -> IResult<Span, Clause> {
    let (s, _) = ws(tag("|"))(s)?;
    let (s, patterns) = many1(pattern)(s)?;
    let (s, _) = arrow2(s)?;
    let (s, expr) = expr(s)?;
    Ok((s, Clause(patterns, expr)))
}

fn cons_decl(s: Span) -> IResult<Span, ConsDecl> {
    map(tuple((
        ws(tag("|")),
        id,
        many0(param),
    )), |(_, name, tele)| ConsDecl{name, tele})(s)
}

fn expr(s: Span) -> IResult<Span, Expr> {
    alt((
        map(tuple((expr_core, expr)), |(e0, e1)| Expr::Two(Box::new(e0), Box::new(e1))),
        map(tuple((expr_core, ws(tag(".1")))), |(expr, _)| Expr::Fst(Box::new(expr))),
        map(tuple((expr_core, ws(tag(".2")))), |(expr, _)| Expr::Snd(Box::new(expr))),
        map(tuple((expr_core, arrow, expr)), |(e0, _, e1)| Expr::Arrow(Box::new(e0), Box::new(e1))),
        map(tuple((expr_core, times, expr)), |(e0, _, e1)| Expr::Times(Box::new(e0), Box::new(e1))),
        expr_core,
        /*map(many1(tuple((arrow, expr_core))), |v| ),
        map(many0(tuple((times, expr_core))), |v| if v.is_empty() {
            e0
        } else {
            v.into_iter()

        })*/
    ))(s)
}

pub fn expr_core(s: Span) -> IResult<Span, Expr> {
    alt((
        map(ws(tag("U")), |_| Expr::Univ),
        map(ws(tag("Type")), |_| Expr::Univ),
        map(tuple((pi, param, arrow, expr)), |(_, param, _, expr)| Expr::Pi(param, Box::new(expr))),
        map(tuple((sig, param, arrow, expr)), |(_, param, _, expr)| Expr::Sig(param, Box::new(expr))),
        map(tuple((lam, many1(id), ws(tag(".")), expr)), |(_, param, _, expr)| Expr::Lam(param, Box::new(expr))),
        map(tuple((ws(tag("<<")), expr, ws(tag(",")), expr, ws(tag(">>")))), |(_, e0, _, e1, _)| Expr::Pair(Box::new(e0), Box::new(e1))),
        map(id, Expr::Ref),
        map(tuple((ws(tag("(")), expr, ws(tag(")")))), |(_, expr, _)| Expr::Paren(Box::new(expr))),
    ))(s)
}

fn arrow(s: Span) -> IResult<Span, ()> {
    let (s, _) = ws(alt((tag("->"), tag("\u{2192}"))))(s)?;
    Ok((s, ()))
}

fn arrow2(s: Span) -> IResult<Span, ()> {
    let (s, _) = ws(alt((tag("=>"), tag("\u{21D2}"))))(s)?;
    Ok((s, ()))
}

fn times(s: Span) -> IResult<Span, ()> {
    let (s, _) = ws(alt((tag("**"), tag("\u{00D7}"))))(s)?;
    Ok((s, ()))
}

fn sig(s: Span) -> IResult<Span, ()> {
    let (s, _) = ws(alt((tag("Sig"), tag("\u{03A3}"))))(s)?;
    Ok((s, ()))
}

fn lam(s: Span) -> IResult<Span, ()> {
    let (s, _) = ws(alt((tag("\\\\"), tag("\u{03BB}"))))(s)?;
    Ok((s, ()))
}

fn pi(s: Span) -> IResult<Span, ()> {
    let (s, _) = ws(alt((tag("Pi"), tag("\u{03A0}"))))(s)?;
    Ok((s, ()))
}

fn param(s: Span) -> IResult<Span, Param> {
    let (s, _) = ws(tag("("))(s)?;
    let (s, x) = many1(id)(s)?;
    let (s, _) = ws(tag(":"))(s)?;
    let (s, expr) = expr(s)?;
    let (s, _) = ws(tag(")"))(s)?;
    Ok((s, Param(x, Box::new(expr))))
}

pub fn id(s: Span) -> IResult<Span, Id> {
    //TODO: '-'
    let (s, a) = is_a("[~!@#$%^&*+=<>?/|abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")(s)?;
    let (s, b) = opt(is_a("[~!@#$%^&*+'-=<>?/|abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))(s)?;
    let (s, _) = multispace0(s)?;
    let a = if let Some(b) = b {
        concat(a, b).unwrap()
    } else {
        a
    };
    if matches!(a.fragment(), &"=>" | &"->" | &"**" | &"|" | &"Pi" | &"Sig" | &"def" | &"data" | &"print" | &"U" | &"Type") {
        Err(Err::Error(make_error(s, ErrorKind::Fix)))
    } else {
        Ok((s, Id(a.fragment().to_string(), into_locate(a))))
    }
}

pub(crate) fn ws<'a, O, F>(
    mut f: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O>
where
    F: FnMut(Span<'a>) -> IResult<Span<'a>, O>,
{
    move |s: Span<'a>| {
        let (s, x) = f(s)?;
        let (s, _) = multispace0(s)?;
        Ok((s, x))
    }
}

pub(crate) fn concat<'a>(a: Span<'a>, b: Span<'a>) -> Option<Span<'a>> {
    let c = unsafe { str_concat::concat(a.fragment(), b.fragment()) };
    if let Ok(c) = c {
        let ret = unsafe {
            Span::new_from_raw_offset(a.location_offset(), a.location_line(), c, ())
        };
        Some(ret)
    } else {
        None
    }
}

pub(crate) fn into_locate(s: Span) -> Locate {
    Locate {
        offset: s.location_offset(),
        line: s.location_line(),
        len: s.fragment().len(),
    }
}

#[test]
fn test() {
    let s = r"def uncurry (A B C : U)
        (t : A ** B) (f : A -> B -> C) : C => f (t.1) (t.2)
      def uncurry' (A : U) (t : A ** A) (f : A -> A -> A) : A => uncurry A A A t f";
    println!("{}", parser(s).is_ok());
    let s = r"def Eq (A : U) (a b : A) : U => Pi (P : A -> U) -> P a -> P b
      def refl (A : U) (a : A) : Eq A a a => \\P pa. pa
      def sym (A : U) (a b : A) (e : Eq A a b) : Eq A b a =>
          e (\\b. Eq A b a) (refl A a)";
    println!("{}", parser(s).is_ok());
    let s = r"data Unit | unit
      def unnit : Unit => unit
      data Nat
      | zero
      | succ (n : Nat)

      def two : Nat => succ (succ zero)
      print : Nat => two

      data List (A : U)
      | nil
      | cons (x : A) (xs : List A)

      def lengthTwo (A : U) (a : A) : List A => cons A a (cons A a (nil A))
      print : List Nat => lengthTwo Nat two";
    println!("{}", parser(s).is_ok());
    let s = r"data Nat
      | zero
      | succ (n : Nat)

      def plus (a : Nat) (b : Nat) : Nat
      | zero b => b
      | (succ a) b => succ (plus a b)

      def two : Nat => succ (succ zero)
      def four : Nat => plus two two
      def six : Nat => plus four two
      print : Nat => six";
    println!("{}", parser(s).is_ok());
    let s = r"data Nat
      | zero
      | succ (n : Nat)
      def plus-bad (a : Nat) (b : Nat) : Nat
      | (succ a) b => succ (plus-bad a b)";
    println!("{}", parser(s).is_ok());
}
