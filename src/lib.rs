use std::{collections::HashMap, fmt::Debug};

use parser::{ast::Locate, parser};

use crate::tyck::{elaborator::Elaborator, resolver::Resolver};

pub mod parser;
pub mod syntax;
pub mod util;
pub mod tyck;
mod locally_nameless;

type Error = Diagnostic;

#[derive(Clone)]
pub struct Diagnostic {
    pos: Locate,
    msg: String,
}

impl Debug for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n -> {} {}", self.msg, self.pos.line, self.pos.len)
    }
}

fn main() {
    println!("Hello, world!");
}

fn run(s: &str) {
    let mut elaborator = Elaborator {
        name_id: HashMap::new(),
        unnamed_num: 0,
        sigma: HashMap::new(),
        gamma: HashMap::new(),
    };
    let decl = parser(s).unwrap();
    let mut edj = Resolver { env: HashMap::new() };
    let mut resolve_decl = vec![];
    for d in decl {
        resolve_decl.push(edj.def(&d).unwrap());
    }
    //println!("{:#?}", resolve_decl);
    for mut dec in resolve_decl {
        let x = elaborator.def(&mut dec).unwrap();
        match &x {
            syntax::def::Def::Fn { name, telescope, result: _, body: _ }
            | syntax::def::Def::Data { name, telescope, cons: _ } => {
                elaborator.sigma.insert(name.name.clone(), x);
            },
            syntax::def::Def::Cons { name, owner, tele } => todo!(),
            syntax::def::Def::Print { telescope, result, body } => {
                println!("{:?}", x);
            },
        }
        elaborator.unnamed_num = 0;
        elaborator.name_id.clear();
        elaborator.gamma.clear();
    }
    //println!("{:?}", elaborator)
}

#[test]
fn test() {
    let s = r"def uncurry (A B C : U)
        (t : A ** B) (f : A -> B -> C) : C => f (t.1) (t.2)
      def uncurry' (D : U) (t : D ** D) (f : D -> D -> D) : D => uncurry D D D t f";
    run(s);
    println!("finish 1");
    let s = r"def Eq (A : U) (a b : A) : U => Pi (P : A -> U) -> P a -> P b
      def refl (A : U) (a : A) : Eq A a a => \\P pa. pa
      ";
    run(s);
    println!("finish 2");
    /* def sym (A : U) (a b : A) (e : Eq A a b) : Eq A b a =>
          e (\\b. Eq A b a) (refl A a)*/
    /*let s = r"data Unit | unit
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
    run(s);
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
    run(s);
    let s = r"data Nat
      | zero
      | succ (n : Nat)
      def plus-bad (a : Nat) (b : Nat) : Nat
      | (succ a) b => succ (plus-bad a b)";
    run(s);*/
}
