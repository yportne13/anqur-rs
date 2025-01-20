use std::{collections::HashMap, ops::Add};

use colored::Colorize;

use crate::parser::ast::*;

use super::list::List;

type Lvl = usize;

#[derive(Debug, Clone, Default)]
pub enum Value {
    Lvl(Id<usize>),
    App(Box<Value>, Box<Value>),
    Lam(Id<String>, Closure),
    Pi(Id<String>, Box<Value>, Closure),
    Sig(Box<Value>, Box<Value>),
    #[default]
    U,
    Two(Box<Value>, Box<Value>),
    Tuple(Box<Value>, Box<Value>),
    Fst(Box<Value>),
    Snd(Box<Value>),
}

#[derive(Debug, Clone)]
pub struct Closure (List<Value>, Expr<Lvl>);

impl Closure {
    fn apply(self, arg: Value) -> Value {
        let mut new_env = self.0;
        new_env = new_env.prepend(arg);
        eval(new_env, self.1)
    }
}

/// eval env tm =
///      match tm with
///      | Idx idx   -> List.nth env idx
///      | Lam tm'   -> VLam(env, tm')
///      | App(f, a) -> apply_val (eval env f) (eval env a)
fn eval(env: List<Value>, tm: Expr<Lvl>) -> Value {
    //println!("  {} on {:?} at\n  {:?}", "eval".red(), tm, env);
    match tm {
        Expr::Ref(idx) => env.iter().nth(idx.0).expect(&format!("unexpected error: get ref {} failed", idx.0)).clone(),
        Expr::App(f, a) | Expr::Two(f, a) => match (eval(env.clone(), *f), eval(env, *a)) {
            (Value::Lam(_, body), va) => eval(body.0.prepend(va), body.1),
            (vf, va) => Value::App(Box::new(vf), Box::new(va)),
        },
        Expr::Lam(name, t) => Value::Lam(name, Closure(env, *t)),
        Expr::Dt(is_pi, Param(name, term), term1) => if is_pi {
            Value::Pi(name, Box::new(eval(env.clone(), *term)), Closure(env, *term1))
        } else {
            Value::Sig(Box::new(eval(env.clone(), *term)), Box::new(eval(env, *term1)))
        },
        Expr::Pair(a, b) => {
            Value::Tuple(Box::new(eval(env.clone(), *a)), Box::new(eval(env, *b)))
        },
        Expr::Let(_, _, term1) => eval(env.clone(), *term1),
        Expr::Univ => Value::U,
        Expr::Fst(expr) => Value::Fst(Box::new(eval(env, *expr))),
        Expr::Snd(expr) => Value::Snd(Box::new(eval(env, *expr))),
        Expr::Paren(expr) => eval(env, *expr),
        Expr::Match(expr, vec) => todo!(),
    }
}

fn quote(level: usize, value: Value) -> Expr<usize> {
    match value {
        Value::Lvl(lvl) => Expr::Ref(Id(level - lvl.0 - 1, lvl.1)),
        Value::Lam(name, body) => Expr::Lam(
            name,
            Box::new(
                quote(
                    level + 1,
                    eval(body.0.prepend(Value::Lvl(Id(level, Locate {
                        offset: 0,
                        line: 0,
                        len: 0//TODO:
                    }))), body.1)
                )
            )
        ),
        Value::App(vf, va) => Expr::App(
            Box::new(quote(level, *vf)),
            Box::new(quote(level, *va))
        ),
        Value::Pi(name, value, closure) => todo!(),/*Expr::Dt(
            name,
            Box::new(quote(level, *value)),
            Box::new(
                quote(
                    level + 1,
                    eval(closure.0.prepend(Value::Lvl(level)), closure.1)
                )
            )
        ),*/
        Value::Sig(a, b) => todo!(),
        Value::U => Expr::Univ,
        Value::Two(a, b) => Expr::Two(
            Box::new(quote(level, *a)),
            Box::new(quote(level, *b))
        ),
        Value::Tuple(a, b) => Expr::Pair(
            Box::new(quote(level, *a)),
            Box::new(quote(level, *b)),
        ),
        Value::Fst(a) => Expr::Fst(Box::new(quote(level, *a))),
        Value::Snd(a) => Expr::Snd(Box::new(quote(level, *a))),
    }
}

fn normalize_expr(t: Expr<usize>) -> Expr<usize> {
    quote(0, eval(List::new(), t))
}

pub fn normalize(t: Decl<usize>) -> Decl<usize> {
    match t {
        Decl::Def { name, tele, result, body } => Decl::Def {
            name,
            tele,
            result: normalize_expr(result),
            body: normalize_expr(body),
        },
        Decl::Print { tele, result, body } => Decl::Print {
            tele,
            result: normalize_expr(result),
            body: normalize_expr(body),
        },
        Decl::Data { name, tele, cons } => todo!(),
    }
}

// Elaboration context
#[derive(Debug, Clone)]
pub struct Cxt {
    env: List<Value>,
    types: Vec<(String, Value)>,
    lvl: usize,
    pos: usize, // Simplified SourcePos for error reporting
}

impl Cxt {
    pub fn empty(pos: usize) -> Self {
        Cxt {
            env: Default::default(),
            types: vec![],
            lvl: 0,
            pos,
        }
    }

    // Extend Cxt with a bound variable
    pub fn bind(&self, x: Id<String>, a: Value) -> Self {
        let mut env = self.env.clone();
        env = env.prepend(Value::Lvl(Id(self.lvl, x.1)));
        let mut types = self.types.clone();
        types.push((x.0, a));
        Cxt {
            env,
            types,
            lvl: self.lvl + 1,
            pos: self.pos,
        }
    }

    // Extend Cxt with a definition
    pub fn define(&self, x: String, t: Value, a: Value) -> Self {
        let mut types = self.types.clone();
        types.push((x, a));
        Cxt {
            env: self.env.prepend(t),
            types,
            lvl: self.lvl + 1,
            pos: self.pos,
        }
    }
}

pub fn infer(cxt: &Cxt, t: &Decl<String>) -> Result<(Decl<Lvl>, Value, Cxt), (String, usize)> {
    match t {
        Decl::Def { name, tele, result, body } => {
            let mut ret_cxt = cxt.clone();
            let mut cxt = cxt.clone();
            let param: Vec<Param<usize>> = tele.iter().map(|x| {
                    //println!(">>>> {}", x.0.0);
                    //println!("<<<< {:?}", infer_expr(&cxt, &x.1));
                    let (typ, _) = infer_expr(&cxt, &x.1)?;
                    let vtyp = eval(cxt.env.clone(), typ.clone());
                    //let typ_tm = check(&cxt, &x.1, &Value::U)?;
                    //let vtyp = eval(cxt.env.clone(), typ_tm.clone());
                    //println!("tele: {} {:?} -->> {:?}\n\n", x.0.0, x.1, vtyp);
                    //cxt = cxt.define(x.0.0.clone(), vtyp.clone(), vtyp);//TODO: last param may not vtyp
                    cxt = cxt.bind(x.0.clone(), vtyp);//TODO: last param may not vtyp
                    Ok(Param(x.0.clone(), Box::new(typ)))
                }).collect::<Result<_, _>>()?;
            let result_u = check(&cxt, &result, &Value::U)?;
            let ret_val = eval(cxt.env.clone(), result_u.clone());
            let body_u = check(&cxt, body, &ret_val)?;
            let vt = eval(cxt.env.clone(), body_u.clone());
            //tele.iter().for_each(|x| cxt = cxt.bind(x.0, x.1));
            let typ = tele.iter().rev()
                .fold(result.clone(), |a, b| {
                    Expr::Dt(true, b.clone(), Box::new(a))
                });
            let bod = tele.iter().rev()
                .fold(body.clone(), |a, b| {
                    Expr::Lam(b.0.clone(), Box::new(a))
                });
            {
                let typ_tm = check(&ret_cxt, &typ, &Value::U)?;
                let vtyp = eval(ret_cxt.env.clone(), typ_tm.clone());
                //println!("------------------->");
                //println!("{:?}", vtyp);
                //println!("-------------------<");
                let t_tm = check(&ret_cxt, &bod, &vtyp)?;
                //println!("begin vt {}", "------".green());
                let vt = eval(ret_cxt.env.clone(), t_tm.clone());
                ret_cxt = ret_cxt.define(name.0.clone(), vt, vtyp);
            }
            Ok((Decl::Def {
                name: name.clone(),
                tele: param,
                result: result_u,
                body: body_u,
            }, vt, ret_cxt))//TODO:vt may be wrong
        },
        Decl::Print { tele, result, body } => todo!(),
        Decl::Data { name, tele, cons } => todo!(),
    }
}

fn infer_expr(cxt: &Cxt, t: &Expr<String>) -> Result<(Expr<Lvl>, Value), (String, usize)> {
    match t {
        Expr::Ref(x) => {
            for (i, (x_, a)) in cxt.types.iter().rev().enumerate() {
                if &x.0 == x_ {
                    //println!("-----------------{}: {}  -- {:?}", x.0, i, a);
                    return Ok((Expr::Ref(Id(i, x.1)), a.clone()));
                }
            }
            report(cxt, &format!("variable out of scope: {}", x.0))
        }

        Expr::Univ => Ok((Expr::Univ, Value::U)), // U : U rule

        Expr::App(t, u) => {
            let (t_tm, tty) = infer_expr(cxt, t)?;
            match tty {
                Value::Pi(_, a, b) => {
                    let u_tm = check(cxt, u, &a)?;
                    let u_val = eval(cxt.env.clone(), u_tm.clone());
                    Ok((Expr::App(Box::new(t_tm), Box::new(u_tm)), b.apply(u_val)))
                }
                _ => report(cxt, "Expected a function type"),
            }
        }

        Expr::Lam { .. } => report(cxt, "Can't infer type for lambda expression"),

        Expr::Dt(is_pi, Param(name, expr), b) => if *is_pi {
            let a_tm = check(cxt, expr, &Value::U)?;
            let va = eval(cxt.env.clone(), a_tm.clone());
            //let new_cxt = if name.0 != "_" {cxt.bind(name.clone(), va)} else {cxt.clone()};
            let new_cxt = cxt.bind(name.clone(), va);
            let b_tm = check(&new_cxt, b, &Value::U)?;
            //println!("{:?}  {:?}", b, b_tm);
            Ok((Expr::Dt(*is_pi, Param(name.clone(), Box::new(a_tm)), Box::new(b_tm)), Value::U))
        } else {
            //TODO:
            let (expr_tm, expr_ty) = infer_expr(cxt, expr)?;
            let (expr1_tm, expr1_ty) = infer_expr(cxt, b)?;
            Ok((
                Expr::Dt(*is_pi, Param(name.clone(), Box::new(expr_tm)), Box::new(expr1_tm)),
                Value::U,
            ))
        },
        Expr::Pair(a, b) => {
            let (expr_tm, expr_ty) = infer_expr(cxt, a)?;
            let (expr1_tm, expr1_ty) = infer_expr(cxt, b)?;
            Ok((
                Expr::Pair(Box::new(expr_tm), Box::new(expr1_tm)),
                Value::Tuple(Box::new(expr_ty), Box::new(expr1_ty)),
            ))
        },
        Expr::Two(t, u) => {
            let (t_tm, tty) = infer_expr(cxt, t)?;
            match tty {
                Value::Pi(_, a, b) => {
                    let u_tm = check(cxt, u, &a)?;
                    let u_val = eval(cxt.env.clone(), u_tm.clone());
                    Ok((Expr::Two(Box::new(t_tm), Box::new(u_tm)), b.apply(u_val)))
                }
                _ => report(cxt, &format!("Expected a function type but get {:?}:{:?}", t, tty)),
            }
        },
        Expr::Fst(expr) => {
            let (expr_tm, expr_ty) = infer_expr(cxt, expr)?;
            match (expr_tm, expr_ty) {
                (e, Value::Sig(fst, _)) => Ok((
                    Expr::Fst(Box::new(e)),
                    *fst
                )),
                (_, expr_ty) => report(
                    cxt,
                    &format!("Expected a pair type for fst, but get {:?}. infer from {:?}", expr_ty, expr)
                ),
            }
        },
        Expr::Snd(expr) => {
            let (expr_tm, expr_ty) = infer_expr(cxt, expr)?;
            match (expr_tm, expr_ty) {
                (e, Value::Sig(_, snd)) => Ok((
                    Expr::Snd(Box::new(e)),
                    *snd
                )),
                (_, expr_ty) => report(
                    cxt,
                    &format!("Expected a pair type for snd, but get {:?}. infer from {:?}", expr_ty, expr)
                ),
            }
        },
        Expr::Paren(expr) => {
            let (expr_tm, expr_ty) = infer_expr(cxt, expr)?;
            Ok((Expr::Paren(Box::new(expr_tm)), expr_ty))
        },
        Expr::Match(expr, vec) => todo!(),

        Expr::Let(name, typ, body) => {
            let typ_tm = check(cxt, typ, &Value::U)?;
            let vtyp = eval(cxt.env.clone(), typ_tm.clone());
            let t_tm = check(cxt, body, &vtyp)?;
            let vt = eval(cxt.env.clone(), t_tm.clone());
            //let new_cxt = cxt.define(name.0.clone(), vt, vtyp);
            Ok((Expr::Let(name.clone(), Box::new(typ_tm), Box::new(t_tm)), vt))
        }
    }
}

// Bidirectional algorithm:
//   use check when the type is already known
//   use infer if the type is unknown
fn check(cxt: &Cxt, t: &Expr<String>, a: &Value) -> Result<Expr<Lvl>, (String, usize)> {
    //println!(" {} {:?} == {:?}\n   in {:?}\n\n", "check".red(), t, a, cxt);
    match (t, a) {
        // Setting the source pos
        //(Raw::RSrcPos(pos, t), a) => check(&Cxt { pos: *pos, ..cxt.clone() }, t, a),

        // Checking Lam with Pi type (canonical checking case)
        // (\x. t) : ((x : A) -> B)
        (Expr::Lam(x, t), Value::Pi(x_, a_, b)) => {
            let new_cxt = cxt.bind(x.clone(), *a_.clone());
            let body = check(&new_cxt, t, &b.clone().apply(Value::Lvl(Id(cxt.lvl, Locate {
                offset: 0,
                line: 0,
                len: 0,
            }))))?;
            Ok(Expr::Lam(x.clone(), Box::new(body)))
        }

        // Fall-through checking
        (Expr::Let(x, a, t), a_) => {
            let a_tm = check(cxt, a, &Value::U)?;
            let va = eval(cxt.env.clone(), a_tm.clone());
            let t_tm = check(cxt, t, &va)?;
            //let vt = eval(cxt.env.clone(), t_tm.clone());
            //let new_cxt = cxt.define(x.clone(), vt, va);
            //let u_tm = check(&new_cxt, u, a_)?;
            Ok(Expr::Let(x.clone(), Box::new(a_tm), Box::new(t_tm)))
        }

        // If the term is not checkable, switch to infer (change of direction)
        _ => {
            let (t_tm, tty) = infer_expr(cxt, t)?;
            if !conv(Id(cxt.lvl, Locate {
                offset: 0,
                line: 0,
                len: 0,
            }), &tty, a) {
                report(
                    cxt,
                    &format!(
                        "type mismatch\n\nexpected type:\n\n  {:?}\n\ninferred type:\n\n  {:?}\n",
                        //show_val(cxt, a),
                        //show_val(cxt, &tty)
                        a, tty
                    ),
                )?;
            }
            Ok(t_tm)
        }
    }
}

fn report(cxt: &Cxt, msg: &str) -> Result<(Expr<Lvl>, Value), (String, usize)> {
    Err((msg.to_string(), cxt.pos))
}

fn conv(l: Id<usize>, t: &Value, u: &Value) -> bool {
    match (t, u) {
        (Value::U, Value::U) => true,
        (Value::Pi(_, a, b), Value::Pi(_, a_, b_)) => {
            conv(l, a, a_) && conv(l + 1, &b.clone().apply(Value::Lvl(l)), &b_.clone().apply(Value::Lvl(l)))
        }
        (Value::Lam(_, t), Value::Lam(_, t_)) => {
            conv(l + 1, &t.clone().apply(Value::Lvl(l)), &t_.clone().apply(Value::Lvl(l)))
        }
        (Value::Lam(_, t), u) => conv(l + 1, &t.clone().apply(Value::Lvl(l)), &Value::App(Box::new(u.clone()), Box::new(Value::Lvl(l)))),
        (u, Value::Lam(_, t)) => conv(l + 1, &Value::App(Box::new(u.clone()), Box::new(Value::Lvl(l))), &t.clone().apply(Value::Lvl(l))),
        (Value::Lvl(x), Value::Lvl(x_)) => x.0 == x_.0,
        (Value::App(t, u), Value::App(t_, u_)) => conv(l, t, t_) && conv(l, u, u_),
        (Value::Sig(a, b), Value::Sig(c, d)) => conv(l, a, c) && conv(l, b, d),
        _ => false,
    }
}

impl Add<usize> for Id<usize> {
    type Output = Id<usize>;

    fn add(self, rhs: usize) -> Self::Output {
        Id(self.0 + rhs, self.1)
    }
}

/*pub fn to_locally_nameless_decl<T: Eq + std::hash::Hash + Clone>(
    decl: &Decl<T>,
    env: &mut HashMap<T, u32>,
) -> Decl<Var<T>> {
    match decl {
        Decl::Def {
            name,
            tele,
            result,
            body,
        } => {
            // Add the function name to the environment
            let mut new_env = env.clone();
            new_env.insert(name.0.clone(), 0);

            // Convert the telescope (parameters)
            let tele_ln = tele
                .iter()
                .map(|param| {
                    let id_ln = Var::F(param.0 .0.clone());
                    let expr_ln = to_locally_nameless_expr(&param.1, &mut new_env);
                    Param(Id(id_ln, param.0 .1), Box::new(expr_ln))
                })
                .collect();

            // Convert the result type
            let result_ln = to_locally_nameless_expr(result, &mut new_env);

            // Convert the function body
            let body_ln = match body {
                FnBody::Expr(expr) => FnBody::Expr(to_locally_nameless_expr(expr, &mut new_env)),
                FnBody::Clause(clauses) => FnBody::Clause(
                    clauses
                        .iter()
                        .map(|clause| {
                            let patterns_ln = clause
                                .0
                                .iter()
                                .map(|pattern| to_locally_nameless_pattern(pattern, &mut new_env))
                                .collect();
                            let expr_ln = to_locally_nameless_expr(&clause.1, &mut new_env);
                            Clause(patterns_ln, expr_ln)
                        })
                        .collect(),
                ),
            };

            Decl::Def {
                name: Id(Var::F(name.0.clone()), name.1),
                tele: tele_ln,
                result: result_ln,
                body: body_ln,
            }
        }
        Decl::Print { tele, result, body } => {
            let mut new_env = env.clone();

            // Convert the telescope (parameters)
            let tele_ln = tele
                .iter()
                .map(|param| {
                    let id_ln = Var::F(param.0 .0.clone());
                    let expr_ln = to_locally_nameless_expr(&param.1, &mut new_env);
                    Param(Id(id_ln, param.0 .1), Box::new(expr_ln))
                })
                .collect();

            // Convert the result type
            let result_ln = to_locally_nameless_expr(result, &mut new_env);

            // Convert the body
            let body_ln = to_locally_nameless_expr(body, &mut new_env);

            Decl::Print {
                tele: tele_ln,
                result: result_ln,
                body: body_ln,
            }
        }
        Decl::Data { name, tele, cons } => {
            let mut new_env = env.clone();
            new_env.insert(name.0.clone(), 0);

            // Convert the telescope (parameters)
            let tele_ln = tele
                .iter()
                .map(|param| {
                    let id_ln = Var::F(param.0 .0.clone());
                    let expr_ln = to_locally_nameless_expr(&param.1, &mut new_env);
                    Param(Id(id_ln, param.0 .1), Box::new(expr_ln))
                })
                .collect();

            // Convert the constructors
            let cons_ln = cons
                .iter()
                .map(|cons_decl| {
                    let mut cons_env = new_env.clone();
                    cons_env.insert(cons_decl.name.0.clone(), 0);

                    let tele_ln = cons_decl
                        .tele
                        .iter()
                        .map(|param| {
                            let id_ln = Var::F(param.0 .0.clone());
                            let expr_ln = to_locally_nameless_expr(&param.1, &mut cons_env);
                            Param(Id(id_ln, param.0 .1), Box::new(expr_ln))
                        })
                        .collect();

                    ConsDecl {
                        name: Id(Var::F(cons_decl.name.0.clone()), cons_decl.name.1),
                        tele: tele_ln,
                    }
                })
                .collect();

            Decl::Data {
                name: Id(Var::F(name.0.clone()), name.1),
                tele: tele_ln,
                cons: cons_ln,
            }
        }
    }
}

fn to_locally_nameless_expr<T: Eq + std::hash::Hash + Clone>(
    expr: &Expr<T>,
    env: &mut HashMap<T, u32>,
) -> Expr<Var<T>> {
    match expr {
        Expr::Two(l, r) => Expr::Two(
            Box::new(to_locally_nameless_expr(l, env)),
            Box::new(to_locally_nameless_expr(r, env)),
        ),
        Expr::Fst(e) => Expr::Fst(Box::new(to_locally_nameless_expr(e, env))),
        Expr::Snd(e) => Expr::Snd(Box::new(to_locally_nameless_expr(e, env))),
        Expr::Univ => Expr::Univ,
        Expr::Dt(b, param, body) => {
            let id_ln = Var::F(param.0 .0.clone());
            let expr_ln = to_locally_nameless_expr(&param.1, env);
            let mut new_env = env.clone();
            new_env.insert(param.0 .0.clone(), 0);
            let body_ln = to_locally_nameless_expr(body, &mut new_env);
            Expr::Dt(*b, Param(Id(id_ln, param.0 .1), Box::new(expr_ln)), Box::new(body_ln))
        }
        Expr::Lam(id, body) => {
            let mut new_env = env.clone();
            new_env.insert(id.0.clone(), 0);
            let body_ln = to_locally_nameless_expr(body, &mut new_env);
            Expr::Lam(Id(Var::F(id.0.clone()), id.1), Box::new(body_ln))
        }
        Expr::App(l, r) => Expr::App(
            Box::new(to_locally_nameless_expr(l, env)),
            Box::new(to_locally_nameless_expr(r, env)),
        ),
        Expr::Ref(id) => {
            if let Some(&idx) = env.get(&id.0) {
                Expr::Ref(Id(Var::B(idx), id.1))
            } else {
                Expr::Ref(Id(Var::F(id.0.clone()), id.1))
            }
        }
        Expr::Paren(e) => Expr::Paren(Box::new(to_locally_nameless_expr(e, env))),
    }
}

fn to_locally_nameless_pattern<T: Eq + std::hash::Hash + Clone>(
    pattern: &Pattern<T>,
    env: &mut HashMap<T, u32>,
) -> Pattern<Var<T>> {
    match pattern {
        Pattern::Id(id) => {
            if let Some(&idx) = env.get(&id.0) {
                Pattern::Id(Id(Var::B(idx), id.1))
            } else {
                Pattern::Id(Id(Var::F(id.0.clone()), id.1))
            }
        }
        Pattern::Pat(id, pats) => {
            let mut new_env = env.clone();
            new_env.insert(id.0.clone(), 0);
            let pats_ln = pats
                .iter()
                .map(|pat| to_locally_nameless_pattern(pat, &mut new_env))
                .collect();
            Pattern::Pat(Id(Var::F(id.0.clone()), id.1), pats_ln)
        }
    }
}

#[test]
fn test() {
    // 示例：定义一个简单的 Decl
    let decl = Decl::Def {
        name: Id("foo".to_string(), Locate::default()),
        tele: vec![Param(Id("x".to_string(), Locate::default()), Box::new(Expr::Univ))],
        result: Expr::Univ,
        body: FnBody::Expr(Expr::Ref(Id("x".to_string(), Locate::default()))),
    };

    let mut env = HashMap::new();
    let decl_ln = to_locally_nameless_decl(&decl, &mut env);
    println!("{:?}", decl_ln);
}*/
