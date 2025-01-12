use crate::parser::ast::*;

//use super::to_local::Var;

/*fn whnf_expr<T: Eq + std::hash::Hash + Clone>(
    expr: &Expr<Var<T>>,
    env: &mut Vec<Expr<Var<T>>>,
) -> Expr<Var<T>> {
    match expr {
        Expr::Two(l, r) => {
            env.push(*r.clone());
            whnf_expr(l, env)
        }
        Expr::Lam(id, body) if !env.is_empty() => {
            let arg = env.pop().unwrap();
            let body = open(&arg, body);
            whnf_expr(&body, env)
        }
        _ => {
            let mut result = expr.clone();
            while let Some(arg) = env.pop() {
                result = Expr::Two(Box::new(result), Box::new(arg));
            }
            result
        }
    }
}

fn open<T: Eq + std::hash::Hash + Clone>(
    arg: &Expr<Var<T>>,
    body: &Expr<Var<T>>,
) -> Expr<Var<T>> {
    fn go<T: Eq + std::hash::Hash + Clone>(
        outer: u32,
        arg: &Expr<Var<T>>,
        expr: &Expr<Var<T>>,
    ) -> Expr<Var<T>> {
        match expr {
            Expr::Two(l, r) => Expr::Two(
                Box::new(go(outer, arg, l)),
                Box::new(go(outer, arg, r)),
            ),
            Expr::Fst(e) => Expr::Fst(Box::new(go(outer, arg, e))),
            Expr::Snd(e) => Expr::Snd(Box::new(go(outer, arg, e))),
            Expr::Univ => Expr::Univ,
            Expr::Dt(b, param, body) => {
                let param = Param(
                    Id(param.0 .0.clone(), param.0 .1),
                    Box::new(go(outer, arg, &param.1)),
                );
                let body = go(outer + 1, arg, body);
                Expr::Dt(*b, param, Box::new(body))
            }
            Expr::Lam(id, body) => {
                let body = go(outer + 1, arg, body);
                Expr::Lam(id.clone(), Box::new(body))
            }
            Expr::App(l, r) => Expr::App(
                Box::new(go(outer, arg, l)),
                Box::new(go(outer, arg, r)),
            ),
            Expr::Ref(id) => match &id.0 {
                Var::B(bv) if *bv == outer => arg.clone(),
                Var::B(bv) => Expr::Ref(Id(Var::B(*bv), id.1)),
                Var::F(fv) => Expr::Ref(Id(Var::F(fv.clone()), id.1)),
            },
            Expr::Paren(e) => Expr::Paren(Box::new(go(outer, arg, e))),
        }
    }

    go(0, arg, body)
}

pub fn whnf_decl<T: Eq + std::hash::Hash + Clone>(
    decl: &Decl<Var<T>>,
) -> Decl<Var<T>> {
    match decl {
        Decl::Def {
            name,
            tele,
            result,
            body,
        } => {
            let result = whnf_expr(result, &mut Vec::new());
            let body = match body {
                FnBody::Expr(expr) => FnBody::Expr(whnf_expr(expr, &mut Vec::new())),
                FnBody::Clause(clauses) => FnBody::Clause(
                    clauses
                        .iter()
                        .map(|clause| {
                            let patterns = clause.0.clone();
                            let expr = whnf_expr(&clause.1, &mut Vec::new());
                            Clause(patterns, expr)
                        })
                        .collect(),
                ),
            };
            Decl::Def {
                name: name.clone(),
                tele: tele.clone(),
                result,
                body,
            }
        }
        Decl::Print { tele, result, body } => {
            let result = whnf_expr(result, &mut Vec::new());
            let body = whnf_expr(body, &mut Vec::new());
            Decl::Print {
                tele: tele.clone(),
                result,
                body,
            }
        }
        Decl::Data { name, tele, cons } => Decl::Data {
            name: name.clone(),
            tele: tele.clone(),
            cons: cons.clone(),
        },
    }
}*/