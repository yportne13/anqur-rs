use crate::{syntax::term::Term, Diagnostic};

pub fn untyped(l: &Term, r: &Term) -> Result<bool, Diagnostic> {
    match (l, r) {
        (a, b) if a == b => Ok(true),
        (
            Term::Lam { x: _, body: bodyl },
            Term::Lam { x: _, body: bodyr },
        ) => {
            untyped(bodyl, bodyr)
        },
        (Term::Lam { x, body }, b) => {
            untyped(body, &b.app(vec![Term::Ref { var: *x }]))
        },
        (a, Term::Lam { x, body }) => {
            untyped(body, &a.app(vec![Term::Ref { var: *x }]))
        },
        (Term::Ref { var: varl }, Term::Ref { var: varr }) => {
            Ok(varl == varr)
        }
        (Term::Two { is_app: l_is_app, f: lf, a: la }, Term::Two { is_app: r_is_app, f: rf, a: ra }) => {
            Ok((l_is_app == r_is_app) & (untyped(lf, rf)?) & (untyped(la, ra)?))
        },
        (
            Term::DT { is_pi: l_is_pi, param: lparam, cod: lcod },
            Term::DT { is_pi: r_is_pi, param: rparam, cod: rcod },
        ) => {
            Ok(
                (l_is_pi == r_is_pi) &
                    (untyped(&lparam.ty, &rparam.ty)?) &
                    (untyped(lcod, &rcod.subst(rparam.id, Term::Ref { var: lparam.id }))?)
            )
        },
        (Term::Proj { t: lt, is_one: l_is_one }, Term::Proj { t: rt, is_one: r_is_one }) => {
            Ok((l_is_one == r_is_one) &
                (untyped(lt, rt)?))
        },
        //Term::UI == Term::UI
        (Term::FnCall { fun: lfun, args: largs }, Term::FnCall { fun: rfun, args: rargs })
        | (Term::ConCall { fun: lfun, args: largs }, Term::ConCall { fun: rfun, args: rargs })
        | (Term::DataCall { fun: lfun, args: largs }, Term::DataCall { fun: rfun, args: rargs }) => {
            Ok((lfun == rfun) &
                unify_seq(largs, rargs)?
            )
        },
        _ => Ok(false)
    }
}

fn unify_seq(l: &[Term], r: &[Term]) -> Result<bool, Diagnostic> {
    let mut ret = true;
    for (a, b) in l.iter().zip(r.iter()) {
        ret &= untyped(a, b)?;
    }
    Ok(ret)
}
