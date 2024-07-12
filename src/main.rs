// https://github.com/zesterer/chumsky/blob/main/tutorial.md

use chumsky::{error::Simple, primitive::{end, just}, recursive::recursive, text::{self, TextParser}, Parser};


#[derive(Debug)]
enum Expr {
    Num(f64),
    Var(String),

    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    Call(String, Vec<Expr>),
    Let {
        name: String,
        rhs: Box<Expr>,
        then: Box<Expr>
    },
    Fn {
        name: String,
        args: Vec<String>,
        body: Box<Expr>,
        then: Box<Expr>
    }
}

fn eval<'a>(expr: &'a Expr, vars: &mut Vec<(&'a String, f64)>) -> Result<f64, String> {
    match expr {
        Expr::Num(x) => Ok(*x),
        Expr::Neg(x) => Ok(-eval(x, vars)?),
        Expr::Add(x, y) => Ok(eval(x, vars)? + eval(y, vars)?),
        Expr::Sub(x, y) => Ok(eval(x, vars)? - eval(y, vars)?),
        Expr::Mul(x, y) => Ok(eval(x, vars)? * eval(y, vars)?),
        Expr::Div(x, y) => Ok(eval(x, vars)? / eval(y, vars)?),
        Expr::Let { name, rhs, then } => {
            let val = eval(rhs, vars)?;
            vars.push((name, val));
            let output = eval(then, vars);
            vars.pop();
            output
        },
        Expr::Var(name) => {
            if let Some((_, val)) = vars.iter().rev().find(|(varname, _)| *varname == name) {
                Ok(*val)
            } else {
                Err(format!("Cannot find variable `{}` in scope", name))
            }
        },
        _ => todo!()
    }
}

fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let ident = text::ident().padded();

    let expr = recursive(|expr| {
        let int = text::int(10)
            .map(|s: String| Expr::Num(s.parse().unwrap()))
            .padded();

        let atom = int
            .or(expr.delimited_by(just('('), just(')'))).padded()
            .or(ident.map(Expr::Var));

        let op = |c| just(c).padded();

        let unary = op('-')
            .repeated()
            .then(atom)
            .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));

        let product = unary.clone()
            .then(op('*').to(Expr::Mul as fn(_, _) -> _)
                .or(op('/').to(Expr::Div as fn(_, _) -> _))
                .then(unary).repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let sum = product.clone()
            .then(op('+').to(Expr::Add as fn(_, _) -> _)
                .or(op('-').to(Expr::Sub as fn(_, _) -> _))
                .then(product).repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        sum
    });

    // let a = let b = let c = <expr>hj;

    let decl = recursive(|decl| {
        let r#let = text::keyword("let")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(just(';'))
            .then(decl)
            .map(|((name, rhs), then)| Expr::Let {
                name,
                rhs: Box::new(rhs),
                then: Box::new(then),
            });

        r#let.or(expr).padded()
    });

    decl.then_ignore(end())
}

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let src = std::fs::read_to_string(file).unwrap();

    let parsed = parser().parse(src);
    eprintln!("parsed = {:#?}", parsed);

    if let Ok(parsed) = parsed {
        let mut vars = Vec::new();
        let evaluated = eval(&parsed, &mut vars);
        eprintln!("evaluated = {:#?}", evaluated);
    }
}
