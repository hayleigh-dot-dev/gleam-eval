import eval.{type Eval}
import eval/context
import gleam/list
import gleam/pair
import gleam/result

// -----------------------------------------------------------------------------
// TYPES
// -----------------------------------------------------------------------------

pub type Expr {
  Add(Expr, Expr)
  Sub(Expr, Expr)
  Mul(Expr, Expr)
  Div(Expr, Expr)
  Num(Float)
  Var(String)
  Let(String, be: Expr, in: Expr)
}

pub type Error {
  DivisionByZero
  UndefinedVariable(String)
}

pub type Scope =
  List(#(String, Float))

// -----------------------------------------------------------------------------
// FUNCTIONS
// -----------------------------------------------------------------------------

pub fn eval(expr: Expr) -> Eval(Float, Error, Scope) {
  case expr {
    Add(lhs, rhs) -> add(lhs, rhs)
    Sub(lhs, rhs) -> sub(lhs, rhs)
    Mul(lhs, rhs) -> mul(lhs, rhs)
    Div(lhs, rhs) -> div(lhs, rhs)
    Num(x) -> eval.return(x)
    Var(name) -> var(name)
    Let(name, value, body) -> let_(name, value, body)
  }
}

fn add(lhs: Expr, rhs: Expr) -> Eval(Float, Error, Scope) {
  use x <- eval.try(eval(lhs))
  use y <- eval.try(eval(rhs))

  eval.return(x +. y)
}

fn sub(lhs: Expr, rhs: Expr) -> Eval(Float, Error, Scope) {
  use x <- eval.try(eval(lhs))
  use y <- eval.try(eval(rhs))

  eval.return(x -. y)
}

fn mul(lhs: Expr, rhs: Expr) -> Eval(Float, Error, Scope) {
  use x <- eval.try(eval(lhs))
  use y <- eval.try(eval(rhs))

  eval.return(x *. y)
}

fn div(lhs: Expr, rhs: Expr) -> Eval(Float, Error, Scope) {
  use x <- eval.try(eval(lhs))
  use y <- eval.try(eval(rhs))
  use <- eval.guard(y == 0.0, DivisionByZero)

  eval.return(x /. y)
}

fn var(name: String) -> Eval(Float, Error, Scope) {
  let lookup = list.find(_, fn(binding) { pair.first(binding) == name })
  use scope <- eval.try(context.get())

  case lookup(scope) {
    Ok(#(_, expr)) -> eval.return(expr)
    Error(_) -> eval.throw(UndefinedVariable(name))
  }
}

fn let_(name: String, value: Expr, body: Expr) -> Eval(Float, Error, Scope) {
  // The standard library doesn't have a function for prepending an element to a
  // list, so we gotta define one ourselves.
  let push = fn(list, val) { [val, ..list] }
  // The standard library function that gets the tail of a list, somewhat annoyingly
  // returns an error, so this wrapper exists to just return an empty list instead.
  let pop = fn(list) {
    list.rest(list)
    |> result.unwrap([])
  }

  use n <- eval.try(eval(value))
  use _ <- eval.try(context.update(push(_, #(name, n))))
  use expr <- eval.try(eval(body))
  use _ <- eval.try(context.update(pop))

  eval.return(expr)
}
