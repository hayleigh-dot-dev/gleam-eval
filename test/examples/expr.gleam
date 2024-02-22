import gleam/list
import gleam/pair
import gleam/result
import eval.{type Eval}
import eval/context

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
    Add(lhs, rhs) ->
      add(lhs, rhs)

    Sub(lhs, rhs) ->
      sub(lhs, rhs)

    Mul(lhs, rhs) ->
      mul(lhs, rhs)

    Div(lhs, rhs) ->
      div(lhs, rhs)

    Num(x) -> 
      eval.succeed(x)

    Var(name) ->
      var(name)

    Let(name, value, body) ->
      let_(name, value, body)
  }
}

fn add (lhs: Expr, rhs: Expr) -> Eval(Float, Error, Scope) {
    let x = eval(lhs)
    let y = eval(rhs)

    eval.map2(x, y, fn (x, y) {
        x +. y
    })
}

fn sub (lhs: Expr, rhs: Expr) -> Eval(Float, Error, Scope) {
    let x = eval(lhs)
    let y = eval(rhs)

    eval.map2(x, y, fn (x, y) {
        x -. y
    })
}

fn mul (lhs: Expr, rhs: Expr) -> Eval(Float, Error, Scope) {
    let x = eval(lhs)
    let y = eval(rhs)

    eval.map2(x, y, fn (x, y) {
        x *. y
    })
}

fn div (lhs: Expr, rhs: Expr) -> Eval(Float, Error, Scope) {
    let x = eval(lhs)
    let y = eval(rhs) |> eval.then(fn (y) {
        case y == 0.0 {
            True -> 
                eval.throw(DivisionByZero)

            False ->
                eval.succeed(y)
        }
    })


    eval.map2(x, y, fn (x, y) {
        x /. y
    })
}

fn var (name: String) -> Eval(Float, Error, Scope) {
  let lookup = list.find(_, fn (binding) { pair.first(binding) == name })

  context.get() |> eval.then(fn (scope) {
    case lookup(scope) {
      Ok(#(_, expr)) -> 
        eval.succeed(expr)

      Error(_) -> 
        eval.throw(UndefinedVariable(name))
    }
  })
}

fn let_ (name: String, value: Expr, body: Expr) -> Eval(Float, Error, Scope) {
  // The standard library doesn't have a function for prepending an element to a
  // list, so we gotta define one ourselves.
  let push = fn (list, val) { [val, ..list] }
  // The standard library function that gets the tail of a list, somewhat annoyingly
  // returns an error, so this wrapper exists to just return an empty list instead.
  let pop  = fn (list) { list.rest(list) |> result.unwrap([]) }

  eval(value)
    |> eval.then(fn (n) { context.modify(push(_, #(name, n))) })
    |> eval.then(fn (_) { eval(body) })
    |> eval.then(fn (expr) { 
      context.modify(pop)
        |> eval.replace(expr)
    })
}
