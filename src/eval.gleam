////

import gleam/bool
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair

// -----------------------------------------------------------------------------

/// A `Eval` represents a computation to be run given some context. That "to be
/// run" part turns out to be quite powerful. By combining `Eval`s using some
/// of the functions in this module, we can build up a computation that has
/// access to a sort of mutable state that is updated as the computations are
/// run.
///
/// There are three type parameters here, not just two, because an `Eval` also
/// represents a computation that can fail. In many ways, an `Eval` is just a
/// superpowered `Result`!
///
pub opaque type Eval(a, e, ctx) {
  Eval(run: fn(ctx) -> #(ctx, Result(a, e)))
}

/// Given an `Eval`, actuall perform the computation by also providing the context
/// that the computation is running in.
///
pub fn run(eval: Eval(a, e, ctx), with context: ctx) -> Result(a, e) {
  eval.run(context) |> pair.second
}

/// Step through an `Eval` and get back both it's result and the context it
/// produced. This is especially useful if you want to run some computation,
/// do some other Gleam bits, and then continue with the computation by passing
/// the produced context to `run` or `step` again.
///
pub fn step(eval: Eval(a, e, ctx), ctx: ctx) -> #(ctx, Result(a, e)) {
  eval.run(ctx)
}

// -----------------------------------------------------------------------------
// CONSTRUCTORS
// -----------------------------------------------------------------------------

/// Construct an `Eval` that always succeeds with the given value, regardless of
/// context.
///
/// 📝 Note: you might find this called `pure` or `return` in some other languages
/// like Haskell or PureScript.
///
pub fn return(value: a) -> Eval(a, e, ctx) {
  use ctx <- Eval

  #(ctx, Ok(value))
}

/// Construct an `Eval` that always fails with the given error, regardless of
/// context. Often used in combination with `then` to run some `Eval` and then
/// potentially fail based on the result of that computation.
///
/// ```gleam
/// eval(expr) |> then(fn (y) {
///   case y == 0.0 {
///     True ->
///       throw(DivisionByZero)
///
///     False ->
///       succeed(y)
///   }
/// })
/// ```
///
pub fn throw(error: e) -> Eval(a, e, ctx) {
  use ctx <- Eval

  #(ctx, Error(error))
}

/// Construct an `Eval` from a function that takes some context and returns a pair
/// of a new context and some `Result` value. This is provided as a fallback if
/// none of the functions here or in `eval/context` are getting you where you need
/// to go: generally you should avoid using this in favour of _combining_ the
/// other functions in this module!
///
pub fn from(eval: fn(ctx) -> #(ctx, Result(a, e))) -> Eval(a, e, ctx) {
  Eval(eval)
}

/// Construct an `Eval` from an optional value and an error to throw if that value
/// is `None`. This is useful for situations where you have some function or value
/// that returns an `Option` but is not dependent on the context.
///
pub fn from_option(value: Option(a), error: e) -> Eval(a, e, ctx) {
  case value {
    Some(a) -> return(a)
    None -> throw(error)
  }
}

/// Construct an `Eval` from a result. This is useful for situations where you have
/// some function or value that returns a `Result` but is not dependent on the
/// context.
///
pub fn from_result(value: Result(a, e)) -> Eval(a, e, ctx) {
  case value {
    Ok(a) -> return(a)
    Error(e) -> throw(e)
  }
}

// -----------------------------------------------------------------------------
// MANIPULATIONS
// -----------------------------------------------------------------------------

/// Transform the value produced by an `Eval` using the given function.
///
/// 📝 Note: you might find this called `fmap` or `<$>` in some other languages
/// like Haskell or PureScript. In this context, the `Eval` type would be known
/// as a _functor_.
///
pub fn map(eval: Eval(a, e, ctx), by f: fn(a) -> b) -> Eval(b, e, ctx) {
  use ctx <- Eval
  let #(ctx, result) = eval.run(ctx)

  case result {
    Ok(a) -> #(ctx, Ok(f(a)))
    Error(e) -> #(ctx, Error(e))
  }
}

///
///
/// 📝 Note: you might find this called `liftA2` or `liftM2` in some other
/// languages like Haskell or PureScript.
///
pub fn map2(
  eval_a: Eval(a, e, ctx),
  eval_b: Eval(b, e, ctx),
  by f: fn(a, b) -> c,
) -> Eval(c, e, ctx) {
  use ctx <- Eval
  let #(ctx, res) = eval_a.run(ctx)

  case res {
    Ok(a) -> {
      let #(ctx, res) = eval_b.run(ctx)

      case res {
        Ok(b) -> #(ctx, Ok(f(a, b)))
        Error(e) -> #(ctx, Error(e))
      }
    }

    Error(e) -> #(ctx, Error(e))
  }
}

/// Just like `map` but for error-producing steps instead. Transforms the error
/// produced by some `Eval` step using the given function.
///
pub fn map_error(eval: Eval(a, e, ctx), by f: fn(e) -> x) -> Eval(a, x, ctx) {
  use ctx <- Eval
  let #(ctx, result) = eval.run(ctx)

  case result {
    Ok(a) -> #(ctx, Ok(a))
    Error(e) -> #(ctx, Error(f(e)))
  }
}

/// Run an `Eval` step but then replace its result with some other fixed value.
/// Often used in tandem with effectful steps that often _do_ something but don't
/// produce any meaninful value (and so are usually `Eval(Nil, e, ctx)`).
///
pub fn replace(eval: Eval(a, e, ctx), with replacement: b) -> Eval(b, e, ctx) {
  use ctx <- Eval
  let #(ctx, result) = eval.run(ctx)

  case result {
    Ok(_) -> #(ctx, Ok(replacement))
    Error(e) -> #(ctx, Error(e))
  }
}

/// Just like `replace` but for error-producing steps instead. Replaces the error
/// thrown by some `Eval` step with another, fixed, value.
///
pub fn replace_error(
  eval: Eval(a, e, ctx),
  with replacement: x,
) -> Eval(a, x, ctx) {
  use ctx <- Eval
  let #(ctx, result) = eval.run(ctx)

  case result {
    Ok(a) -> #(ctx, Ok(a))
    Error(_) -> #(ctx, Error(replacement))
  }
}

// -----------------------------------------------------------------------------
// COMBINATORS
// -----------------------------------------------------------------------------

/// Run an `Eval` and then apply a function that returns another `Eval` to the
/// result. This can be useful for chaining together multiple `Eval`s.
///
/// 📝 Note: you might find this called `bind`, `>>=`, `flatMap`, or `andThen` in
/// some other languages like Haskell, Elm, or PureScript. In this context, the
/// `Eval` type would be known as a _monad_.
///
pub fn then(
  eval: Eval(a, e, ctx),
  do f: fn(a) -> Eval(b, e, ctx),
) -> Eval(b, e, ctx) {
  use ctx <- Eval
  let #(ctx, result) = eval.run(ctx)

  case result {
    Ok(a) -> step(f(a), ctx)
    Error(e) -> #(ctx, Error(e))
  }
}

/// Run an `Eval` and then apply a function that returns another `Eval` to the
/// result. This can be useful for chaining together multiple `Eval`s. This is
/// the same as [`then`](#then) but you might find the `try` naming nicer to use
/// with Gleam's `use` notation.
///
/// 📝 Note: you might find this called `bind`, `>>=`, `flatMap`, or `andThen` in
/// some other languages like Haskell, Elm, or PureScript. In this context, the
/// `Eval` type would be known as a _monad_.
///
pub fn try(
  eval: Eval(a, e, ctx),
  then f: fn(a) -> Eval(b, e, ctx),
) -> Eval(b, e, ctx) {
  use ctx <- Eval
  let #(ctx, result) = eval.run(ctx)

  case result {
    Ok(a) -> step(f(a), ctx)
    Error(e) -> #(ctx, Error(e))
  }
}

///
///
pub fn guard(
  when requirement: Bool,
  return consequence: e,
  otherwise do: fn() -> Eval(a, e, ctx),
) -> Eval(a, e, ctx) {
  bool.guard(requirement, throw(consequence), do)
}

/// Run a list of `Eval`s in sequence and then combine their results into a list.
/// If any of the `Eval`s fail, the whole sequence fails.
///
/// 📝 Note: you might find this called `sequence` in some other languages like
/// Haskell or PureScript.
///
///
pub fn all(evals: List(Eval(a, e, ctx))) -> Eval(List(a), e, ctx) {
  let prepend = fn(list, a) { [a, ..list] }
  let callback = fn(a, list) { map2(a, list, prepend) }

  list.fold(evals, return([]), callback)
  |> map(list.reverse)
}

/// Run an `Eval` and then attempt to recover from an error by applying a function
/// that takes the error value and returns another `Eval`.
///
pub fn attempt(
  eval: Eval(a, e, ctx),
  catch f: fn(ctx, e) -> Eval(a, e, ctx),
) -> Eval(a, e, ctx) {
  use ctx <- Eval
  let #(ctx_, result) = eval.run(ctx)

  case result {
    Ok(a) -> #(ctx_, Ok(a))
    Error(e) -> step(f(ctx_, e), ctx)
  }
}
