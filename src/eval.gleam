////
import gleam/function
import gleam/list
import gleam/option.{Option, Some, None}
import gleam/pair

// -----------------------------------------------------------------------------

/// A `Eval` represents a computation to be run given some context. That "to be
/// run" part turns out to be quite powerful. By combining `Eval`s, using some
/// of the functions in this module, we can build up a computation that has
/// access to a sort of mutable state that is updated as the computations are
/// run.
///
/// There are three type parameters here, not just two, because an `Eval` also
/// represents a computation that can fail. In many ways, an `Eval` is just a
/// superpowered `Result`!
///
pub opaque type Eval(a, e, ctx) {
  Eval(fn (ctx) -> #(ctx, Result(a, e)))
}

/// Given an `Eval`, actuall perform the computation by also providing the context
/// that the computation is running in.
///
pub fn run (eval: Eval(a, e, ctx), with context: ctx) -> Result(a, e) {
  runwrap(eval, context)
    |> pair.second
}

/// This is an internal function that just makes it easier to run individual
/// evals. Gleam does not support pattern matching in function arguments, even
/// in cases where it would not be ambiguous, so we use `runwrap` to avoid an
/// extra `let` binding just to unrwap the function contained inside an `Eval`.
///
fn runwrap (eval: Eval(a, e, ctx), ctx: ctx) -> #(ctx, Result(a, e)) {
  let Eval(eval) = eval

  eval(ctx)
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
pub fn succeed (a: a) -> Eval(a, e, ctx) {
  Eval(fn (ctx) {
    #(ctx, Ok(a))
  })
}

///
pub fn succeed2 (f: fn (a, b) -> c) -> Eval(fn (a) -> fn (b) -> c, e, ctx) {
  Eval(fn (ctx) {
    #(ctx, Ok(function.curry2(f)))
  })
}

///
pub fn succeed3 (f: fn (a, b, c) -> d) -> Eval(fn (a) -> fn (b) -> fn (c) -> d, e, ctx) {
  Eval(fn (ctx) {
    #(ctx, Ok(function.curry3(f)))
  })
}

///
pub fn succeed4 (f: fn (a, b, c, d) -> e) -> Eval(fn (a) -> fn (b) -> fn (c) -> fn (d) -> e, e, ctx) {
  Eval(fn (ctx) {
    #(ctx, Ok(function.curry4(f)))
  })
}

///
pub fn succeed5 (f: fn (a, b, c, d, e) -> f) -> Eval(fn (a) -> fn (b) -> fn (c) -> fn (d) -> fn (e) -> f, e, ctx) {
  Eval(fn (ctx) {
    #(ctx, Ok(function.curry5(f)))
  })
}

///
pub fn succeed6 (f: fn (a, b, c, d, e, f) -> g) -> Eval(fn (a) -> fn (b) -> fn (c) -> fn (d) -> fn (e) -> fn (f) -> g, e, ctx) {
  Eval(fn (ctx) {
    #(ctx, Ok(function.curry6(f)))
  })
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
pub fn throw (e: e) -> Eval(a, e, ctx) {
  Eval(fn (ctx) {
    #(ctx, Error(e))
  })
}

/// Construct an `Eval` from a function that takes some context and returns a pair
/// of a new context and some `Result` value. This is provided as a fallback if
/// none of the functions here or in `eval/context` are getting you where you need
/// to go: generally you should avoid using this in favour of _combining_ the
/// other functions in this module!
///
pub fn from (eval: fn (ctx) -> #(ctx, Result(a, e))) -> Eval(a, e, ctx) {
  Eval(eval)
}

/// Construct an `Eval` from an optional value and an error to throw if that value
/// is `None`. This is useful for situations where you have some function or value
/// that returns an `Option` but is not dependent on the context.
///
pub fn from_option (a: Option(a), e: e) -> Eval(a, e, ctx) {
  case a {
    Some(a) -> 
      succeed(a)
    
    None ->
      throw(e)
  }
}

/// Construct an `Eval` from a result. This is useful for situations where you have
/// some function or value that returns a `Result` but is not dependent on the
/// context.
///
pub fn from_result (a: Result(a, e)) -> Eval(a, e, ctx) {
  case a {
    Ok(a) -> 
      succeed(a)
    
    Error(e) ->
      throw(e)
  }
}

// -----------------------------------------------------------------------------
// MANIPULATIONS
// -----------------------------------------------------------------------------

/// 
///
/// 📝 Note: you might find this called `fmap` or `<$>` in some other languages
/// like Haskell or PureScript.
///
pub fn map (eval: Eval(a, e, ctx), by f: fn (a) -> b) -> Eval(b, e, ctx) {
  Eval(fn (ctx) {
    let #(ctx, result) = runwrap(eval, ctx)

    case result {
      Ok(a) ->
        #(ctx, Ok(f(a)))
  
      Error(e) ->
        #(ctx, Error(e))
    }
  })
}

///
///
/// 📝 Note: you might find this called `liftA2` or `liftM2` in some other
/// languages like Haskell or PureScript.
///
pub fn map2 (eval_a: Eval(a, e, ctx), eval_b: Eval(b, e, ctx), by f: fn (a, b) -> c) -> Eval(c, e, ctx) {
  Eval(fn (ctx) {
    let #(ctx, result1) = runwrap(eval_a, ctx)

    case result1 {
      Ok(a) -> {
        let #(ctx, result2) = runwrap(eval_b, ctx)

        case result2 {
          Ok(b) ->
            #(ctx, Ok(f(a, b)))

          Error(e) ->
            #(ctx, Error(e))
        }
      }

      Error(e) ->
        #(ctx, Error(e))
    }
  })
}

///
pub fn map_error (eval: Eval(a, e, ctx), by f: fn (e) -> x) -> Eval(a, x, ctx) {
  Eval(fn (ctx) {
    let #(ctx, result) = runwrap(eval, ctx)

    case result {
      Ok(a) ->
        #(ctx, Ok(a))
  
      Error(e) ->
        #(ctx, Error(f(e)))
    }
  })
}

///
pub fn replace (eval: Eval(a, e, ctx), with replacement: b) -> Eval(b, e, ctx) {
  Eval(fn (ctx) {
    let #(ctx, result) = runwrap(eval, ctx)

    case result {
      Ok(_) ->
        #(ctx, Ok(replacement))
  
      Error(e) ->
        #(ctx, Error(e))
    }
  })
}

///
pub fn replace_error (eval: Eval(a, e, ctx), with replacement: x) -> Eval(a, x, ctx) {
  Eval(fn (ctx) {
    let #(ctx, result) = runwrap(eval, ctx)

    case result {
      Ok(a) ->
        #(ctx, Ok(a))
  
      Error(_) ->
        #(ctx, Error(replacement))
    }
  })
}

// -----------------------------------------------------------------------------
// COMBINATORS
// -----------------------------------------------------------------------------

/// Intended to be used in combination with the `succeed{N}` functions. This runs
/// an `Eval` and then "keeps" it by applying it to the result of the previous
/// previous eval.
///
/// ```gleam
/// case expr {
///   Add(lhs, rhs) ->
///     succeed2(fn (x, y) { x + y })
///       |> keep(eval(lhs))
///       |> keep(eval(rhs))
///
///   ...
/// }
/// ```
///
/// 📝 Note: you might find this called `ap` or `<*>` in some other languages
/// like Haskell or PureScript.
///
pub fn keep (eval_f: Eval(fn (a) -> b, e, ctx), eval_a: Eval(a, e, ctx)) -> Eval(b, e, ctx) {
  map2(eval_f, eval_a, fn (f, a) {
    f(a)
  })
}

/// Intended to be used in combination with the `succeed{N}` functions. This runs
/// runs an `Eval` and then "drops" it, keeping the result of some previous eval
/// instead.
/// 
/// This is particularly useful if you have evals that modify the context but 
/// don't produce any meaningful values. In the example below, we will modify
/// the context to include a variable `"z"` to exist just in the scope of `lhs`
/// and then remove it before evaluating `rhs`.
/// 
/// ```gleam
/// case expr {
///   Add(lhs, rhs) ->
///     succeed2(fn (x, y) { x + y })
///       |> drop(context.modify(stack.push(_, #("z", 1))))
///       |> keep(eval(lhs))
///       |> drop(context.modify(stack.pop(_)))
///       |> drop(eval(rhs))
/// 
///   ...
/// }
/// ```
/// 
/// 📝 Note: you might find this called `<*` or `*>` in some other languages like
/// Haskell or PureScript.
/// 
pub fn drop (eval_a: Eval(a, e, ctx), eval_b: Eval(b, e, ctx)) -> Eval(a, e, ctx) {
  map2(eval_a, eval_b, fn (a, _) {
    a
  })
}

/// Run an `Eval` and then apply a function that returns another `Eval` to the
/// result. This can be useful for chaining together multiple `Eval`s.
///
/// 📝 Note: you might find this called `bind`, `>>=`, `flatMap`, or `andThen` in
/// some other languages like Haskell, Elm, or PureScript.
///
pub fn then (eval : Eval(a, e, ctx), do f: fn (a) -> Eval(b, e, ctx)) -> Eval(b, e, ctx) {
  Eval(fn (ctx) {
    let #(ctx, result) = runwrap(eval, ctx)

    case result {
      Ok(a) -> {
        runwrap(f(a), ctx)
      }

      Error(e) ->
        #(ctx, Error(e))
    }
  })
}

/// Run a list of `Eval`s in sequence and then combine their results into a list.
/// If any of the `Eval`s fail, the whole sequence fails.
///
/// 📝 Note: you might find this called `sequence` in some other languages like
/// Haskell or PureScript.
///
pub fn all (evals: List(Eval(a, e, ctx))) -> Eval(List(a), e, ctx) {
  let prepend  = fn (list, a) { [a, ..list] }
  let callback = fn (a, list) { map2(a, list, prepend) }

  list.fold(evals, succeed([]), callback)
    |> map(list.reverse)
}

/// Run an `Eval` and then attempt to recover from an error by applying a function
/// that takes the error value and returns another `Eval`.
///
pub fn try_ (eval: Eval(a, e, ctx), catch f: fn (e) -> Eval(a, e, ctx)) -> Eval(a, e, ctx) {
  Eval(fn (ctx) {
    let #(ctx, result) = runwrap(eval, ctx)

    case result {
      Ok(a) ->
        #(ctx, Ok(a))

      Error(e) ->
        runwrap(f(e), ctx)
    }
  })
}
