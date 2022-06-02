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
pub fn succeed (value: a) -> Eval(a, e, ctx) {
  Eval(fn (ctx) {
    #(ctx, Ok(value))
  })
}

/// Like `succeed`, but used specifically with a function that takes two arguments.
/// This is most commonly used with `apply` to run a series of `Eval`s in a
/// pipeline to build up some more complex value.
///
/// 📝 Note: when used this way, this is often known as "applicative programming".
/// In this context, the `Eval` type would be known as an _applicative functor_.
///
/// ❓ Why are these `succeedN` functions necessary? In other functional programming
/// languages, like Elm or Haskell, functions are _curried_ which means all
/// functions are actually just a series of single-argument functions that return
/// other functions. We can achieve this in Gleam by using the `function.curryN`
/// functions. 
///
/// We need the functions passed to `succeed` to be curried to work properly with
/// `apply`, and so we provide a handful of these `succeedN` functions that do
/// the currying for you.
///
pub fn succeed2 (f: fn (a, b) -> c) -> Eval(fn (a) -> fn (b) -> c, e, ctx) {
  function.curry2(f)
    |> succeed
}

/// Like `succeed`, but used specifically with a function that takes three arguments.
/// This is most commonly used with `apply` to run a series of `Eval`s in a
/// pipeline to build up some more complex value.
///
pub fn succeed3 (f: fn (a, b, c) -> d) -> Eval(fn (a) -> fn (b) -> fn (c) -> d, e, ctx) {
  function.curry3(f)
    |> succeed
}

/// Like `succeed`, but used specifically with a function that takes four arguments.
/// This is most commonly used with `apply` to run a series of `Eval`s in a
/// pipeline to build up some more complex value.
///
pub fn succeed4 (f: fn (a, b, c, d) -> e) -> Eval(fn (a) -> fn (b) -> fn (c) -> fn (d) -> e, e, ctx) {
  function.curry4(f)
    |> succeed
}

/// Like `succeed`, but used specifically with a function that takes five arguments.
/// This is most commonly used with `apply` to run a series of `Eval`s in a
/// pipeline to build up some more complex value.
///
pub fn succeed5 (f: fn (a, b, c, d, e) -> f) -> Eval(fn (a) -> fn (b) -> fn (c) -> fn (d) -> fn (e) -> f, e, ctx) {
  function.curry5(f)
    |> succeed
}


/// Like `succeed`, but used specifically with a function that takes six arguments.
/// This is most commonly used with `apply` to run a series of `Eval`s in a
/// pipeline to build up some more complex value.
///
pub fn succeed6 (f: fn (a, b, c, d, e, f) -> g) -> Eval(fn (a) -> fn (b) -> fn (c) -> fn (d) -> fn (e) -> fn (f) -> g, e, ctx) {
  function.curry6(f)
    |> succeed
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
pub fn throw (error: e) -> Eval(a, e, ctx) {
  Eval(fn (ctx) {
    #(ctx, Error(error))
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
pub fn from_option (value: Option(a), error: e) -> Eval(a, e, ctx) {
  case value {
    Some(a) -> 
      succeed(a)
    
    None ->
      throw(error)
  }
}

/// Construct an `Eval` from a result. This is useful for situations where you have
/// some function or value that returns a `Result` but is not dependent on the
/// context.
///
pub fn from_result (value: Result(a, e)) -> Eval(a, e, ctx) {
  case value {
    Ok(a) -> 
      succeed(a)
    
    Error(e) ->
      throw(e)
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

/// Just like `map` but for error-producing steps instead. Transforms the error
/// produced by some `Eval` step using the given function.
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

/// Run an `Eval` step but then replace its result with some other fixed value.
/// Often used in tandem with effectful steps that often _do_ something but don't
/// produce any meaninful value (and so are usually `Eval(Nil, e, ctx)`).
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

/// Just like `replace` but for error-producing steps instead. Replaces the error
/// thrown by some `Eval` step with another, fixed, value.
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
/// an `Eval` and then _applies_ it to the result of the second argument.
///
/// ```gleam
/// case expr {
///   Add(lhs, rhs) ->
///     succeed2(fn (x, y) { x + y })
///       |> apply(eval(lhs))
///       |> apply(eval(rhs))
///
///   ...
/// }
/// ```
///
/// 📝 Note: you might find this called `ap` or `<*>` in some other languages
/// like Haskell or PureScript. In this context, the `Eval` type would be known
/// as an _applicative functor_.
///
pub fn apply (eval_f: Eval(fn (a) -> b, e, ctx), to eval_a: Eval(a, e, ctx)) -> Eval(b, e, ctx) {
  map2(eval_f, eval_a, fn (f, a) {
    f(a)
  })
}


/// Run an `Eval` and then apply a function that returns another `Eval` to the
/// result. This can be useful for chaining together multiple `Eval`s.
///
/// 📝 Note: you might find this called `bind`, `>>=`, `flatMap`, or `andThen` in
/// some other languages like Haskell, Elm, or PureScript. In this context, the
/// `Eval` type would be known as a _monad_.
///
pub fn then (eval: Eval(a, e, ctx), do f: fn (a) -> Eval(b, e, ctx)) -> Eval(b, e, ctx) {
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
/// ✨ Tip: in other languages there might be a more general version of this
/// function called `traverse`. You can easily create that by combining `list.map`
/// and `all`!
///
/// ```gleam
/// [ 1, 2, 3 ]
///     |> list.map(eval.succeed)
///     |> eval.all
///     // => Eval(List(Int), e, ctx)
/// ```
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
pub fn attempt (eval: Eval(a, e, ctx), catch f: fn (ctx, e) -> Eval(a, e, ctx)) -> Eval(a, e, ctx) {
  Eval(fn (ctx) {
    let #(ctx_, result) = runwrap(eval, ctx)

    case result {
      Ok(a) ->
        #(ctx_, Ok(a))

      Error(e) ->
        runwrap(f(ctx_, e), ctx)
    }
  })
}
