import eval.{type Eval}

/// Get the current context.
///
pub fn get() -> Eval(ctx, e, ctx) {
  eval.from(fn(ctx) { #(ctx, Ok(ctx)) })
}

/// Replace the current context with a new fixed value.
///
pub fn set(ctx: ctx) -> Eval(Nil, e, ctx) {
  eval.from(fn(_) { #(ctx, Ok(Nil)) })
}

/// Update the current context by applying a function to it.
///
pub fn update(f: fn(ctx) -> ctx) -> Eval(Nil, e, ctx) {
  get()
  |> eval.then(fn(ctx) { set(f(ctx)) })
}
