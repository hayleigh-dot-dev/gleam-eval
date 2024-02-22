import eval.{type Eval}

/// Get the current context.
///
pub fn get () -> Eval(ctx, e, ctx) {
    eval.from(fn (ctx) {
        #(ctx, Ok(ctx))
    })
}

/// Replace the current context with a new fixed value.
///
pub fn set (ctx: ctx) -> Eval(Nil, e, ctx) {
    eval.from(fn (_) {
        #(ctx, Ok(Nil))
    })
}

/// Run an `Eval` step and then replace the resulting context with a new fixed
/// value.
///
pub fn then_set (eval: Eval(a, e, ctx), ctx: ctx) -> Eval(a, e, ctx) {
    update(eval, fn (_, _) { ctx })
}

/// Update the current context by applying a function to it.
///
pub fn modify (f: fn (ctx) -> ctx) -> Eval(Nil, e, ctx) {
    get() |> eval.then(fn (ctx) { 
        set(f(ctx)) 
    })
}

/// 
///
pub fn update (eval: Eval(a, e, ctx), f: fn (ctx, a) -> ctx) -> Eval(a, e, ctx) {
    eval.map2(eval, get(), fn (a, ctx) { #(ctx, a) })
        |> eval.then(fn (pair) {
            let #(ctx, a) = pair

            set(f(ctx, a)) |> eval.map(fn (_) { a })
        })
}
