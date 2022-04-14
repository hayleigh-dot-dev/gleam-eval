import eval.{Eval}

/// 
pub fn get () -> Eval(ctx, e, ctx) {
    eval.from(fn (ctx) {
        #(ctx, Ok(ctx))
    })
}

///
pub fn set (ctx: ctx) -> Eval(Nil, e, ctx) {
    eval.from(fn (_) {
        #(ctx, Ok(Nil))
    })
}

///
pub fn modify (f: fn (ctx) -> ctx) -> Eval(Nil, e, ctx) {
    get() |> eval.then(fn (ctx) { 
        set(f(ctx)) 
    })
}

///
pub fn update (eval: Eval(a, e, ctx), f: fn (ctx, a) -> ctx) -> Eval(a, e, ctx) {
    eval.map2(eval, get(), fn (a, ctx) { #(ctx, a) })
        |> eval.then(fn (pair) {
            let #(ctx, a) = pair

            set(f(ctx, a)) |> eval.map(fn (_) { a })
        })
}
