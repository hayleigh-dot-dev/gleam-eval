import eval.{Eval}
import eval/context
import gleam/int
import gleam/list
import gleam/string

// -----------------------------------------------------------------------------
// TYPES
// -----------------------------------------------------------------------------

pub type Error {
    Unexpected(String)
    UnexpectedEOF
    InvalidParser
}

pub type Context {
    Context(
        commit: Bool,
        stream: List(String)
    )
}

type Parser(a)
    = Eval(a, Error, Context)

pub fn run (input: String, parser: Parser(a)) -> Result(a, Error) {
    let context = Context(False, string.to_graphemes(input))

    eval.run(parser, context)
}

// -----------------------------------------------------------------------------
// BASIC COMBINATORS
// -----------------------------------------------------------------------------

pub fn peek () -> Parser(String) {
    context.get() |> eval.then(fn (context: Context) {
        case context.stream {
            [ grapheme, .. ] ->
                Context(..context, commit: False)
                    |> context.set
                    |> eval.replace(grapheme)
            
            _ ->
                eval.throw(UnexpectedEOF)
        }
    })
}

pub fn pop () -> Parser(String) {
    context.get() |> eval.then (fn (context: Context) {
        case context.stream {
            [ grapheme, ..rest ] ->
                Context(commit: True, stream: rest)
                    |> context.set
                    |> eval.replace(grapheme)

            _ ->
                eval.throw(UnexpectedEOF)
        }
    })
}

pub fn many (parser: Parser(a)) -> Parser(List(a)) {
    let push = fn (x, xs) { [ x, ..xs ] }
    let go = fn (xs) {
        eval.try_(parser |> eval.map(push(_, xs)), catch: fn (_, _) {
            list.reverse(xs)
                |> eval.succeed
        })
    }

    go([])
}

pub fn one_of (parsers: List(Parser(a))) -> Parser(a) {
    case parsers {
        [] ->
            eval.throw(InvalidParser)

        [ parser ] ->
            parser

        [ parser, ..rest ] ->
            eval.try_(parser, catch: fn (e, ctx) {
                case ctx.commit {
                    True ->
                        eval.throw(e)
                        
                    False ->
                        one_of(rest)
                }
            })
    }
}

pub fn keep (f: Parser(fn (a) -> b), a: Parser(a)) -> Parser(b) {
    eval.apply(f, a)
}

pub fn drop (a: Parser(a), b: Parser(b)) -> Parser(a) {
    eval.map2(a, b, fn (x, _) {
        x
    })
}

// -----------------------------------------------------------------------------
// PARSERS
// -----------------------------------------------------------------------------

pub fn int () -> Parser(Int) {
    let is_digit = fn (s) {
        case s {
            "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
                True

            _ ->
                False
        }
    }
    let digit = peek() |> eval.then(fn (grapheme) {
        case is_digit(grapheme) {
            True ->
                pop()
            
            False ->
                eval.throw(Unexpected(grapheme))
        }
    })

    many(digit) |> eval.map(fn (num) {
        assert Ok(n) = string.join(num, "") |> int.parse

        n
    })
}

pub fn ws () -> Parser(Nil) {
    let is_space = fn (s) {
        case s {
            " " | "\t" | "\n" | "\r" ->
                True

            _ ->
                False
        }
    }
    let space = peek() |> eval.then(fn (grapheme) {
        case is_space(grapheme) {
            True ->
                pop()
            
            False ->
                eval.throw(Unexpected(grapheme))
        }
    })

    many(space) |> eval.replace(Nil)
}

pub fn symbol (sym: String) -> Parser(Nil) {
    string.to_graphemes(sym) |> list.fold(eval.succeed(Nil), fn (p, s) {
        p |> eval.then(fn (_) {
            peek() |> eval.then(fn (grapheme) {
                case grapheme == s {
                    True ->
                        pop() |> eval.replace(Nil)
                    
                    False ->
                        eval.throw(Unexpected(grapheme))
                }
            })
        })
    })
}
