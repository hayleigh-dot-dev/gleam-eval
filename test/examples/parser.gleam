import eval.{type Eval}
import eval/context
import gleam/function
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
  Context(commit: Bool, stream: List(String))
}

type Parser(a) =
  Eval(a, Error, Context)

pub fn run(input: String, parser: Parser(a)) -> Result(a, Error) {
  let context = Context(False, string.to_graphemes(input))

  eval.run(parser, context)
}

// -----------------------------------------------------------------------------
// BASIC COMBINATORS
// -----------------------------------------------------------------------------

pub fn peek() -> Parser(String) {
  use ctx: Context <- eval.try(context.get())

  case ctx.stream {
    [grapheme, ..] ->
      Context(..ctx, commit: False)
      |> context.set
      |> eval.replace(grapheme)
    _ -> eval.throw(UnexpectedEOF)
  }
}

pub fn pop() -> Parser(String) {
  use ctx: Context <- eval.try(context.get())

  case ctx.stream {
    [grapheme, ..rest] ->
      Context(commit: True, stream: rest)
      |> context.set
      |> eval.replace(grapheme)

    _ -> eval.throw(UnexpectedEOF)
  }
}

pub fn many(parser: Parser(a)) -> Parser(List(a)) {
  let go = fn(xs) {
    eval.attempt(
      parser
        |> eval.map(list.prepend(xs, _)),
      catch: fn(_, _) {
        list.reverse(xs)
        |> eval.return
      },
    )
  }

  go([])
}

pub fn one_of(parsers: List(Parser(a))) -> Parser(a) {
  case parsers {
    [] -> eval.throw(InvalidParser)

    [parser] -> parser

    [parser, ..rest] ->
      eval.attempt(parser, catch: fn(ctx, e) {
        case ctx.commit {
          True -> eval.throw(e)

          False -> one_of(rest)
        }
      })
  }
}

pub fn keep(f: Parser(fn(a) -> b), a: Parser(a)) -> Parser(b) {
  eval.map2(f, a, function.apply1)
}

pub fn drop(a: Parser(a), b: Parser(b)) -> Parser(a) {
  eval.map2(a, b, fn(x, _) { x })
}

// -----------------------------------------------------------------------------
// PARSERS
// -----------------------------------------------------------------------------

pub fn int() -> Parser(Int) {
  let is_digit = fn(s) {
    case s {
      "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True

      _ -> False
    }
  }
  let digit =
    peek()
    |> eval.then(fn(grapheme) {
      case is_digit(grapheme) {
        True -> pop()

        False -> eval.throw(Unexpected(grapheme))
      }
    })

  many(digit)
  |> eval.map(fn(num) {
    let assert Ok(n) =
      string.join(num, "")
      |> int.parse

    n
  })
}

pub fn ws() -> Parser(Nil) {
  let is_space = fn(s) {
    case s {
      " " | "\t" | "\n" | "\r" -> True

      _ -> False
    }
  }
  let space =
    peek()
    |> eval.then(fn(grapheme) {
      case is_space(grapheme) {
        True -> pop()

        False -> eval.throw(Unexpected(grapheme))
      }
    })

  many(space)
  |> eval.replace(Nil)
}

pub fn symbol(sym: String) -> Parser(Nil) {
  string.to_graphemes(sym)
  |> list.fold(eval.return(Nil), fn(p, s) {
    p
    |> eval.then(fn(_) {
      peek()
      |> eval.then(fn(grapheme) {
        case grapheme == s {
          True ->
            pop()
            |> eval.replace(Nil)

          False -> eval.throw(Unexpected(grapheme))
        }
      })
    })
  })
}
