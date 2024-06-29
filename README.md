# gleam_eval

> Chain together stateful computations in Gleam, and defer their evaluation until
you need them.

[![Package Version](https://img.shields.io/hexpm/v/eval)](https://hex.pm/packages/eval)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/eval/)

> ❗️ **While the documentation for this package is still being worked on, the API
is very much in flux. Feel free to experiment but know that things may change
in the future!**

Gleam is an immutable functional programming language, but sometimes it would be
really helpful to model some mutable state that can be threaded through multiple
computations. That's where `Eval` comes in!

Everything in this package is built around the idea of an `Eval` type, which can
be summarised as:

```gleam
fn (context) -> #(context, Result(a, e))
```

That is, a function that takes some `context` and returns a (potentially updated)
`context` and a `Result` of some `a` or some error `e`. With this, we can model
all manner of computations that can be expressed in a purely functional way: we
can update the context, throw an error, return a value, or some combination!

> ❗️ This package is written in _pure Gleam_ so you can use it whether you're
targetting Erlang _or_ JavaScript.

---

## Installation

If available on Hex, this package can be added to your Gleam project:

```sh
gleam add eval
```

and its documentation can be found at <https://hexdocs.pm/eval>.

---

## Usage

We mentioned above that an `Eval` is essentially a function that takes some
`context` and returns a `Result`. We can start to express more complex and powerful
computations by composing these functions together using the various functions
provided in this package.

To demonstrate how everything can neatly fit together, let's write a parser for
a simple expression language. To keep things manageable, our expression language
will only allow for two types of expressions:

- `Number`s which are just integers...
- ...and `Add`s, which are binary expressions that add two numbers together

```gleam
type Expr {
  Number(Int)
  Add(Expr, Expr)
}
```

Our `Parser` type will be a specialisation of the `Eval` type from this package.
The context will be a `List(String)` that represents the current stream of
graphemes (Gleam doesn't have a traditional `Char` type). Additionally, we will
define an `Error` type that we can throw if something goes wrong during parsing:

```gleam
type Error {
  Unexpected(String)
  Eof
  InvalidParser
}
```

Finally, while we intend on ultimately parsing some `Expr` value, we will leave
the the type free in our `Parser` type, so that we can write intermediary parsers
that may return some other type instead (this will be useful, as we'll se in a
moment).

```gleam
type Parser(a) =
  Eval(a, Error, List(String))
```

### Basic building blocks

Now we know what types we'll be dealing with, it's time to write out first parsers
using this package! We're going to begin by defining a `peek` and a `pop` parser
that will return the next grapheme in the context (and remove it, in the case of
`pop`).

First, make sure we've imported everything we need:

```gleam
import eval
import eval/context
import gleam/int
import gleam/list
import gleam/string
```

Then we can define our `peek` parser:

```gleam
pub fn peek () -> Parser(String) {
  use stream <- eval.try(context.get())

  case stream {
    [grapheme, ..] -> eval.return(grapheme)
    _ -> eval.throw(Eof)
  }
}
```

The API of this package has been designed so that (hopefully) this reads quite
naturally, even if you are not familiar with how things are implemented, or indeed
Gleam or functional programming in general. First we `get` the context, `then` we
look at the stream. If there are still graphemes left we `succeed` with the first
element in the stream, otherwise we `throw` with an `EOF` error.

Our `pop` parser is almost identical, with the additional step of modifying the
context to remove the grapheme we want to return:

```gleam
pub fn pop () -> Parser(String) {
  use stream <- eval.try(context.get())

  case stream {
    [grapheme, ..rest] -> {
      use _ <- eval.try(context.set(rest))

      eval.return(grapheme)
    }
    _ -> eval.throw(Eof)
  }
}
```

### Some parser combinators

With just `peek` and `pop` we can start to build up more powerful parsers. To
parse an integer we'll need to continuously `pop` graphemes off the stack until
we come across a non-digit character. We can do this by writing a `many`
combinator:

```gleam
pub fn many (parser: Parser(a)) -> Parser(List(a)) {
  let one = fn (xs) { eval.map(parser, list.prepend(xs, _)) }
  let go = fn (xs) {
    eval.attempt(one, catch: fn (_) {
      list.reverse(xs) |> eval.succeed
    })
  }

  go([])
}
```

This will repeatedly call `parser` until it throws, and then return the list of
all the values it has parsed so far. The list is reversed before returning
because we want to return the values in the order they were parsed.

> ❓ How might we modify this to parse `one_or_more` of something instead? There's
more than one way, but thinking about it will hopefully help you see all the
interesting ways `Eval` computations can be built up!

It's worth noting that `many` itself doesn't alter the context in any way. Despite
this, changes to the context made by running `parser` will be propagated to later
runs as you'd expect. No manual threading of state required.

Before we can parse integers, we'll want a `digit` parser to pass to `many`:

```gleam
pub fn digit () -> Parser(String) {
  use grapheme <- eval.try(peek())

  case grapheme {
    "0" | "1" | "2" | "3" | "4" |
    "5" | "6" | "7" | "8" | "9" -> eval.replace(pop(), grapheme)
    _ -> eval.throw(Unexpected(grapheme))
  }
}
```

Then, we can write an `int` parser that consumes as many digits as possible,
joins the string back together and then uses Gleam's own `int.parse` function to
turn that string into a bona fide integer:

```gleam
pub fn int () -> Parser(Int) {
  use digits <- eval.try(many(digit()))
  let number = string.join(digits, "")
  let assert Ok(n) = int.parse(number)

  eval.return(n)
}
```

With that out of the way, the final combinator we need is one that let's attempt
multiple parsers, and keep the result of whichever succeeds first. For that we
will define `one_of`:

```gleam
pub fn one_of (parsers: List(Parser(a))) -> Parser(a) {
  case parsers {
    [parser, ..rest] -> eval.attempt(parser, catch: fn (_) { one_of(rest) })
    _ -> eval.throw(InvalidParser)
  }
}
```

### Parsing expressions

Now we are ready to write a parser for each our of expression variants, and then
combine everything into a single `expr` parser. We've seen enough of the `eval`
package now to be able to put together these parsers will little comment, so here
is the rest of the program:

```gleam
pub fn number () -> Parser(Expr) {
  int() |> eval.map(Number)
}

pub fn expr () -> Parser(Expr) {
  use lhs <- eval.try(number())
  use grapheme <- eval.try(peek())

  case grapheme {
    "+" -> {
      use _ <- eval.try(pop())
      use rhs <- eval.try(expr())

      eval.return(Add(lhs, rhs))
    }
    _ -> eval.succeed(lhs)
  }
}

pub fn run (input: String) -> Result(Expr, Error) {
  eval.run(expr(), with: string.graphemes(input))
}
```

Take a look in `test/examples/` to see the complete parser example, as well as
a handful of others.
