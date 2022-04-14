# gleam_eval

> Chain together stateful computations in Gleam, and defer their evaluation until
you need them.

[![Package Version](https://img.shields.io/hexpm/v/eval)](https://hex.pm/packages/eval)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/eval/)

Gleam is an immutable functional programming language, but sometimes it would be
really helpful to model some mutable state that can be threaded through multiple
computations. That's where `Eval` comes in!

Everything in this package is built around the idea of an `Eval` type, which can
be summarised as:

```gleam
fn (context) -> #( context, Result(a, e) )
```

That is, a function that takes some `context` and returns a (potentially updated)
`context` and a `Result` of some `a` or some error `e`. With this, we can model
all manner of computations that can be expressed in a purely functional way: we
can update the context, throw an error, return a value, or some combination!

---

## Quick start

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

## Installation

If available on Hex this package can be added to your Gleam project:

```sh
gleam add eval
```

and its documentation can be found at <https://hexdocs.pm/eval>.
