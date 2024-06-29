import eval
import examples/expr.{Add, Let, Num, UndefinedVariable, Var, eval}
import examples/parser
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn expr_test() {
  let expect = fn(expected, expr) {
    eval(expr)
    |> eval.run([])
    |> should.equal(expected)
  }

  // First let's make sure the context is modified in let bindings to include the
  // newly defined variables.
  expect(Ok(5.0), Let("x", be: Num(2.0), in: Add(Var("x"), Num(3.0))))

  // Then, we'll sanity-check and make sure nested let bindings also continue to
  // push new variables into the context.
  expect(
    Ok(5.0),
    Let(
      "x",
      be: Num(2.0),
      in: Let("y", be: Num(3.0), in: Add(Var("x"), Var("y"))),
    ),
  )

  // This test demonstrates that expressions properly error-out during evaluation.
  expect(
    Error(UndefinedVariable("y")),
    Let("x", be: Num(2.0), in: Add(Var("x"), Var("y"))),
  )

  // Finally, let's make sure the context is modified in the correct order and/or
  // out-of-scope bindings are properly popped off the stack.
  expect(
    Error(UndefinedVariable("y")),
    Let(
      "x",
      be: Var("y"),
      in: Let("y", be: Num(2.0), in: Add(Var("x"), Var("y"))),
    ),
  )
}

pub fn parser_test() {
  let expect = fn(input, expected, parser) {
    parser.run(input, parser)
    |> should.equal(expected)
  }

  expect("1 + 1", Ok(2), {
    use x <- eval.try(parser.int())
    use _ <- eval.try(parser.ws())
    use _ <- eval.try(parser.symbol("+"))
    use _ <- eval.try(parser.ws())
    use y <- eval.try(parser.int())

    eval.return(x + y)
  })
}
