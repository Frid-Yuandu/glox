import birdie
import gleam/option.{None, Some}
import pprint

import interpreter.{type Interpreter, Interpreter}
import interpreter/environment
import interpreter/io_controller
import parse/expr
import parse/stmt
import parse/token.{Token}

// TODO: combine none output

pub fn should_interpret_literal_test() {
  let given_expr = [
    stmt.Expression(expr.String("test\tstring")),
    stmt.Expression(expr.Boolean(False)),
    stmt.Expression(expr.Number(1.0)),
    stmt.Expression(expr.NilLiteral),
  ]

  let #(rst, _) =
    new_test_interpreter()
    |> interpreter.interpret(given_expr)

  snapshot(rst, title: "literal expression statements")
}

// test unary expressions

pub fn should_interpret_apply_bang_on_false_as_true_test() {
  let given_expr = [
    stmt.Expression(expr.NegativeBool(
      token: Token(token.Bang, 1),
      value: expr.Boolean(False),
    )),
    stmt.Expression(expr.NegativeBool(
      token: Token(token.Bang, 1),
      value: expr.NilLiteral,
    )),
    stmt.Expression(expr.NegativeBool(
      token: Token(token.Bang, 1),
      value: expr.Number(23_333.0),
    )),
  ]

  let #(rst, _) =
    new_test_interpreter()
    |> interpreter.interpret(given_expr)

  snapshot(rst, "unary expression statements")
}

// test binary expressions

pub fn should_interpret_binary_calculations_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Number(3.0),
      Token(token.Plus, 1),
      expr.Number(4.0),
    )),
    stmt.Expression(expr.Binary(
      expr.String("foo"),
      Token(token.Plus, 1),
      expr.String("bar"),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(7.0),
      Token(token.Minus, 1),
      expr.Number(3.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(3.0),
      Token(token.Star, 1),
      expr.Number(2.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(10.0),
      Token(token.Slash, 1),
      expr.Number(2.0),
    )),
  ]

  let #(rst, _) =
    new_test_interpreter()
    |> interpreter.interpret(given_expr)

  snapshot(rst, "binary calculations expression statements")
}

pub fn should_interpret_equal_equal_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Number(10.0),
      Token(token.Greater, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.GreaterEqual, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(2.0),
      Token(token.Less, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.LessEqual, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.NotEqual, 1),
      expr.Number(3.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.EqualEqual, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(10.0),
      Token(token.Greater, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.GreaterEqual, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(2.0),
      Token(token.Less, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.LessEqual, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.NotEqual, 1),
      expr.Number(3.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.EqualEqual, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.EqualEqual, 1),
      expr.Number(5.0),
    )),
  ]

  let #(rst, _) =
    new_test_interpreter()
    |> interpreter.interpret(given_expr)

  snapshot(rst, "equalities expression statements")
}

// error cases in binary expressions

pub fn should_error_on_add_number_and_non_number_types_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Number(1.0),
      Token(token.Plus, 1),
      expr.Boolean(True),
    )),
  ]

  let #(rst, _) =
    new_test_interpreter()
    |> interpreter.interpret(given_expr)

  snapshot(rst, "error in add number with non-number")
}

pub fn should_error_in_addition_non_number_or_string_types_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.NilLiteral,
      Token(token.Plus, 1),
      expr.Boolean(False),
    )),
  ]

  let #(rst, _) =
    new_test_interpreter()
    |> interpreter.interpret(given_expr)

  snapshot(rst, "error in add not number or string type")
}

pub fn should_error_on_addition_string_and_non_string_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.String("foo"),
      Token(token.Plus, 1),
      expr.Number(1.0),
    )),
  ]

  let #(rst, _) =
    new_test_interpreter()
    |> interpreter.interpret(given_expr)

  snapshot(rst, "error in add wrong type to string")
}

pub fn should_error_on_subtraction_non_numbers_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.String("foo"),
      Token(token.Minus, 1),
      expr.Number(1.0),
    )),
  ]

  let #(rst, _) =
    new_test_interpreter()
    |> interpreter.interpret(given_expr)

  snapshot(rst, "error in subtract wrong type to string")
}

pub fn should_error_in_division_by_zero_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Number(10.0),
      Token(token.Slash, 1),
      expr.Number(0.0),
    )),
  ]

  let #(rst, _) =
    new_test_interpreter()
    |> interpreter.interpret(given_expr)

  snapshot(rst, "error in divide by zero")
}

// test grouping expressions

pub fn should_interpret_grouping_expression_test() {
  let given_expr = [
    stmt.Expression(
      expr.Grouping(
        Some(expr.Binary(
          expr.Number(3.0),
          Token(token.Plus, 1),
          expr.Number(4.0),
        )),
      ),
    ),
  ]

  let #(rst, _) =
    new_test_interpreter()
    |> interpreter.interpret(given_expr)

  snapshot(rst, "non-empty gourping expression")
}

pub fn should_interpret_empty_grouping_test() {
  let given_expr = [stmt.Expression(expr.Grouping(None))]

  let #(rst, _) =
    new_test_interpreter()
    |> interpreter.interpret(given_expr)

  snapshot(rst, "empty grouping expression")
}

// helper
fn new_test_interpreter() -> Interpreter(fn() -> String) {
  Interpreter(env: environment.new(), io: io_controller.io_capturer())
}

fn snapshot(any: a, title title: String) {
  pprint.format(any)
  |> birdie.snap(title)
}
