import birdie
import gleam/dict
import gleam/option.{None, Some}
import pprint

import interpreter.{type Interpreter, Interpreter}
import interpreter/environment.{Environment}
import interpreter/io_controller
import parse/expr
import parse/stmt
import parse/token.{Token}

// TODO: combine none output

pub fn should_interpret_literal_test() {
  let given_expr = [
    stmt.Expression(expr.Literal(expr.String("test\tstring"))),
    stmt.Expression(expr.Literal(expr.Bool(False))),
    stmt.Expression(expr.Literal(expr.Number(1.0))),
    stmt.Expression(expr.Literal(expr.NilLiteral)),
  ]

  new_test_interpreter()
  |> interpreter.interpret(given_expr)
  |> snapshot(title: "literal expression statements")
}

// test unary expressions

pub fn should_interpret_apply_bang_on_false_as_true_test() {
  let given_expr = [
    stmt.Expression(expr.Unary(
      Token(token.Bang, 1),
      expr.Literal(expr.Bool(False)),
    )),
    stmt.Expression(expr.Unary(
      Token(token.Bang, 1),
      expr.Literal(expr.NilLiteral),
    )),
    stmt.Expression(expr.Unary(
      Token(token.Bang, 1),
      expr.Literal(expr.Number(23_333.0)),
    )),
  ]

  new_test_interpreter()
  |> interpreter.interpret(given_expr)
  |> snapshot("unary expression statements")
}

// test binary expressions

pub fn should_interpret_binary_calculations_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(3.0)),
      Token(token.Plus, 1),
      expr.Literal(expr.Number(4.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.String("foo")),
      Token(token.Plus, 1),
      expr.Literal(expr.String("bar")),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(7.0)),
      Token(token.Minus, 1),
      expr.Literal(expr.Number(3.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(3.0)),
      Token(token.Star, 1),
      expr.Literal(expr.Number(2.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(10.0)),
      Token(token.Slash, 1),
      expr.Literal(expr.Number(2.0)),
    )),
  ]

  new_test_interpreter()
  |> interpreter.interpret(given_expr)
  |> snapshot("binary calculations expression statements")
}

pub fn should_interpret_equal_equal_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(10.0)),
      Token(token.Greater, 1),
      expr.Literal(expr.Number(5.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(5.0)),
      Token(token.GreaterEqual, 1),
      expr.Literal(expr.Number(5.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(2.0)),
      Token(token.Less, 1),
      expr.Literal(expr.Number(5.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(5.0)),
      Token(token.LessEqual, 1),
      expr.Literal(expr.Number(5.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(5.0)),
      Token(token.NotEqual, 1),
      expr.Literal(expr.Number(3.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(5.0)),
      Token(token.EqualEqual, 1),
      expr.Literal(expr.Number(5.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(10.0)),
      Token(token.Greater, 1),
      expr.Literal(expr.Number(5.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(5.0)),
      Token(token.GreaterEqual, 1),
      expr.Literal(expr.Number(5.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(2.0)),
      Token(token.Less, 1),
      expr.Literal(expr.Number(5.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(5.0)),
      Token(token.LessEqual, 1),
      expr.Literal(expr.Number(5.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(5.0)),
      Token(token.NotEqual, 1),
      expr.Literal(expr.Number(3.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(5.0)),
      Token(token.EqualEqual, 1),
      expr.Literal(expr.Number(5.0)),
    )),
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(5.0)),
      Token(token.EqualEqual, 1),
      expr.Literal(expr.Number(5.0)),
    )),
  ]

  new_test_interpreter()
  |> interpreter.interpret(given_expr)
  |> snapshot("equalities expression statements")
}

// error cases in binary expressions

pub fn should_error_on_add_number_and_non_number_types_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(1.0)),
      Token(token.Plus, 1),
      expr.Literal(expr.Bool(True)),
    )),
  ]

  new_test_interpreter()
  |> interpreter.interpret(given_expr)
  |> snapshot("error in add number with non-number")
}

pub fn should_error_in_addition_non_number_or_string_types_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.NilLiteral),
      Token(token.Plus, 1),
      expr.Literal(expr.Bool(False)),
    )),
  ]

  new_test_interpreter()
  |> interpreter.interpret(given_expr)
  |> snapshot("error in add not number or string type")
}

pub fn should_error_on_addition_string_and_non_string_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.String("foo")),
      Token(token.Plus, 1),
      expr.Literal(expr.Number(1.0)),
    )),
  ]

  new_test_interpreter()
  |> interpreter.interpret(given_expr)
  |> snapshot("snapshot for binary calculations")
}

pub fn should_error_on_subtraction_non_numbers_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.String("foo")),
      Token(token.Minus, 1),
      expr.Literal(expr.Number(1.0)),
    )),
  ]

  new_test_interpreter()
  |> interpreter.interpret(given_expr)
  |> snapshot("snapshot for binary calculations")
}

pub fn should_error_in_division_by_zero_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(10.0)),
      Token(token.Slash, 1),
      expr.Literal(expr.Number(0.0)),
    )),
  ]

  new_test_interpreter()
  |> interpreter.interpret(given_expr)
  |> snapshot("snapshot for binary calculations")
}

// test grouping expressions

pub fn should_interpret_grouping_expression_test() {
  let given_expr = [
    stmt.Expression(
      expr.Grouping(
        Some(expr.Binary(
          expr.Literal(expr.Number(3.0)),
          Token(token.Plus, 1),
          expr.Literal(expr.Number(4.0)),
        )),
      ),
    ),
  ]

  new_test_interpreter()
  |> interpreter.interpret(given_expr)
  |> snapshot("snapshot for binary calculations")
}

pub fn should_interpret_empty_grouping_test() {
  let given_expr = [stmt.Expression(expr.Grouping(None))]

  new_test_interpreter()
  |> interpreter.interpret(given_expr)
  |> snapshot("snapshot for binary calculations")
}

// helper
fn new_test_interpreter() -> Interpreter(fn() -> String) {
  Interpreter(env: Environment(dict.new()), io: io_controller.io_capturer())
}

fn snapshot(any: a, title title: String) {
  pprint.format(any)
  |> birdie.snap(title)
}
