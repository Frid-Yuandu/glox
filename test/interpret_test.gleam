import gleam/option.{None, Some}
import gleeunit/should
import parse/stmt

import interpreter
import interpreter/runtime_error.{
  DivideByZero, OperandMustBeNumber, OperandMustBeNumberOrString,
  OperandMustBeString, RuntimeError,
}
import interpreter/types
import parse/expr
import parse/token.{Token}

pub fn should_interpret_literal_number_test() {
  let given_expr = [stmt.Expression(expr.Literal(expr.Number(1.0)))]
  let wanted = Ok(types.Num(1.0))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_literal_string_test() {
  let given_expr = [stmt.Expression(expr.Literal(expr.String("test string")))]
  let wanted = Ok(types.Str("test string"))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_literal_string_escape_test() {
  let given_expr = [stmt.Expression(expr.Literal(expr.String("\t")))]
  let wanted = Ok(types.Str("\t"))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_literal_boolean_test() {
  let given_expr = [stmt.Expression(expr.Literal(expr.Bool(False)))]
  let wanted = Ok(types.Boolean(False))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_literal_nil_test() {
  let given_expr = [stmt.Expression(expr.Literal(expr.NilLiteral))]
  let wanted = Ok(types.NilVal)

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

// test unary expressions

pub fn should_interpret_apply_bang_on_false_as_true_test() {
  let given_expr = [
    stmt.Expression(expr.Unary(
      Token(token.Bang, 1),
      expr.Literal(expr.Bool(False)),
    )),
  ]
  let wanted = Ok(types.Boolean(True))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_apply_bang_on_nil_as_true_test() {
  let given_expr = [
    stmt.Expression(expr.Unary(
      Token(token.Bang, 1),
      expr.Literal(expr.NilLiteral),
    )),
  ]
  let wanted = Ok(types.Boolean(True))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_apply_bang_on_truthy_as_false_test() {
  let given_expr = [
    stmt.Expression(expr.Unary(
      Token(token.Bang, 1),
      expr.Literal(expr.Number(23_333.0)),
    )),
  ]
  let wanted = Ok(types.Boolean(False))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

// test binary expressions

pub fn should_interpret_addition_numbers_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(3.0)),
      Token(token.Plus, 1),
      expr.Literal(expr.Number(4.0)),
    )),
  ]
  let wanted = Ok(types.Num(7.0))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_addition_strings_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.String("foo")),
      Token(token.Plus, 1),
      expr.Literal(expr.String("bar")),
    )),
  ]
  let wanted = Ok(types.Str("foobar"))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_subtraction_numbers_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(7.0)),
      Token(token.Minus, 1),
      expr.Literal(expr.Number(3.0)),
    )),
  ]
  let wanted = Ok(types.Num(4.0))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_multiplication_numbers_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(3.0)),
      Token(token.Star, 1),
      expr.Literal(expr.Number(2.0)),
    )),
  ]
  let wanted = Ok(types.Num(6.0))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_division_numbers_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(10.0)),
      Token(token.Slash, 1),
      expr.Literal(expr.Number(2.0)),
    )),
  ]
  let wanted = Ok(types.Num(5.0))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_greater_than_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(10.0)),
      Token(token.Greater, 1),
      expr.Literal(expr.Number(5.0)),
    )),
  ]
  let wanted = Ok(types.Boolean(True))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_greater_than_equal_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(5.0)),
      Token(token.GreaterEqual, 1),
      expr.Literal(expr.Number(5.0)),
    )),
  ]
  let wanted = Ok(types.Boolean(True))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_less_than_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(2.0)),
      Token(token.Less, 1),
      expr.Literal(expr.Number(5.0)),
    )),
  ]
  let wanted = Ok(types.Boolean(True))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_less_than_equal_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(5.0)),
      Token(token.LessEqual, 1),
      expr.Literal(expr.Number(5.0)),
    )),
  ]
  let wanted = Ok(types.Boolean(True))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_not_equal_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(5.0)),
      Token(token.NotEqual, 1),
      expr.Literal(expr.Number(3.0)),
    )),
  ]
  let wanted = Ok(types.Boolean(True))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_equal_equal_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(5.0)),
      Token(token.EqualEqual, 1),
      expr.Literal(expr.Number(5.0)),
    )),
  ]
  let wanted = Ok(types.Boolean(True))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
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
  let wanted = Error(RuntimeError(Token(token.Plus, 1), OperandMustBeNumber))

  interpreter.interpret(given_expr)
  |> should.equal(wanted)
}

pub fn should_error_on_addition_non_number_or_string_types_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.NilLiteral),
      Token(token.Plus, 1),
      expr.Literal(expr.Bool(False)),
    )),
  ]
  let wanted =
    Error(RuntimeError(Token(token.Plus, 1), OperandMustBeNumberOrString))

  interpreter.interpret(given_expr)
  |> should.equal(wanted)
}

pub fn should_error_on_addition_string_and_non_string_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.String("foo")),
      Token(token.Plus, 1),
      expr.Literal(expr.Number(1.0)),
    )),
  ]
  let wanted = Error(RuntimeError(Token(token.Plus, 1), OperandMustBeString))

  interpreter.interpret(given_expr)
  |> should.equal(wanted)
}

pub fn should_error_on_subtraction_non_numbers_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.String("foo")),
      Token(token.Minus, 1),
      expr.Literal(expr.Number(1.0)),
    )),
  ]
  let wanted = Error(RuntimeError(Token(token.Minus, 1), OperandMustBeNumber))

  interpreter.interpret(given_expr)
  |> should.equal(wanted)
}

pub fn should_error_on_division_by_zero_test() {
  let given_expr = [
    stmt.Expression(expr.Binary(
      expr.Literal(expr.Number(10.0)),
      Token(token.Slash, 1),
      expr.Literal(expr.Number(0.0)),
    )),
  ]
  let wanted = Error(RuntimeError(Token(token.Slash, 1), DivideByZero))

  interpreter.interpret(given_expr)
  |> should.equal(wanted)
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
  let wanted = Ok(types.Num(7.0))

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}

pub fn should_interpret_empty_grouping_test() {
  let given_expr = [stmt.Expression(expr.Grouping(None))]
  let wanted = Ok(types.NilVal)

  interpreter.interpret(given_expr)
  |> should.equal(Ok(Nil))
}
