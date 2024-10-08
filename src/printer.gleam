import gleam/bool
import gleam/float
import gleam/io
import gleam/option.{None, Some}
import parse/error

import parse/expr
import parse/token
import runtime_error
import types

pub fn inspect_ast(expr: expr.Expr) -> String {
  case expr {
    expr.Literal(value) ->
      case value {
        expr.Number(n) -> float.to_string(n)
        expr.String(s) -> s
        expr.Bool(b) -> bool.to_string(b)
        expr.NilLiteral -> "nil"
      }
    expr.Unary(op, right) ->
      "(" <> token.to_lexeme(op.type_) <> inspect_ast(right) <> ")"
    expr.Binary(left, op, right) ->
      "("
      <> token.to_lexeme(op.type_)
      <> " "
      <> inspect_ast(left)
      <> " "
      <> inspect_ast(right)
      <> ")"
    expr.Grouping(e) -> {
      let str = case e {
        Some(e) -> inspect_ast(e)
        None -> "none"
      }
      "(" <> "gourp" <> " " <> str <> ")"
    }
  }
}

pub fn print_parse_error(err: error.ParseError) -> Nil {
  error.inspect_parse_error(err)
  |> io.println_error
}

pub fn print_runtime_error(err: runtime_error.RuntimeError) -> Nil {
  runtime_error.inspect_runtime_error(err)
  |> io.println_error
}

pub fn print_evaluated_object(obj: types.Object) -> Nil {
  types.inspect_object(obj)
  |> io.println
}
