import gleam/bool
import gleam/float
import gleam/io
import gleam/option.{None, Some}

import interpreter/runtime_error
import interpreter/types
import parse/error
import parse/expr
import parse/token

pub fn inspect_ast(expr: expr.Expr) -> String {
  case expr {
    expr.Variable(name) -> token.to_lexeme(name.type_)

    expr.Number(n) -> float.to_string(n)
    expr.String(s) -> s
    expr.Boolean(b) -> bool.to_string(b)
    expr.NilLiteral -> "nil"

    expr.NegativeBool(token: op, value: right) ->
      "(" <> token.to_lexeme(op.type_) <> inspect_ast(right) <> ")"
    expr.NegativeNumber(token: op, value: right) ->
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
    expr.Assign(name, e) ->
      "("
      <> "let"
      <> " "
      <> token.to_lexeme(name.type_)
      <> inspect_ast(e)
      <> ")"
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
