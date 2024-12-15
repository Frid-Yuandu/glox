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

    expr.NegativeBool(token: op, value: right) -> inspect_unary(op, right)
    expr.NegativeNumber(token: op, value: right) -> inspect_unary(op, right)

    expr.LogicAnd(left, token, right) -> inspect_binary(token:, left:, right:)
    expr.LogicOr(left, token, right) -> inspect_binary(token:, left:, right:)

    expr.Binary(left, op, right) -> inspect_binary(token: op, left:, right:)
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
    expr.Call(_, _, _) -> todo
  }
}

fn inspect_unary(token: token.Token, value: expr.Expr) -> String {
  "(" <> token.to_lexeme(token.type_) <> inspect_ast(value) <> ")"
}

fn inspect_binary(
  token token: token.Token,
  left left: expr.Expr,
  right right: expr.Expr,
) -> String {
  "("
  <> token.to_lexeme(token.type_)
  <> " "
  <> inspect_ast(left)
  <> " "
  <> inspect_ast(right)
  <> ")"
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
