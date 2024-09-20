import gleam/bool
import gleam/float
import gleam/io
import gleam/list
import gleam/option.{None, Some}

import parse/token

import expr
import parse/lexer

pub fn inspect(expr: expr.Expr) -> String {
  io.debug(expr)
  case expr {
    expr.Literal(value) ->
      case value {
        expr.Number(n) -> float.to_string(n)
        expr.String(s) -> s
        expr.Bool(b) -> bool.to_string(b)
        expr.NilLiteral -> "nil"
      }
    expr.Unary(op, right) ->
      "(" <> token.to_lexeme(op.type_) <> inspect(right) <> ")"
    expr.Binary(left, op, right) ->
      "("
      <> token.to_lexeme(op.type_)
      <> " "
      <> inspect(left)
      <> " "
      <> inspect(right)
      <> ")"
    expr.Grouping(e) -> {
      let str = case e {
        Some(e) -> inspect(e)
        None -> "none"
      }
      "(" <> "gourp" <> " " <> str <> ")"
    }
  }
}

pub fn print_errors(tokens: List(lexer.LexicalError)) -> Nil {
  tokens
  |> list.map(lexer.inspect_error)
  |> list.each(io.println_error)
}
