import gleam/bool
import gleam/float
import gleam/option.{None, Some}
import parse/token

import expr

pub fn inspect(expr: expr.Expr) -> String {
  case expr {
    expr.Literal(value) ->
      case value {
        expr.Number(n) -> float.to_string(n)
        expr.String(s) -> s
        expr.Bool(b) -> bool.to_string(b)
        expr.NilLiteral(_) -> "nil"
      }
    expr.Unary(op, right) ->
      "(" <> token.to_lexeme(op.token_type) <> inspect(right) <> ")"
    expr.Binary(left, op, right) ->
      "("
      <> token.to_lexeme(op.token_type)
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
