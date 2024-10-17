import gleam/option.{type Option}
import parse/token.{type Token}

pub type Expr {
  // Literal
  Number(value: Float)
  String(value: String)
  Boolean(value: Bool)
  NilLiteral

  // Unary
  NegativeNumber(value: Expr, token: Token)
  NegativeBool(value: Expr, token: Token)

  Assign(name: Token, value: Expr)

  Binary(left: Expr, op: Token, right: Expr)
  Grouping(expr: Option(Expr))
  Variable(name: Token)
}
