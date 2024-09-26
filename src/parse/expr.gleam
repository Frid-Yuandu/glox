import gleam/option.{type Option}
import parse/token.{type Token}

pub type Expr {
  Literal(value: LiteralValue)
  Unary(op: Token, right: Expr)
  Binary(left: Expr, op: Token, right: Expr)
  Grouping(expr: Option(Expr))
}

pub type LiteralValue {
  Number(value: Float)
  String(value: String)
  Bool(value: Bool)
  NilLiteral
}
