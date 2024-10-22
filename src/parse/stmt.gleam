import gleam/option.{type Option}
import parse/expr.{type Expr}
import parse/token.{type Token}

pub type Stmt {
  Block(statements: List(Stmt))
  Expression(expr: Expr)
  Print(expr: Expr)
  Declaration(name: Token, initializer: Option(Expr))
}
