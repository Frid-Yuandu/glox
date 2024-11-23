import gleam/option.{type Option}
import parse/expr.{type Expr}
import parse/token.{type Token}

pub type Stmt {
  If(condition: Expr, then_branch: Stmt, else_branch: Option(Stmt))
  Block(statements: List(Stmt))
  Expression(expr: Expr)
  Print(expr: Expr)
  Declaration(name: Token, initializer: Option(Expr))
  While(condition: Expr, body: Stmt)
  EmptyExpression
  Break
  Continue
}
