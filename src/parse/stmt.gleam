import parse/expr.{type Expr}

pub type Stmt {
  Expression(expr: Expr)
  Print(expr: Expr)
}
