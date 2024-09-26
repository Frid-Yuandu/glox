import gleam/bool
import gleam/float
import gleam/option.{None, Some}
import gleam/result

import parse/expr.{type Expr}
import parse/token
import runtime_error.{
  type RuntimeError, DivideByZero, OperandMustBeNumber,
  OperandMustBeNumberOrString, OperandMustBeString, RuntimeError,
}
import types.{type Object, Boolean, Class, NilVal, Num, Str}

pub type Interpreter {
  Interpreter
}

pub type EvalResult =
  Result(Object, RuntimeError)

pub fn interpret(exp: Expr) -> EvalResult {
  evaluate(exp)
}

pub fn evaluate(exp: Expr) -> EvalResult {
  case exp {
    expr.Literal(literal) ->
      case literal {
        expr.Number(n) -> Ok(Num(n))
        expr.String(s) -> Ok(Str(s))
        expr.Bool(b) -> Ok(Boolean(b))
        expr.NilLiteral -> Ok(NilVal)
      }
    expr.Binary(left, op, right) -> {
      let left_val = evaluate(left)
      let right_val = evaluate(right)

      // use <- bool.guard(result.is_error(left_val), left_val)
      // use <- bool.guard(result.is_error(right_val), right_val)
      // let assert Ok(left_val) = left_val
      // let assert Ok(right_val) = right_val

      use left_val <- result.try(left_val)
      use right_val <- result.try(right_val)

      case op.type_ {
        token.Minus ->
          case left_val, right_val {
            Num(l), Num(r) -> Ok(Num(l -. r))
            _, _ -> Error(RuntimeError(op, OperandMustBeNumber))
          }
        token.Slash ->
          case left_val, right_val {
            Num(_), Num(0.0) -> Error(RuntimeError(op, DivideByZero))
            Num(l), Num(r) -> Ok(Num(l /. r))
            _, _ -> Error(RuntimeError(op, OperandMustBeNumber))
          }
        token.Star ->
          case left_val, right_val {
            Num(l), Num(r) -> Ok(Num(l *. r))
            _, _ -> Error(RuntimeError(op, OperandMustBeNumber))
          }
        token.Plus ->
          case left_val, right_val {
            Num(l), Num(r) -> Ok(Num(l +. r))
            // Lox support string concate using `+`.
            // Overload `+` operator when both operands are string.
            Str(l), Str(r) -> Ok(Str(l <> r))

            Num(_), _ -> Error(RuntimeError(op, OperandMustBeNumber))
            Str(_), _ -> Error(RuntimeError(op, OperandMustBeString))
            _, _ -> Error(RuntimeError(op, OperandMustBeNumberOrString))
          }
        token.Greater ->
          case left_val, right_val {
            Num(l), Num(r) -> Ok(Boolean(l >. r))
            _, _ -> Error(RuntimeError(op, OperandMustBeNumber))
          }
        token.GreaterEqual ->
          case left_val, right_val {
            Num(l), Num(r) -> Ok(Boolean(l >=. r))
            _, _ -> Error(RuntimeError(op, OperandMustBeNumber))
          }
        token.Less ->
          case left_val, right_val {
            Num(l), Num(r) -> Ok(Boolean(l <. r))
            _, _ -> Error(RuntimeError(op, OperandMustBeNumber))
          }
        token.LessEqual ->
          case left_val, right_val {
            Num(l), Num(r) -> Ok(Boolean(l <=. r))
            _, _ -> Error(RuntimeError(op, OperandMustBeNumber))
          }
        token.NotEqual -> Ok(Boolean(!is_equal(left_val, right_val)))
        token.EqualEqual -> Ok(Boolean(is_equal(left_val, right_val)))

        _ -> panic
      }
    }
    expr.Grouping(grouped) ->
      case grouped {
        None -> Ok(NilVal)
        Some(e) -> evaluate(e)
      }
    expr.Unary(op, right) -> {
      let right_val = evaluate(right)
      use <- bool.guard(result.is_error(right_val), right_val)
      let assert Ok(right_val) = right_val

      case op.type_ {
        token.Minus ->
          case right_val {
            Num(v) -> Ok(Num(float.negate(v)))
            _ -> Error(RuntimeError(token: op, error: OperandMustBeNumber))
          }
        token.Bang -> Ok(Boolean(!is_truthy(right_val)))
        _ -> panic
      }
    }
  }
}

/// is_truthy converts any object to boolean. As in Ruby's rule,
/// `false` and `nil` are falsey, and everything else are truthy.
fn is_truthy(obj: Object) -> Bool {
  case obj {
    Boolean(v) -> v
    NilVal -> False
    _ -> True
  }
}

/// is_equal provide ability to compare any types of value.
fn is_equal(left: Object, right: Object) -> Bool {
  case left, right {
    NilVal, NilVal -> True
    Num(l), Num(r) -> l == r
    Str(l), Str(r) -> l == r
    Boolean(l), Boolean(r) -> l == r
    Class(l_name, l_fields), Class(r_name, r_fields) ->
      l_name == r_name && l_fields == r_fields
    _, _ -> False
  }
}
