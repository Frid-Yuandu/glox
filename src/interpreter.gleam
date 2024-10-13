import gleam/bool
import gleam/dict
import gleam/float
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

import interpreter/environment.{type Environment, Environment}
import interpreter/io_controller.{type IOInterface}
import interpreter/runtime_error.{
  type RuntimeError, DivideByZero, OperandMustBeNumber,
  OperandMustBeNumberOrString, OperandMustBeString, RuntimeError,
}
import interpreter/types.{type Object, BoolVal, Class, NilVal, Num, Str}
import parse/expr.{
  type Expr, Binary, Boolean, Grouping, NegativeBool, NegativeNumber, NilLiteral,
  Number, String, Variable,
}
import parse/stmt.{type Stmt}
import parse/token

pub type Interpreter(a) {
  Interpreter(env: Environment, io: IOInterface(a))
}

pub fn new() -> Interpreter(Nil) {
  Interpreter(env: Environment(dict.new()), io: io_controller.default_io())
}

pub type EvalResult =
  Result(Object, RuntimeError)

// a represent the interpreter output. In common cases such as write to stdout/stderr
// it is nil, and in test environment it maybe fn() -> String.
pub type InterpretResult(a) =
  Result(Option(a), RuntimeError)

/// interpret interprets the provided statements and return the interpret result
/// of the last statement. This function will exhaust the statements unless a
/// runtime error is occured.
///
/// NOTE: Behaviour may change!
pub fn interpret(
  interpreter: Interpreter(a),
  statements: List(Stmt),
) -> InterpretResult(a) {
  use _acc, statement <- list.fold_until(statements, Ok(None))

  case statement {
    stmt.Print(exp) -> {
      case evaluate(exp) {
        Ok(obj) -> {
          let output =
            types.inspect_object(obj)
            |> interpreter.io.write_stdout

          list.Continue(Ok(Some(output)))
        }
        Error(err) -> list.Stop(Error(err))
      }
    }
    stmt.Expression(exp) -> {
      case evaluate(exp) {
        Ok(_) -> list.Continue(Ok(None))
        Error(err) -> list.Stop(Error(err))
      }
    }
    stmt.Declaration(_, _) -> todo
  }
}

pub fn evaluate(exp: Expr) -> EvalResult {
  case exp {
    expr.Variable(_) -> todo

    expr.Number(n) -> Ok(Num(n))
    expr.String(s) -> Ok(Str(s))
    expr.Boolean(b) -> Ok(BoolVal(b))
    expr.NilLiteral -> Ok(NilVal)

    expr.NegativeBool(right, _) -> {
      let right_val = evaluate(right)
      use right_val <- result.try(right_val)
      Ok(BoolVal(!is_truthy(right_val)))
    }
    expr.NegativeNumber(right, tok) -> {
      let right_val = evaluate(right)
      use right_val <- result.try(right_val)
      case right_val {
        Num(n) -> Ok(Num(float.negate(n)))
        _ -> Error(RuntimeError(token: tok, error: OperandMustBeNumber))
      }
    }

    expr.Binary(left, op, right) -> {
      let left_val = evaluate(left)
      let right_val = evaluate(right)

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
            Num(l), Num(r) -> Ok(BoolVal(l >. r))
            _, _ -> Error(RuntimeError(op, OperandMustBeNumber))
          }
        token.GreaterEqual ->
          case left_val, right_val {
            Num(l), Num(r) -> Ok(BoolVal(l >=. r))
            _, _ -> Error(RuntimeError(op, OperandMustBeNumber))
          }
        token.Less ->
          case left_val, right_val {
            Num(l), Num(r) -> Ok(BoolVal(l <. r))
            _, _ -> Error(RuntimeError(op, OperandMustBeNumber))
          }
        token.LessEqual ->
          case left_val, right_val {
            Num(l), Num(r) -> Ok(BoolVal(l <=. r))
            _, _ -> Error(RuntimeError(op, OperandMustBeNumber))
          }
        token.NotEqual -> Ok(BoolVal(!is_equal(left_val, right_val)))
        token.EqualEqual -> Ok(BoolVal(is_equal(left_val, right_val)))

        _ -> panic
      }
    }
    expr.Grouping(grouped) ->
      case grouped {
        None -> Ok(NilVal)
        Some(e) -> evaluate(e)
      }
  }
}

/// is_truthy converts any object to boolean. As in Ruby's rule,
/// `false` and `nil` are falsey, and everything else are truthy.
fn is_truthy(obj: Object) -> Bool {
  case obj {
    BoolVal(v) -> v
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
    BoolVal(l), BoolVal(r) -> l == r
    Class(l_name, l_fields), Class(r_name, r_fields) ->
      l_name == r_name && l_fields == r_fields
    _, _ -> False
  }
}
