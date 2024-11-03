import gleam/bool
import gleam/float
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result

import interpreter/environment.{type Environment, Environment}
import interpreter/io_controller.{type IOInterface}
import interpreter/runtime_error.{
  type RuntimeError, DivideByZero, OperandMustBeNumber,
  OperandMustBeNumberOrString, OperandMustBeString, RuntimeError,
}
import interpreter/types.{type Object, BoolVal, NilVal, Num, Str}
import parse/expr.{
  type Expr, Binary, Boolean, Grouping, NegativeBool, NegativeNumber, NilLiteral,
  Number, String, Variable,
}
import parse/stmt.{type Stmt}
import parse/token.{type Token}
import prelude.{with_ok}

pub type Interpreter(output) {
  Interpreter(env: Environment, io: IOInterface(output))
}

pub fn new() -> Interpreter(Nil) {
  Interpreter(env: environment.new(), io: io_controller.default_io())
}

pub type EvalResult =
  Result(Object, RuntimeError)

// output represent the interpreter output. In common cases such as write to stdout/stderr
// it is nil, and in test environment it maybe fn() -> String.
pub type InterpretResult(output) =
  Result(Option(output), RuntimeError)

/// interpret interprets the provided statements and return the interpret result
/// of the last statement. This function will exhaust the statements unless a
/// runtime error is occured.
///
/// NOTE: Behaviour may change!
pub fn interpret(
  interpreter: Interpreter(output),
  statements: List(Stmt),
) -> #(InterpretResult(output), Interpreter(output)) {
  use #(_, interpreter), statement <- list.fold_until(statements, #(
    Ok(None),
    interpreter,
  ))

  case statement {
    stmt.Print(exp) -> {
      let #(rst, interpreter) = evaluate(interpreter, exp)
      case rst {
        Ok(obj) -> {
          let output = interpreter.io.write_stdout(types.inspect_object(obj))
          list.Continue(#(Ok(Some(output)), interpreter))
        }
        Error(err) -> list.Stop(#(Error(err), interpreter))
      }
    }
    stmt.Expression(exp) -> {
      let #(rst, interpreter) = evaluate(interpreter, exp)
      case rst {
        Ok(_) -> list.Continue(#(Ok(None), interpreter))
        Error(err) -> list.Stop(#(Error(err), interpreter))
      }
    }
    stmt.Declaration(name, initializer) -> {
      let rst = case initializer {
        Some(e) -> evaluate(interpreter, e) |> pair.first
        None -> Ok(NilVal)
      }
      case rst {
        Ok(value) -> {
          let interpreter = define_variable(interpreter, name, value)
          list.Continue(#(Ok(None), interpreter))
        }
        Error(err) -> list.Stop(#(Error(err), interpreter))
      }
    }

    stmt.Block(statements) -> {
      let block_env = environment.new_scope(interpreter.env)
      let #(rst, interpreter) =
        interpret(Interpreter(..interpreter, env: block_env), statements)

      let assert Some(env) = interpreter.env.enclosing
      let interpreter = Interpreter(..interpreter, env:)
      case rst {
        Error(_) -> list.Stop(#(rst, interpreter))
        Ok(_) -> list.Continue(#(rst, interpreter))
      }
    }

    stmt.If(cond, then_branch, maybe_else) -> {
      let #(cond_rst, interpreter) = evaluate(interpreter, cond)
      let #(rst, interpreter) = {
        use obj, interpreter <- with_ok(in: cond_rst, processer: interpreter)
        case types.is_truthy(obj), maybe_else {
          True, _ -> interpret(interpreter, [then_branch])
          False, Some(else_branch) -> interpret(interpreter, [else_branch])
          False, None -> #(Ok(None), interpreter)
        }
      }
      case rst {
        Error(_) -> list.Stop(#(rst, interpreter))
        Ok(_) -> list.Continue(#(rst, interpreter))
      }
    }
  }
}

fn define_variable(
  interpreter: Interpreter(a),
  name: Token,
  value,
) -> Interpreter(a) {
  let env = environment.define(interpreter.env, name, value)
  Interpreter(..interpreter, env:)
}

pub fn evaluate(
  interpreter: Interpreter(output),
  exp: Expr,
) -> #(EvalResult, Interpreter(output)) {
  case exp {
    expr.Variable(tok) -> #(environment.get(interpreter.env, tok), interpreter)

    expr.Number(n) -> #(Ok(Num(n)), interpreter)
    expr.String(s) -> #(Ok(Str(s)), interpreter)
    expr.Boolean(b) -> #(Ok(BoolVal(b)), interpreter)
    expr.NilLiteral -> #(Ok(NilVal), interpreter)

    expr.NegativeBool(right, _) -> {
      use right_val, interpreter <- try_evaluate(interpreter, right)

      #(Ok(BoolVal(!types.is_truthy(right_val))), interpreter)
    }

    expr.NegativeNumber(right, tok) -> {
      use right_val, interpreter <- try_evaluate(interpreter, right)

      case right_val {
        Num(n) -> #(Ok(Num(float.negate(n))), interpreter)
        _ -> #(
          Error(RuntimeError(token: tok, error: OperandMustBeNumber)),
          interpreter,
        )
      }
    }

    expr.LogicOr(left, _tok, right) -> {
      let #(left_rst, interpreter) = evaluate(interpreter, left)
      use left_obj, interpreter <- with_ok(left_rst, interpreter)
      case types.is_truthy(left_obj) {
        True -> #(left_rst, interpreter)
        False -> evaluate(interpreter, right)
      }
    }
    expr.LogicAnd(left, _tok, right) -> {
      let #(left_rst, interpreter) = evaluate(interpreter, left)
      use left_obj, interpreter <- with_ok(left_rst, interpreter)
      case types.is_truthy(left_obj) {
        False -> #(left_rst, interpreter)
        True -> evaluate(interpreter, right)
      }
    }

    expr.Binary(left, op, right) ->
      interpreter |> evaluate_binary(left, op, right)

    expr.Grouping(grouped) ->
      case grouped {
        None -> #(Ok(NilVal), interpreter)
        Some(e) -> evaluate(interpreter, e)
      }

    expr.Assign(name, expr) -> {
      use obj, interpreter <- try_evaluate(interpreter, expr)

      case environment.assign(interpreter.env, name, obj) {
        Ok(env) -> #(Ok(obj), Interpreter(..interpreter, env:))
        Error(err) -> #(Error(err), interpreter)
      }
    }
  }
}

fn evaluate_binary(
  interpreter: Interpreter(output),
  left: Expr,
  op: Token,
  right: Expr,
) -> #(EvalResult, Interpreter(output)) {
  use left_val, interpreter <- try_evaluate(interpreter, left)
  use right_val, interpreter <- try_evaluate(interpreter, right)

  case op.type_ {
    token.Minus ->
      case left_val, right_val {
        Num(l), Num(r) -> #(Ok(Num(l -. r)), interpreter)
        _, _ -> #(Error(RuntimeError(op, OperandMustBeNumber)), interpreter)
      }
    token.Slash ->
      case left_val, right_val {
        Num(_), Num(0.0) -> #(
          Error(RuntimeError(op, DivideByZero)),
          interpreter,
        )
        Num(l), Num(r) -> #(Ok(Num(l /. r)), interpreter)
        _, _ -> #(Error(RuntimeError(op, OperandMustBeNumber)), interpreter)
      }
    token.Star ->
      case left_val, right_val {
        Num(l), Num(r) -> #(Ok(Num(l *. r)), interpreter)
        _, _ -> #(Error(RuntimeError(op, OperandMustBeNumber)), interpreter)
      }
    token.Plus ->
      case left_val, right_val {
        Num(l), Num(r) -> #(Ok(Num(l +. r)), interpreter)
        // Lox support string concate using `+`.
        // Overload `+` operator when both operands are string.
        Str(l), Str(r) -> #(Ok(Str(l <> r)), interpreter)

        Num(_), _ -> #(
          Error(RuntimeError(op, OperandMustBeNumber)),
          interpreter,
        )
        Str(_), _ -> #(
          Error(RuntimeError(op, OperandMustBeString)),
          interpreter,
        )
        _, _ -> #(
          Error(RuntimeError(op, OperandMustBeNumberOrString)),
          interpreter,
        )
      }
    token.Greater ->
      case left_val, right_val {
        Num(l), Num(r) -> #(Ok(BoolVal(l >. r)), interpreter)
        _, _ -> #(Error(RuntimeError(op, OperandMustBeNumber)), interpreter)
      }
    token.GreaterEqual ->
      case left_val, right_val {
        Num(l), Num(r) -> #(Ok(BoolVal(l >=. r)), interpreter)
        _, _ -> #(Error(RuntimeError(op, OperandMustBeNumber)), interpreter)
      }
    token.Less ->
      case left_val, right_val {
        Num(l), Num(r) -> #(Ok(BoolVal(l <. r)), interpreter)
        _, _ -> #(Error(RuntimeError(op, OperandMustBeNumber)), interpreter)
      }
    token.LessEqual ->
      case left_val, right_val {
        Num(l), Num(r) -> #(Ok(BoolVal(l <=. r)), interpreter)
        _, _ -> #(Error(RuntimeError(op, OperandMustBeNumber)), interpreter)
      }
    token.NotEqual -> #(
      Ok(BoolVal(!types.is_equal(left_val, right_val))),
      interpreter,
    )
    token.EqualEqual -> #(
      Ok(BoolVal(types.is_equal(left_val, right_val))),
      interpreter,
    )

    _ -> panic
  }
}

fn try_evaluate(
  interpreter: Interpreter(output),
  expr: Expr,
  callback: fn(Object, Interpreter(output)) ->
    #(Result(a, RuntimeError), Interpreter(output)),
) -> #(Result(a, RuntimeError), Interpreter(output)) {
  let #(rst, interpreter) = evaluate(interpreter, expr)
  use <- bool.lazy_guard(result.is_error(rst), fn() {
    let assert Error(err) = rst
    #(Error(err), interpreter)
  })
  let assert Ok(value) = rst

  callback(value, interpreter)
}
