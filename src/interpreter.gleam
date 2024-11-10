import gleam/float
import gleam/option.{type Option, None, Some}

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
import parse/stmt.{type Stmt, Block, Declaration, Expression, If, Print, While}
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

// TODO: refactor the output return type

/// interpret interprets the provided statements and return the interpret result
/// of the last statement. This function will exhaust the statements unless a
/// runtime error is occured.
pub fn execute(
  interpreter: Interpreter(output),
  statements: List(Stmt),
) -> #(InterpretResult(output), Interpreter(output)) {
  execute_helper(interpreter, statements, Ok(None))
}

fn execute_helper(
  interpreter: Interpreter(output),
  statements: List(Stmt),
  last_result: InterpretResult(output),
) -> #(InterpretResult(output), Interpreter(output)) {
  case statements {
    [] -> #(last_result, interpreter)
    [Print(exp), ..rest] -> {
      let #(rst, interpreter) = evaluate(interpreter, exp)
      case rst {
        Ok(obj) -> {
          let output = interpreter.io.write_stdout(types.inspect_object(obj))
          execute_helper(interpreter, rest, Ok(Some(output)))
        }
        Error(err) -> #(Error(err), interpreter)
      }
    }
    [Expression(exp), ..rest] -> {
      let #(rst, interpreter) = evaluate(interpreter, exp)
      case rst {
        Ok(_) -> execute_helper(interpreter, rest, Ok(None))
        Error(err) -> #(Error(err), interpreter)
      }
    }
    [Declaration(name, initializer), ..rest] -> {
      // assign nil to variables without initializer
      let #(rst, interpreter) = case initializer {
        Some(e) -> evaluate(interpreter, e)
        None -> #(Ok(NilVal), interpreter)
      }
      use value, interpreter <- with_ok(in: rst, processer: interpreter)
      let interpreter = define_variable(interpreter, name, value)

      execute_helper(interpreter, rest, Ok(None))
    }

    [Block(statements), ..rest] -> {
      let #(rst, interpreter) = execute_block(interpreter, statements)
      case rst {
        Error(_) -> #(rst, interpreter)
        Ok(_) -> execute_helper(interpreter, rest, rst)
      }
    }

    [If(cond, then_branch, maybe_else), ..rest] -> {
      let #(rst, interpreter) =
        execute_if(interpreter, cond, then_branch, maybe_else)
      case rst {
        Error(_) -> #(rst, interpreter)
        Ok(_) -> execute_helper(interpreter, rest, rst)
      }
    }
    [While(cond, body), ..rest] -> {
      let #(rst, interpreter) = execute_while(interpreter, cond, body)
      case rst {
        Error(_) -> #(rst, interpreter)
        Ok(_) -> execute_helper(interpreter, rest, rst)
      }
    }
  }
}

// TODO: jump out of block safely, handle none enclosing case and return error
fn execute_block(
  interpreter: Interpreter(output),
  statements: List(Stmt),
) -> #(InterpretResult(output), Interpreter(output)) {
  let #(rst, interpreter) =
    dive_into_block(interpreter)
    |> execute_helper(statements, Ok(None))

  let assert Some(env) = interpreter.env.enclosing
  let interpreter = Interpreter(..interpreter, env:)
  #(rst, interpreter)
}

fn execute_if(
  interpreter: Interpreter(output),
  cond: Expr,
  then_branch: Stmt,
  maybe_else: Option(Stmt),
) -> #(InterpretResult(output), Interpreter(output)) {
  let #(cond_rst, interpreter) = evaluate(interpreter, cond)

  use obj, interpreter <- with_ok(in: cond_rst, processer: interpreter)
  case types.is_truthy(obj), maybe_else {
    True, _ -> execute_helper(interpreter, [then_branch], Ok(None))
    False, Some(else_branch) ->
      execute_helper(interpreter, [else_branch], Ok(None))
    False, None -> #(Ok(None), interpreter)
  }
}

fn execute_while(
  interpreter: Interpreter(output),
  cond: Expr,
  body: Stmt,
) -> #(InterpretResult(output), Interpreter(output)) {
  let #(cond_rst, interpreter) = evaluate(interpreter, cond)

  // loop body
  use obj, interpreter <- with_ok(in: cond_rst, processer: interpreter)
  case types.is_truthy(obj) {
    True -> {
      let #(body_rst, interpreter) =
        execute_helper(interpreter, [body], Ok(None))
      use _output, interpreter <- with_ok(in: body_rst, processer: interpreter)
      execute_while(interpreter, cond, body)
    }
    False -> #(Ok(None), interpreter)
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

fn dive_into_block(interpreter: Interpreter(a)) -> Interpreter(a) {
  let block_env = environment.new_scope(interpreter.env)
  Interpreter(..interpreter, env: block_env)
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
  fun: fn(Object, Interpreter(output)) ->
    #(Result(a, RuntimeError), Interpreter(output)),
) -> #(Result(a, RuntimeError), Interpreter(output)) {
  let #(rst, interpreter) = evaluate(interpreter, expr)
  use obj, interpreter <- with_ok(in: rst, processer: interpreter)
  fun(obj, interpreter)
}
