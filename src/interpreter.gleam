//// The interpreter module implements a tree-walk interpreter for the Lox language.
//// It evaluates expressions and executes statements according to Lox semantics,
//// maintaining state through an environment and handling side effects parametrically.
////
//// The interpreter supports:
//// - Expression evaluation (arithmetic, logic, comparison)
//// - Variable declaration and assignment
//// - Control flow (if/else, while loops)
//// - Block scoping
//// - Basic I/O through print statements
////
//// Side effects like printing are handled through a generic effect type,
//// allowing for different I/O implementations (e.g., stdout vs test captures).
////
//// Example:
////
//// ```gleam
//// let interpreter = interpreter.new()
//// let #(result, new_interpreter) = interpreter.execute(statements)
//// ```
////
//// The interpreter maintains immutability by returning new interpreter instances
//// after each operation that modifies state. Error handling is done through
//// Result types, with runtime errors carrying detailed information about the
//// failure location and cause.

import gleam/float
import gleam/list
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

/// Represents an interpreter instance with an environment and IO interface
/// effect: The type of side effects the interpreter can produce
pub type Interpreter(effect) {
  Interpreter(env: Environment, io: IOInterface(effect))
}

/// Creates a new interpreter instance with a default environment and IO interface
pub fn new() -> Interpreter(Nil) {
  Interpreter(env: environment.new(), io: io_controller.default_io())
}

/// Result of evaluating a single expression
pub type EvalResult =
  Result(Object, RuntimeError)

/// Represents the result of executing statements
/// effect: The type of side effects produced during execution
pub type Executed(effect) {
  Void
  Break
  Pure(Object)
  Effect(side_effect: List(effect), value: Option(Object))
}

/// Result of executing a list of statements
/// effect: The type of side effects produced during execution
pub type ExecuteResult(effect) =
  Result(Executed(effect), RuntimeError)

/// Executes a list of statements in the given interpreter context
/// Returns the execution result and updated interpreter state
pub fn execute(
  interpreter: Interpreter(effect),
  statements: List(Stmt),
) -> #(ExecuteResult(effect), Interpreter(effect)) {
  execute_helper(interpreter, statements, Void)
}

fn execute_helper(
  interpreter: Interpreter(effect),
  statements: List(Stmt),
  last_result: Executed(effect),
) -> #(ExecuteResult(effect), Interpreter(effect)) {
  case statements {
    [] -> #(Ok(last_result), interpreter)

    [Print(exp), ..rest] -> {
      let #(rst, interpreter) = evaluate(interpreter, exp)
      use obj <- with_ok(in: rst, processer: interpreter)

      let output = interpreter.io.write_stdout(types.inspect_object(obj))
      let prev_eft = case last_result {
        Effect(side_effect:, ..) -> side_effect
        _ -> []
      }

      execute_helper(
        interpreter,
        rest,
        Effect(side_effect: [output, ..prev_eft], value: None),
      )
    }
    [stmt.EmptyExpression, ..rest] -> {
      let result = case last_result {
        Effect(side_effect:, ..) -> Effect(side_effect:, value: None)
        _ -> Void
      }
      execute_helper(interpreter, rest, result)
    }
    [Expression(exp), ..rest] -> {
      let #(rst, interpreter) = evaluate(interpreter, exp)
      case rst {
        Ok(obj) -> {
          let result = case last_result {
            Effect(side_effect:, ..) -> Effect(side_effect:, value: Some(obj))
            _ -> Pure(obj)
          }
          execute_helper(interpreter, rest, result)
        }
        Error(err) -> #(Error(err), interpreter)
      }
    }

    // the value of the declaration should always be `Void`
    [Declaration(name, initializer), ..rest] -> {
      // assign nil to variables without initializer
      let #(rst, interpreter) =
        initializer |> option.unwrap(NilLiteral) |> evaluate(interpreter, _)
      use value <- with_ok(in: rst, processer: interpreter)
      let interpreter = define_variable(interpreter, name, value)
      execute_helper(interpreter, rest, Void)
    }

    [Block(statements), ..rest] -> {
      let #(rst, interpreter) = execute_block(interpreter, statements)
      case rst {
        Error(_) | Ok(Break) -> #(rst, interpreter)
        Ok(value) -> execute_helper(interpreter, rest, value)
      }
    }

    [If(cond, then_branch, maybe_else), ..rest] -> {
      let #(rst, interpreter) =
        execute_if(interpreter, cond, then_branch, maybe_else)
      case rst {
        Error(_) | Ok(Break) -> #(rst, interpreter)
        Ok(value) -> execute_helper(interpreter, rest, value)
      }
    }
    [While(cond, body), ..rest] -> {
      let #(rst, interpreter) = execute_while(interpreter, cond, body, Void)
      case rst {
        Error(_) -> #(rst, interpreter)
        Ok(value) -> execute_helper(interpreter, rest, value)
      }
    }
    [stmt.Break, ..] -> #(Ok(Break), interpreter)
    // TODO: implement me
    [stmt.Continue, ..] -> todo
  }
}

// TODO: jump out of block safely, handle none enclosing case and return error
fn execute_block(
  interpreter: Interpreter(effect),
  statements: List(Stmt),
) -> #(ExecuteResult(effect), Interpreter(effect)) {
  let #(rst, interpreter) =
    dive_into_block(interpreter)
    |> execute_helper(statements, Void)

  #(rst, jump_out_of_block(interpreter))
}

fn execute_if(
  interpreter: Interpreter(effect),
  cond: Expr,
  then_branch: Stmt,
  maybe_else: Option(Stmt),
) -> #(ExecuteResult(effect), Interpreter(effect)) {
  let #(cond_rst, interpreter) = evaluate(interpreter, cond)

  use obj <- with_ok(in: cond_rst, processer: interpreter)
  case types.is_truthy(obj), maybe_else {
    True, _ -> execute_helper(interpreter, [then_branch], Void)
    False, Some(else_branch) -> execute_helper(interpreter, [else_branch], Void)
    False, None -> #(Ok(Void), interpreter)
  }
}

// While loop has init void value. If any computation occurred in the loop body,
// the executed value will be "lifted" to pure or effect level.
fn execute_while(
  interpreter: Interpreter(effect),
  cond: Expr,
  body: Stmt,
  last_result: Executed(effect),
) -> #(ExecuteResult(effect), Interpreter(effect)) {
  let #(cond_rst, interpreter) = evaluate(interpreter, cond)

  // loop body
  use obj <- with_ok(in: cond_rst, processer: interpreter)
  case types.is_truthy(obj) {
    True -> {
      let #(rst, interpreter) = execute_helper(interpreter, [body], last_result)
      use execute_value <- with_ok(in: rst, processer: interpreter)
      case execute_value {
        Break -> #(Ok(last_result), interpreter)
        _ -> {
          case execute_value, last_result {
            Void, Effect(..) -> Effect(..last_result, value: None)
            Pure(value), Effect(..) -> Effect(..last_result, value: Some(value))
            Effect(..), Effect(..) ->
              Effect(
                ..execute_value,
                side_effect: list.flatten([
                  execute_value.side_effect,
                  last_result.side_effect,
                ]),
              )
            Effect(..), Void | Effect(..), Pure(..) -> execute_value
            Pure(_), Pure(_) | Pure(_), Void -> execute_value
            Void, Pure(_) | Void, Void -> last_result

            _, _ -> panic as "Unreachable"
          }
          |> execute_while(interpreter, cond, body, _)
        }
      }
    }
    False -> #(Ok(last_result), interpreter)
  }
}

// environment changes helper
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

fn jump_out_of_block(interpreter: Interpreter(a)) -> Interpreter(a) {
  let assert Some(env) = interpreter.env.enclosing
  Interpreter(..interpreter, env:)
}

/// Evaluates an expression in the given interpreter context
/// Returns the evaluation result and updated interpreter state
pub fn evaluate(
  interpreter: Interpreter(effect),
  exp: Expr,
) -> #(EvalResult, Interpreter(effect)) {
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
      use left_obj <- with_ok(left_rst, interpreter)
      case types.is_truthy(left_obj) {
        True -> #(left_rst, interpreter)
        False -> evaluate(interpreter, right)
      }
    }
    expr.LogicAnd(left, _tok, right) -> {
      let #(left_rst, interpreter) = evaluate(interpreter, left)
      use left_obj <- with_ok(left_rst, interpreter)
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

    expr.Call(_, _, _) -> todo
  }
}

fn evaluate_binary(
  interpreter: Interpreter(effect),
  left: Expr,
  op: Token,
  right: Expr,
) -> #(EvalResult, Interpreter(effect)) {
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
  interpreter: Interpreter(effect),
  expr: Expr,
  fun: fn(Object, Interpreter(effect)) ->
    #(Result(a, RuntimeError), Interpreter(effect)),
) -> #(Result(a, RuntimeError), Interpreter(effect)) {
  let #(rst, interpreter) = evaluate(interpreter, expr)
  use obj <- with_ok(in: rst, processer: interpreter)
  fun(obj, interpreter)
}
