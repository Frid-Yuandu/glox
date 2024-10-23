import gleam/int
import gleam/io
import gleam/iterator
import gleam/list
import gleam/option.{None, Some}
import gleam/string

import interpreter.{type Interpreter}
import parse
import parse/error.{
  type ParseError, ExpectRightBrace, ExpectRightParenthesis, ExpectSemicolon,
  ParseError,
}
import parse/expr.{type Expr}
import parse/stmt.{type Stmt}
import printer

import stdin

pub type REPL(a) {
  REPL(pending: List(String), lines: Int, interpreter: Interpreter(a))
}

pub type InputState(a) {
  Valid(a)
  Invalid
  Incomplete
}

pub fn start() {
  let init_repl = REPL(pending: [], lines: 1, interpreter: interpreter.new())
  print_prompt(init_repl)

  stdin.stdin()
  |> iterator.fold(init_repl, loop)
}

const input_arrow_prompt = "> "

fn print_prompt(repl: REPL(a)) -> Nil {
  case repl.pending {
    [] -> io.print(int.to_string(repl.lines) <> " " <> input_arrow_prompt)
    [_, ..] -> {
      let pending_dots =
        int.to_string(repl.lines)
        |> string.length
        |> string.repeat(".", _)
      io.print(pending_dots <> " " <> input_arrow_prompt)
    }
  }
}

fn loop(repl: REPL(a), line: String) -> REPL(a) {
  let repl = REPL(..repl, pending: list.append(repl.pending, [line]))
  let #(stmt_rst, expr_rst) = try_parse_stmt_and_expr(repl.pending)

  let new_repl = case validate(stmt_rst), validate(expr_rst) {
    Valid(Some(stmt)), _ -> interpret_stmt(stmt, with: repl)
    _, Valid(Some(expr)) -> evaluate_expr(expr, with: repl)
    Valid(None), _ | _, Valid(None) -> repl
    Incomplete, _ | _, Incomplete -> repl
    Invalid, Invalid -> handle_parse_error(error: stmt_rst, with: repl)
  }

  print_prompt(new_repl)
  new_repl
}

fn try_parse_stmt_and_expr(source_lines: List(String)) {
  let source = string.join(source_lines, "\n")
  let parser = parse.from_source(source)
  let #(stmt_rst, _) = parse.declaration(parser)
  let #(expr_rst, _) = parse.expression(parser)
  #(stmt_rst, expr_rst)
}

fn validate(parse_result: Result(a, ParseError)) -> InputState(a) {
  case parse_result {
    Ok(any) -> Valid(any)

    Error(ParseError(ExpectSemicolon, _))
    | Error(ParseError(ExpectRightParenthesis, _))
    | Error(ParseError(ExpectRightBrace, _)) -> Incomplete

    _ -> Invalid
  }
}

fn interpret_stmt(stmt: Stmt, with repl: REPL(a)) -> REPL(a) {
  let #(itpr_rest, interpreter) =
    interpreter.interpret(repl.interpreter, [stmt])
  case itpr_rest {
    Error(err) -> printer.print_runtime_error(err)
    Ok(_) -> Nil
  }

  update_repl_with(repl, interpreter)
}

fn evaluate_expr(expr: Expr, with repl: REPL(a)) -> REPL(a) {
  let #(eval_rst, interpreter) = interpreter.evaluate(repl.interpreter, expr)
  case eval_rst {
    Ok(obj) -> printer.print_evaluated_object(obj)
    Error(err) -> printer.print_runtime_error(err)
  }

  update_repl_with(repl, interpreter)
}

fn handle_parse_error(
  error should_error_input: Result(any, ParseError),
  with repl: REPL(a),
) -> REPL(a) {
  let assert Error(parse_err) = should_error_input
  printer.print_parse_error(parse_err)
  REPL(..repl, pending: [])
}

fn update_repl_with(repl: REPL(a), interpreter: Interpreter(a)) -> REPL(a) {
  REPL(lines: repl.lines + 1, interpreter:, pending: [])
}
