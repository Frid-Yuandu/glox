import gleam/bool
import gleam/function
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import repl

import interpreter.{type Interpreter}
import interpreter/runtime_error.{type RuntimeError}
import parse
import parse/error.{type ParseError}
import printer

import argv
import simplifile

pub fn main() {
  case argv.load().arguments {
    [] -> run_prompt()
    [arg] -> run_file(arg)
    _ -> {
      io.println("Usage: glox [script]")
      exit(64)
    }
  }
}

fn run_file(path: String) -> Nil {
  case simplifile.read(path) {
    Ok(content) -> {
      let interpreter = interpreter.new()
      let rst = run(interpreter, content)
      case rst {
        Complete -> exit(0)
        FailedParse(_) -> exit(65)
        FailedRun(_) -> exit(70)
      }
    }
    Error(_) -> exit(64)
  }
}

fn run_prompt() -> Nil {
  repl.start()

  io.println("lox REPL exit.")
}

pub type RunResult {
  Complete
  FailedParse(List(ParseError))
  FailedRun(RuntimeError)
}

pub fn run(interpreter: Interpreter(Nil), source: String) -> RunResult {
  let #(maybe_statements, parse_errors) =
    source
    |> parse.from_source
    |> parse.parse
    |> result.partition

  use <- bool.guard(when: parse_errors != [], return: FailedParse(parse_errors))
  let #(interpret_rst, _) =
    maybe_statements
    |> list.filter_map(fn(maybe) { option.to_result(maybe, Nil) })
    |> list.reverse
    |> interpreter.interpret(interpreter, _)

  case interpret_rst {
    Ok(_) -> Complete
    Error(err) -> FailedRun(err)
  }
  |> function.tap(print_run_result)
}

fn print_run_result(rst: RunResult) -> Nil {
  case rst {
    Complete -> Nil
    FailedParse(errors) -> list.each(errors, printer.print_parse_error)
    FailedRun(err) -> printer.print_runtime_error(err)
  }
}

@external(erlang, "exit_ffi", "do_exit")
@external(javascript, "exit_ffi", "do_exit")
fn exit(code: Int) -> Nil
