import gleam/bool
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
      let #(rst, _) = run(interpreter, content)
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

pub fn run(
  interpreter: Interpreter(Nil),
  source: String,
) -> #(RunResult, Interpreter(Nil)) {
  let #(maybe_statements, parse_errors) =
    source
    |> parse.from_source
    |> parse.parse
    |> result.partition

  let parse_results = case parse_errors {
    [] -> Ok(maybe_statements)
    [_, ..] -> Error(FailedParse(parse_errors))
  }

  let #(interpret_rst, interpreter) = {
    use <- bool.lazy_guard(result.is_error(parse_results), fn() {
      let assert Error(err) = parse_results
      #(Error(err), interpreter)
    })
    let assert Ok(maybe_statements) = parse_results

    let #(interpret_rst, interpreter) =
      maybe_statements
      |> list.filter_map(fn(maybe) { option.to_result(maybe, Nil) })
      |> list.reverse
      |> interpreter.interpret(interpreter, _)

    #(result.map_error(interpret_rst, FailedRun), interpreter)
  }

  let run_rst = case interpret_rst {
    Ok(_) -> Complete
    Error(FailedRun(_) as err) -> err
    Error(FailedParse(_) as err) -> err
    _ -> panic
  }
  print_run_result(run_rst)

  #(run_rst, interpreter)
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
