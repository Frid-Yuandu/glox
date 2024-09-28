import gleam/io
import gleam/iterator
import gleam/list
import gleam/option
import gleam/result

import interpreter
import parse
import parse/error.{type ParseError}
import parse/lexer
import printer
import runtime_error.{type RuntimeError}

import argv
import simplifile
import stdin.{stdin}

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
      case run(content) {
        Complete -> exit(0)
        FailedParse(_) -> exit(65)
        FailedRun(_) -> exit(70)
      }
    }
    Error(_) -> exit(64)
  }
}

fn run_prompt() -> Nil {
  stdin()
  |> iterator.each(fn(line) {
    let _ = run(line)
    io.print("> ")
  })
}

pub type RunResult {
  Complete
  FailedParse(List(ParseError))
  FailedRun(RuntimeError)
}

fn run(source: String) -> RunResult {
  let #(maybe_statements, parse_errors) =
    source
    |> lexer.new
    |> parse.new
    |> parse.parse
    |> result.partition

  let parse_results = case parse_errors {
    [] -> Ok(maybe_statements)
    [_, ..] -> Error(FailedParse(parse_errors))
  }

  let interpret_rst = {
    use maybe_statements <- result.try(parse_results)

    maybe_statements
    |> list.filter_map(fn(maybe) { option.to_result(maybe, Nil) })
    |> interpreter.interpret
    |> result.map_error(FailedRun)
  }

  let run_rst = case interpret_rst {
    Ok(Nil) -> Complete
    Error(FailedRun(_) as err) -> err
    Error(FailedParse(_) as err) -> err
    _ -> panic
  }
  print_run_result(run_rst)

  run_rst
}

pub fn print_run_result(rst: RunResult) -> Nil {
  case rst {
    Complete -> Nil
    FailedParse(errors) -> list.each(errors, printer.print_parse_error)
    FailedRun(err) -> printer.print_runtime_error(err)
  }
}

@external(erlang, "exit_ffi", "do_exit")
fn exit(code: Int) -> Nil
