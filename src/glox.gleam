import gleam/io
import gleam/iterator
import gleam/option.{None, Some}
import gleam/result

import interpreter
import parse
import parse/error.{type ParseError}
import parse/lexer
import printer
import runtime_error.{type RuntimeError}
import types

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
        Empty | Object(_) -> exit(0)
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
  Empty
  Object(types.Object)
  FailedParse(ParseError)
  FailedRun(RuntimeError)
}

fn run(source: String) -> RunResult {
  let parse_rst =
    source
    |> lexer.new
    |> parse.new
    |> parse.parse
    |> result.map_error(FailedParse)

  let interpret_rst = {
    use maybe_exp <- result.try(parse_rst)
    case maybe_exp {
      None -> Error(Empty)
      Some(exp) -> Ok(interpreter.interpret(exp))
    }
  }
  let run_rst = case interpret_rst {
    Error(rst) -> rst
    Ok(Ok(obj)) -> Object(obj)
    Ok(Error(err)) -> FailedRun(err)
  }
  print_run_result(run_rst)

  run_rst
}

pub fn print_run_result(rst: RunResult) -> Nil {
  case rst {
    Empty -> Nil
    FailedParse(err) -> printer.print_parse_error(err)
    FailedRun(err) -> printer.print_runtime_error(err)
    Object(obj) -> printer.print_evaluated_object(obj)
  }
}

@external(erlang, "exit_ffi", "do_exit")
fn exit(code: Int) -> Nil
