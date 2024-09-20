import gleam/io
import gleam/iterator
import gleam/option.{Some}
import printer

import parse/lexer
import parser

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
        Error(_) -> exit(65)
        Ok(_) -> exit(0)
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

fn run(source: String) {
  source
  |> lexer.new
  |> parser.new
  |> parser.parse
  |> fn(result) {
    case result {
      Ok(Some(e)) -> printer.inspect(e)
      _ -> ""
    }
    result
  }
}

@external(erlang, "exit_ffi", "do_exit")
fn exit(code: Int) -> Nil
