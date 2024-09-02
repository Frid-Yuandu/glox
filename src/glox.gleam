import gleam/io
import gleam/iterator
import gleam/list
import gleam/result

import lexer
import token

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
      let lex_result = run(content)
      case list.any(lex_result.tokens, result.is_error) {
        True -> exit(65)
        False -> exit(0)
      }
    }
    Error(_) -> exit(64)
  }
}

fn run_prompt() -> Nil {
  stdin()
  |> iterator.each(fn(line) {
    run(line)
    io.print("> ")
  })
}

fn run(source: String) -> lexer.Lexer {
  let lex_result =
    lexer.from_string(source)
    |> lexer.lex_tokens()

  lex_result.tokens
  |> print_errors

  lex_result.tokens
  |> print_tokens

  lex_result
}

fn print_errors(tokens: List(lexer.LexicalResult)) -> Nil {
  tokens
  |> list.filter(result.is_error)
  |> list.each(fn(item) {
    case item {
      Error(err) -> lexer.inspect_error(err) |> io.println_error
      _ -> Nil
    }
  })
}

fn print_tokens(tokens: List(lexer.LexicalResult)) -> Nil {
  tokens
  |> list.filter(result.is_ok)
  |> list.each(fn(item) {
    case item {
      Ok(tok) -> token.to_string(tok) |> io.println
      _ -> Nil
    }
  })
}

@external(erlang, "exit_ffi", "do_exit")
fn exit(code: Int) -> Nil
