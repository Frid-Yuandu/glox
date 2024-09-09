import gleam/io
import gleam/iterator
import gleam/list
import gleam/option.{Some}
import gleam/result

import ast_printer
import parse/lexer.{type LexResult}
import parse/token
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
  let tokens = lexer.new(source) |> lexer.collect

  print_errors(tokens)
  print_tokens(tokens)

  let lex_result = case list.all(tokens, result.is_ok) {
    True -> Ok(tokens)
    False -> Error(parser.ParseError)
  }

  use tokens <- result.try(lex_result)

  let parse_res =
    tokens
    |> list.map(result.unwrap(_, token.Token(token.EOF, 1)))
    |> parser.from_tokens()
    |> parser.parse

  case parse_res {
    Ok(Some(expr)) -> ast_printer.inspect(expr) |> io.println
    _ -> Nil
  }

  parse_res
}

fn print_errors(tokens: List(LexResult)) -> Nil {
  tokens
  |> list.filter(result.is_error)
  |> list.each(fn(item) {
    case item {
      Error(err) -> lexer.inspect_error(err) |> io.println_error
      _ -> Nil
    }
  })
}

fn print_tokens(tokens: List(LexResult)) -> Nil {
  tokens
  |> list.filter(result.is_ok)
  |> list.each(fn(item) {
    case item {
      Ok(tok) -> token.to_string(tok.token_type) |> io.println
      _ -> Nil
    }
  })
}

@external(erlang, "exit_ffi", "do_exit")
fn exit(code: Int) -> Nil
