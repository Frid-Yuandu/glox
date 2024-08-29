import argv
import gleam/io
import gleam/iterator
import gleam/list
import lexer
import simplifile
import stdin.{stdin}
import token

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
    Ok(content) -> run(content)
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

fn run(source: String) -> Nil {
  lexer.from_string(source)
  |> lexer.lex_tokens()
  |> lexer.get_tokens()
  |> list.map(token.to_string)
  |> list.each(io.println)
}

@external(erlang, "erlang", "halt")
fn exit(code: Int) -> Nil
