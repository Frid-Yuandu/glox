import argv
import gleam/io
import gleam/iterator
import gleam/list
import scan
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
  |> iterator.map(fn(line) {
    io.print("> ")
    run(line)
  })
  |> iterator.run()
}

fn run(source: String) -> Nil {
  scan.scan_token(source)
  |> list.map(token.to_string)
  |> list.each(io.println)
}

fn error(line, message) -> Nil {
  report(line, "", message)
}

fn report(line, where, message) -> Nil {
  io.println("[line " <> line <> "] Error" <> where <> ": " <> message)
}

@external(erlang, "erlang", "halt")
fn exit(code: Int) -> Nil
