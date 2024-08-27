import gleam/io

pub fn error(line line, with msg) -> Nil {
  report(line, "", msg)
}

fn report(line, where, msg) -> Nil {
  io.println("[line " <> line <> "] Error" <> where <> ": " <> msg)
}
