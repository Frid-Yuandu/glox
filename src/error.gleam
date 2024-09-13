import gleam/io

/// Deprecated.
pub fn error(line line, with msg) -> Nil {
  report(line, "", msg)
}

/// Deprecated.
fn report(line, where, msg) -> Nil {
  io.println_error("[line " <> line <> "] Error" <> where <> ": " <> msg)
}
