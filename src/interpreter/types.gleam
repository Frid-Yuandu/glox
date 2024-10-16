import gleam/bool
import gleam/dict.{type Dict}
import gleam/float
import gleam/list
import gleam/string

/// Object is the representation of any lox value at runtime.
pub type Object {
  NilVal
  Num(Float)
  Str(String)
  BoolVal(Bool)
  Class(name: String, fields: Dict(String, Object))
  // Fun(args: List(Object))
}

pub fn inspect_object(obj: Object) -> String {
  case obj {
    NilVal -> "nil"
    BoolVal(b) -> bool.to_string(b)
    Num(n) -> float.to_string(n)
    Str(s) -> s
    Class(name:, fields:) -> {
      let field_series =
        dict.to_list(fields)
        |> list.map(fn(field) { field.0 <> ": " <> inspect_object(field.1) })
        |> string.join(", ")
      name <> "{" <> field_series <> "}"
    }
  }
}
