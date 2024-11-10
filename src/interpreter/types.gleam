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

/// is_truthy converts any object to boolean. As in Ruby's rule,
/// `false` and `nil` are falsey, and everything else are truthy.
pub fn is_truthy(obj: Object) -> Bool {
  case obj {
    BoolVal(v) -> v
    NilVal -> False
    _ -> True
  }
}

/// is_equal provide ability to compare any types of value.
pub fn is_equal(left: Object, right: Object) -> Bool {
  case left, right {
    NilVal, NilVal -> True
    Num(l), Num(r) -> l == r
    Str(l), Str(r) -> l == r
    BoolVal(l), BoolVal(r) -> l == r
    Class(l_name, l_fields), Class(r_name, r_fields) ->
      l_name == r_name && l_fields == r_fields
    _, _ -> False
  }
}
