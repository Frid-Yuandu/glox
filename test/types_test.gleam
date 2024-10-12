import gleeunit/should

import interpreter/types

pub fn should_inspect_object_string_escape_test() {
  let given_obj = types.Str("\r")

  types.inspect_object(given_obj)
  |> should.equal("\r")
}
