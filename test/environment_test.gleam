import gleam/dict
import gleam/option
import gleeunit/should
import interpreter/environment
import interpreter/types
import parse/token

pub fn assign_outer_variable__test() {
  let outer_env = environment.new()
  let outer_env =
    environment.define(
      outer_env,
      token.Token(token.Identifier("foo"), 1),
      types.Str("bar"),
    )
  let inner_env = environment.new_scope(outer_env)
  let assert Ok(inner_env) =
    environment.assign(
      inner_env,
      token.Token(token.Identifier("foo"), 1),
      types.Str("hello"),
    )
  let wanted =
    environment.Environment(
      values: dict.new(),
      enclosing: option.Some(environment.Environment(
        values: dict.new() |> dict.insert("foo", types.Str("hello")),
        enclosing: option.None,
      )),
    )
  should.equal(inner_env, wanted)
}
