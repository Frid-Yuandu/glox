import gleam/dict.{type Dict}
import interpreter/runtime_error.{
  type RuntimeError, RuntimeError, UndefinedVariable,
}
import interpreter/types.{type Object}
import parse/token.{type Token}

pub type Environment {
  Environment(values: Dict(String, Object))
}

pub fn define(env: Environment, name: Token, value: Object) -> Environment {
  Environment(dict.insert(env.values, token.to_lexeme(name.type_), value))
}

pub fn get(env: Environment, tok: Token) -> Result(Object, RuntimeError) {
  case dict.get(env.values, token.to_lexeme(tok.type_)) {
    Ok(obj) -> Ok(obj)
    Error(Nil) -> Error(RuntimeError(tok, UndefinedVariable))
  }
}

pub fn assign(
  env: Environment,
  name: Token,
  value: Object,
) -> Result(Environment, RuntimeError) {
  case dict.has_key(env.values, token.to_lexeme(name.type_)) {
    True -> Ok(define(env, name, value))
    False -> Error(RuntimeError(name, UndefinedVariable))
  }
}
