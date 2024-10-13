import gleam/dict.{type Dict}
import interpreter/runtime_error.{
  type RuntimeError, RuntimeError, UndefinedVariable,
}
import interpreter/types.{type Object}
import parse/token.{type Token}

pub type Environment {
  Environment(values: Dict(String, Object))
}

pub fn define(env: Environment, name, value) -> Environment {
  Environment(dict.insert(env.values, name, value))
}

pub fn get(env: Environment, tok: Token) -> Result(Object, RuntimeError) {
  case dict.get(env.values, token.to_lexeme(tok.type_)) {
    Ok(obj) -> Ok(obj)
    Error(Nil) -> Error(RuntimeError(tok, UndefinedVariable))
  }
}
