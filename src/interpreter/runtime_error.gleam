import gleam/int
import parse/token.{type Token}

pub type RuntimeError {
  RuntimeError(token: Token, error: RuntimeErrorType)
}

pub type RuntimeErrorType {
  OperandMustBeNumber
  OperandMustBeString
  OperandMustBeNumberOrString
  DivideByZero
  UndefinedVariable
}

pub fn inspect_runtime_error(err: RuntimeError) -> String {
  let msg = case err.error {
    OperandMustBeNumber ->
      "the operands of " <> prettier_token(err.token) <> " must be Number type."
    OperandMustBeString ->
      "the operands of " <> prettier_token(err.token) <> " must be String type."
    OperandMustBeNumberOrString ->
      "the operands of "
      <> prettier_token(err.token)
      <> " must be Number type or String type."
    DivideByZero ->
      "divide a number by 0 is not supportted." <> prettier_token(err.token)
    UndefinedVariable ->
      "undefined variable \"" <> token.to_lexeme(err.token.type_) <> "\" ."
  }
  "Runtime Error: " <> msg
}

fn prettier_token(tok: Token) -> String {
  token.to_lexeme(tok.type_) <> " [line " <> int.to_string(tok.line) <> "]"
}
