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
}

pub fn inspect_runtime_error(err: RuntimeError) -> String {
  let msg = case err {
    RuntimeError(op, OperandMustBeNumber) ->
      "the operands of " <> prettier_token(op) <> " must be Number type."
    RuntimeError(op, OperandMustBeString) ->
      "the operands of " <> prettier_token(op) <> " must be String type."
    RuntimeError(op, OperandMustBeNumberOrString) ->
      "the operands of "
      <> prettier_token(op)
      <> " must be Number type or String type."
    RuntimeError(op, DivideByZero) ->
      "divide a number by 0 is not supportted." <> prettier_token(op)
  }
  "Runtime Error: " <> msg
}

fn prettier_token(tok: Token) -> String {
  token.to_lexeme(tok.type_) <> " [line " <> int.to_string(tok.line) <> "]"
}
