import gleam/int
import gleam/io

import parse/token.{type TokenType}

pub type LexicalError {
  LexicalError(error: LexicalErrorType, line: Int)
}

pub type LexicalErrorType {
  UnterminatedString
  UnexpectedCharacter(String)
  FailedParseNumber(String)
}

pub fn inspect_lex_error(err: LexicalError) -> String {
  let msg = case err.error {
    UnterminatedString -> "Unterminated string."
    UnexpectedCharacter(c) -> "Unexpected character: " <> c
    FailedParseNumber(n) -> "Failed to parse number, raw: " <> n
  }

  "[line " <> int.to_string(err.line) <> "] Error" <> ": " <> msg
}

pub fn print_lex_error(tok: LexicalError) -> Nil {
  tok
  |> inspect_lex_error
  |> io.println_error
}

pub type ParseError {
  ParseError(error: ParseErrorType, line: Int)
}

pub type ParseErrorType {
  LexError(LexicalError)

  ExpectRightValue
  ExpectLeftValue
  ExpectExpression
  ExpectStatement
  ExpectSemicolon
  ExpectLeftParentheses
  ExpectRightParentheses
  ExpectRightBrace
  ExpectVariableName

  ExtraneousParentheses

  InvalidAssignmentTarget

  UnexpectedToken(TokenType)
}

pub fn inspect_parse_error(err: ParseError) -> String {
  case err.error {
    LexError(lex_error) ->
      "Lexical error: "
      <> inspect_lex_error(lex_error)
      <> " on line "
      <> int.to_string(err.line)

    ExpectRightValue -> "Expect a value on line " <> int.to_string(err.line)
    ExpectLeftValue -> "Expect a left value on line " <> int.to_string(err.line)
    ExpectExpression ->
      "Expect an expression on line " <> int.to_string(err.line)
    ExpectStatement -> "Expect a statement on line " <> int.to_string(err.line)
    ExpectLeftParentheses ->
      "Expect an left parentheses on line " <> int.to_string(err.line)
    ExpectRightParentheses ->
      "Expect corresponding right parentheses \")\" after expression on line "
      <> int.to_string(err.line)
    ExpectRightBrace ->
      "Expect corresponding right brace \"}\" on line "
      <> int.to_string(err.line)
    ExpectSemicolon ->
      "Expect semicolon \";\" after expression on line "
      <> int.to_string(err.line)
    ExpectVariableName ->
      "Expect variable name on line " <> int.to_string(err.line)

    ExtraneousParentheses ->
      "Extraneous closing parenthesis \")\": " <> int.to_string(err.line)

    // ExtraneousSemicolon ->
    //   "Extraneous semicolon \";\" after expression on line "
    //   <> int.to_string(err.line)
    //   <> ", please remove it"
    UnexpectedToken(tok) ->
      "Unexpected token '"
      <> token.to_string(tok)
      <> "' on line "
      <> int.to_string(err.line)

    InvalidAssignmentTarget ->
      "Invalid assignment target on line" <> int.to_string(err.line)
  }
}
