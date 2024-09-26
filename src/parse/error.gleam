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
  ExpectValue
  ExpectExpression
  UnexpectedToken(TokenType)
  LexError(LexicalError)
  UnclosingParenthesis
  ExtraneousParenthesis
}

pub fn inspect_parse_error(err: ParseError) -> String {
  case err {
    ParseError(ExpectValue, line) ->
      "Expected a value on line " <> int.to_string(line)
    ParseError(ExpectExpression, line) ->
      "Expected an expression on line " <> int.to_string(line)
    ParseError(UnexpectedToken(tok), line) ->
      "Unexpected token '"
      <> token.to_string(tok)
      <> "' on line "
      <> int.to_string(line)
    ParseError(LexError(lex_error), line) ->
      "Lexical error: "
      <> inspect_lex_error(lex_error)
      <> " on line "
      <> int.to_string(line)
    ParseError(UnclosingParenthesis, line) ->
      "Unclosed parenthesis on line " <> int.to_string(line)
    ParseError(ExtraneousParenthesis, line) ->
      "Extraneous closing parenthesis \")\": " <> int.to_string(line)
  }
}
