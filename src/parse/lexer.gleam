import gleam/bool
import gleam/int
import gleam/iterator.{type Iterator}
import gleam/option.{type Option, None, Some}
import gleam/string

import parse/predicate.{is_alpha, is_alphanumeric, is_digit, is_quotation_mark}
import parse/token.{type Token, type TokenType, Token}

pub type LexicalResult =
  Result(Token, LexicalError)

pub type LexicalError {
  LexicalError(error: LexicalErrorType, line: Int)
}

pub type LexicalErrorType {
  UnterminatedString
  UnexpectedCharacter(String)
  FailedParseNumber(String)
}

pub fn inspect_error(err: LexicalError) -> String {
  let msg = case err.error {
    UnterminatedString -> "Unterminated string."
    UnexpectedCharacter(c) -> "Unexpected character: " <> c
    FailedParseNumber(n) -> "Failed to parse number, raw: " <> n
  }

  "[line " <> int.to_string(err.line) <> "] Error" <> ": " <> msg
}

pub type Lexer {
  Lexer(
    source: String,
    pending: Option(LexicalResult),
    char0: Option(String),
    char1: Option(String),
    line: Int,
  )
}

pub fn new(source: String) -> Iterator(LexicalResult) {
  Lexer(source:, pending: None, char0: None, char1: None, line: 1)
  |> advance
  |> advance
  |> to_iter
}

fn advance(lexer: Lexer) -> Lexer {
  case string.pop_grapheme(lexer.source) {
    Ok(#(char, rest)) ->
      Lexer(..lexer, source: rest, char0: lexer.char1, char1: Some(char))

    Error(Nil) -> Lexer(..lexer, char0: lexer.char1, char1: None)
  }
}

pub fn collect(lexer: Iterator(LexicalResult)) -> List(LexicalResult) {
  iterator.to_list(lexer)
}

fn to_iter(lexer: Lexer) -> Iterator(LexicalResult) {
  use lexer <- iterator.unfold(lexer)

  case next(lexer) {
    #(Ok(Token(token.EOF, _)), _) -> iterator.Done
    #(res, lexer) -> iterator.Next(res, lexer)
  }
}

fn next(lexer: Lexer) -> #(LexicalResult, Lexer) {
  let lexer = lex_token(lexer)
  case lexer.pending {
    None -> #(Ok(Token(token.EOF, lexer.line)), lexer)
    Some(res) -> #(res, Lexer(..lexer, pending: None))
  }
}

fn lex_token(lexer: Lexer) -> Lexer {
  let lexer = consume_grapheme(lexer)
  case lexer {
    Lexer(char0: Some(_), pending: None, ..) -> lex_token(lexer)
    _ -> lexer
  }
}

fn consume_grapheme(lexer: Lexer) -> Lexer {
  let char = lexer.char0 |> option.unwrap("")

  use <- bool.lazy_guard(is_quotation_mark(char), fn() { lex_string(lexer) })
  use <- bool.lazy_guard(is_digit(char), fn() { lex_number(lexer) })
  use <- bool.lazy_guard(is_alpha(char), fn() { lex_identifier(lexer) })

  lex_characters(lexer)
}

fn lex_characters(lexer: Lexer) -> Lexer {
  let Lexer(line:, ..) = lexer

  case lexer.char0 {
    Some("(") -> consume_token(lexer, Token(token.LeftParen, line))
    Some(")") -> consume_token(lexer, Token(token.RightParen, line))
    Some("{") -> consume_token(lexer, Token(token.LeftBrace, line))
    Some("}") -> consume_token(lexer, Token(token.RightBrace, line))
    Some(",") -> consume_token(lexer, Token(token.Comma, line))
    Some(".") -> consume_token(lexer, Token(token.Dot, line))
    Some("-") -> consume_token(lexer, Token(token.Minus, line))
    Some("+") -> consume_token(lexer, Token(token.Plus, line))
    Some(";") -> consume_token(lexer, Token(token.Semicolon, line))
    Some("*") -> consume_token(lexer, Token(token.Star, line))

    Some(" ") | Some("\r") | Some("\t") -> advance(lexer)
    Some("\n") -> Lexer(..lexer, line: line + 1) |> advance

    // Single or double characters operators, should look ahead.
    Some("!") ->
      case lexer.char1 {
        Some("=") -> consume_double_char(lexer, Token(token.NotEqual, line))
        _ -> consume_token(lexer, Token(token.Bang, line))
      }
    Some("=") ->
      case lexer.char1 {
        Some("=") -> consume_double_char(lexer, Token(token.EqualEqual, line))
        _ -> consume_token(lexer, Token(token.Equal, line))
      }
    Some(">") ->
      case lexer.char1 {
        Some("=") -> consume_double_char(lexer, Token(token.GreaterEqual, line))
        _ -> consume_token(lexer, Token(token.Greater, line))
      }
    Some("<") ->
      case lexer.char1 {
        Some("=") -> consume_double_char(lexer, Token(token.LessEqual, line))
        _ -> consume_token(lexer, Token(token.Less, line))
      }
    Some("/") ->
      case lexer.char1 {
        Some("/") -> consume_comment(lexer)
        _ -> consume_token(lexer, Token(token.Slash, line))
      }

    Some(c) -> consume_error(lexer, LexicalError(UnexpectedCharacter(c), line))
    None -> lexer
  }
}

fn consume_comment(lexer: Lexer) -> Lexer {
  case lexer.char0 {
    // Do not consume the newline in order to update lexer's line.
    Some("\n") | None -> lexer
    Some(_) -> consume_comment(advance(lexer))
  }
}

fn lex_string(lexer: Lexer) -> Lexer {
  advance(lexer) |> consume_string("")
}

fn consume_string(lexer: Lexer, acc: String) -> Lexer {
  case lexer.char0 {
    // The closing "
    Some("\"") -> consume_token(lexer, Token(token.String(acc), lexer.line))
    Some("\n") ->
      advance(Lexer(..lexer, line: lexer.line + 1))
      |> consume_string(acc <> "\n")
    Some(c) -> consume_string(advance(lexer), acc <> c)
    None -> consume_error(lexer, LexicalError(UnterminatedString, lexer.line))
  }
}

fn lex_number(lexer) -> Lexer {
  let #(acc, lexer) = consume_number(lexer, "")
  let #(acc, lexer) = case is_fraction(lexer) {
    True -> consume_number(advance(lexer), acc <> ".")
    False -> #(acc, lexer)
  }

  case token.parse_number(acc) {
    Ok(tok) -> add_token(lexer, Ok(Token(tok, lexer.line)))
    Error(_) ->
      add_token(lexer, Error(LexicalError(FailedParseNumber(acc), lexer.line)))
  }
}

fn consume_number(lexer: Lexer, acc: String) -> #(String, Lexer) {
  let char = lexer.char0 |> option.unwrap("")
  case is_digit(char) {
    True -> consume_number(advance(lexer), acc <> char)
    False -> #(acc, lexer)
  }
}

fn is_fraction(lexer: Lexer) -> Bool {
  let is_next_digit = lexer.char1 |> option.unwrap("") |> is_digit

  case lexer.char0, is_next_digit {
    Some("."), True -> True
    _, _ -> False
  }
}

fn lex_identifier(lexer: Lexer) -> Lexer {
  let #(idt_str, lexer) = consume_identifier(lexer, "")
  case to_keyword(idt_str) {
    Some(tok) -> add_token(lexer, Ok(Token(tok, lexer.line)))
    None -> add_token(lexer, Ok(Token(token.Identifier(idt_str), lexer.line)))
  }
}

fn consume_identifier(lexer: Lexer, acc: String) -> #(String, Lexer) {
  let char = lexer.char0 |> option.unwrap("")
  case is_alphanumeric(char) {
    True -> consume_identifier(advance(lexer), acc <> char)
    False -> #(acc, lexer)
  }
}

fn to_keyword(char) -> Option(TokenType) {
  case char {
    "and" -> Some(token.And)
    "class" -> Some(token.Class)
    "else" -> Some(token.Else)
    "false" -> Some(token.False)
    "fun" -> Some(token.Fun)
    "for" -> Some(token.For)
    "if" -> Some(token.If)
    "nil" -> Some(token.NilLiteral)
    "or" -> Some(token.Or)
    "print" -> Some(token.Print)
    "return" -> Some(token.Return)
    "super" -> Some(token.Super)
    "this" -> Some(token.This)
    "true" -> Some(token.True)
    "var" -> Some(token.Var)
    "while" -> Some(token.While)
    _ -> None
  }
}

fn consume_token(lexer: Lexer, tok: Token) -> Lexer {
  add_token(lexer, Ok(tok))
  |> advance
}

fn consume_double_char(lexer: Lexer, tok: Token) -> Lexer {
  consume_token(lexer, tok)
  |> advance
}

fn consume_error(lexer: Lexer, error: LexicalError) -> Lexer {
  add_token(lexer, Error(error))
  |> advance
}

fn add_token(lexer: Lexer, result: LexicalResult) -> Lexer {
  Lexer(..lexer, pending: Some(result))
}
