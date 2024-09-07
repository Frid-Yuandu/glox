import gleam/bool
import gleam/int

import gleam/list
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
  Lexer(source: String, tokens: List(LexicalResult), pos: Int, line: Int)
}

pub fn from_string(source: String) -> Lexer {
  Lexer(source:, tokens: [], pos: 0, line: 1)
}

fn shift(lexer: Lexer) -> Lexer {
  Lexer(..lexer, pos: lexer.pos + 1)
}

fn peak(lexer: Lexer) -> Option(String) {
  let Lexer(source:, pos:, ..) = lexer
  case is_at_end(lexer) {
    True -> None
    False -> Some(string.slice(from: source, at_index: pos, length: 1))
  }
}

fn is_at_end(lexer: Lexer) -> Bool {
  lexer.pos >= string.length(lexer.source)
}

pub fn lex_tokens(lexer: Lexer) -> Lexer {
  let Lexer(tokens:, line:, ..) = lexer
  case peak(lexer) {
    Some(char) -> {
      lexer
      |> lex_token(char)
      |> lex_tokens
    }
    None ->
      Lexer(..lexer, tokens: list.append(tokens, [Ok(Token(token.EOF, line))]))
  }
}

fn lex_token(lexer: Lexer, char: String) -> Lexer {
  use <- bool.lazy_guard(is_quotation_mark(char), fn() { lex_string(lexer) })
  use <- bool.lazy_guard(is_digit(char), fn() { lex_number(lexer) })
  use <- bool.lazy_guard(is_alpha(char), fn() { lex_identifier(lexer) })

  lex_characters(lexer)
}

fn lex_characters(lexer: Lexer) -> Lexer {
  let char0 = peak(lexer)

  case char0 {
    Some("(") -> consume_single(lexer, Ok(Token(token.LeftParen, lexer.line)))
    Some(")") -> consume_single(lexer, Ok(Token(token.RightParen, lexer.line)))
    Some("{") -> consume_single(lexer, Ok(Token(token.LeftBrace, lexer.line)))
    Some("}") -> consume_single(lexer, Ok(Token(token.RightBrace, lexer.line)))
    Some(",") -> consume_single(lexer, Ok(Token(token.Comma, lexer.line)))
    Some(".") -> consume_single(lexer, Ok(Token(token.Dot, lexer.line)))
    Some("-") -> consume_single(lexer, Ok(Token(token.Minus, lexer.line)))
    Some("+") -> consume_single(lexer, Ok(Token(token.Plus, lexer.line)))
    Some(";") -> consume_single(lexer, Ok(Token(token.Semicolon, lexer.line)))
    Some("*") -> consume_single(lexer, Ok(Token(token.Star, lexer.line)))

    Some(" ") | Some("\r") | Some("\t") -> shift(lexer)
    Some("\n") -> Lexer(..lexer, line: lexer.line + 1) |> shift

    Some("!") ->
      case peak(shift(lexer)) {
        Some("=") ->
          consume_double(lexer, Ok(Token(token.NotEqual, lexer.line)))
        _ -> consume_single(lexer, Ok(Token(token.Bang, lexer.line)))
      }
    Some("=") ->
      case peak(shift(lexer)) {
        Some("=") ->
          consume_double(lexer, Ok(Token(token.EqualEqual, lexer.line)))
        _ -> consume_single(lexer, Ok(Token(token.Equal, lexer.line)))
      }
    Some(">") ->
      case peak(shift(lexer)) {
        Some("=") ->
          consume_double(lexer, Ok(Token(token.GreaterEqual, lexer.line)))
        _ -> consume_single(lexer, Ok(Token(token.Greater, lexer.line)))
      }
    Some("<") ->
      case peak(shift(lexer)) {
        Some("=") ->
          consume_double(lexer, Ok(Token(token.LessEqual, lexer.line)))
        _ -> consume_single(lexer, Ok(Token(token.Less, lexer.line)))
      }
    Some("/") ->
      case peak(shift(lexer)) {
        Some("/") -> consume_comment(lexer)
        _ -> consume_single(lexer, Ok(Token(token.Slash, lexer.line)))
      }

    Some(c) ->
      consume_single(
        lexer,
        Error(LexicalError(UnexpectedCharacter(c), lexer.line)),
      )
    None -> lexer
  }
}

fn consume_comment(lexer: Lexer) -> Lexer {
  case peak(lexer) {
    // Do not consume the newline in order to update lexer's line.
    None | Some("\n") -> lexer
    Some(_) -> consume_comment(shift(lexer))
  }
}

fn lex_string(lexer: Lexer) -> Lexer {
  shift(lexer) |> consume_string("")
}

fn consume_string(lexer: Lexer, acc: String) -> Lexer {
  let Lexer(line:, tokens:, ..) = lexer

  case peak(lexer) {
    // The closing "
    Some("\"") -> {
      let tokens =
        list.append(tokens, [Ok(Token(token.String(acc), lexer.line))])
      Lexer(..lexer, tokens:) |> shift
    }
    Some("\n") ->
      shift(Lexer(..lexer, line: line + 1))
      |> consume_string(acc <> "\n")
    Some(c) ->
      shift(lexer)
      |> consume_string(acc <> c)
    None -> {
      let tokens =
        list.append(tokens, [Error(LexicalError(UnterminatedString, line))])
      Lexer(..lexer, tokens:) |> shift
    }
  }
}

fn lex_number(lexer) -> Lexer {
  let #(acc, lexer) = consume_number(lexer, "")
  let #(acc, lexer) = case is_need_consume_fraction(lexer) {
    True -> consume_number(shift(lexer), acc <> ".")
    False -> #(acc, lexer)
  }

  case token.parse_number(acc) {
    Ok(tok) ->
      Lexer(
        ..lexer,
        tokens: list.append(lexer.tokens, [Ok(Token(tok, lexer.line))]),
      )

    Error(_) -> {
      let tokens =
        list.append(lexer.tokens, [
          Error(LexicalError(FailedParseNumber(acc), lexer.line)),
        ])
      Lexer(..lexer, tokens:)
    }
  }
}

fn consume_number(lexer, acc) -> #(String, Lexer) {
  let char = peak(lexer) |> option.unwrap("")
  case is_digit(char) {
    True -> consume_number(shift(lexer), acc <> char)
    False -> #(acc, lexer)
  }
}

fn is_need_consume_fraction(lexer) -> Bool {
  let next_lexer = Lexer(..lexer, pos: lexer.pos + 1)
  let is_next_digit = peak(next_lexer) |> option.unwrap("") |> is_digit

  case peak(lexer), is_next_digit {
    Some("."), True -> True
    _, _ -> False
  }
}

fn lex_identifier(lexer) -> Lexer {
  let #(idt_str, lexer) = consume_identifier(lexer, "")
  let tokens = case to_keyword(idt_str) {
    Some(tok) -> list.append(lexer.tokens, [Ok(Token(tok, lexer.line))])
    None ->
      list.append(lexer.tokens, [
        Ok(Token(token.Identifier(idt_str), lexer.line)),
      ])
  }
  Lexer(..lexer, tokens:)
}

fn consume_identifier(lexer, acc) -> #(String, Lexer) {
  let char = peak(lexer) |> option.unwrap("")
  case is_alphanumeric(char) {
    True -> consume_identifier(shift(lexer), acc <> char)
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

fn consume_single(lexer: Lexer, result: LexicalResult) -> Lexer {
  Lexer(..lexer, tokens: list.append(lexer.tokens, [result]))
  |> shift
}

fn consume_double(lexer, result) -> Lexer {
  consume_single(lexer, result)
  |> shift
}
