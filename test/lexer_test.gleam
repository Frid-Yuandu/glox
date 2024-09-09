import gleam/list
import gleam/string
import gleeunit/should

import parse/lexer
import parse/token.{Token}

pub fn should_lex_single_character_tokens_test() {
  let source = ";(+{,.}-*)"
  let tokens = [
    Ok(Token(token.Semicolon, 1)),
    Ok(Token(token.LeftParen, 1)),
    Ok(Token(token.Plus, 1)),
    Ok(Token(token.LeftBrace, 1)),
    Ok(Token(token.Comma, 1)),
    Ok(Token(token.Dot, 1)),
    Ok(Token(token.RightBrace, 1)),
    Ok(Token(token.Minus, 1)),
    Ok(Token(token.Star, 1)),
    Ok(Token(token.RightParen, 1)),
    Ok(Token(token.EOF, 1)),
  ]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_one_or_two_characters_test() {
  let source = "!!= = == <> <=>="
  let tokens = [
    Ok(Token(token.Bang, 1)),
    Ok(Token(token.NotEqual, 1)),
    Ok(Token(token.Equal, 1)),
    Ok(Token(token.EqualEqual, 1)),
    Ok(Token(token.Less, 1)),
    Ok(Token(token.Greater, 1)),
    Ok(Token(token.LessEqual, 1)),
    Ok(Token(token.GreaterEqual, 1)),
    Ok(Token(token.EOF, 1)),
  ]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_comments_test() {
  let source = "!// this is a comment"
  let tokens = [Ok(Token(token.Bang, 1)), Ok(Token(token.EOF, 1))]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_token_between_comment_lines_test() {
  let source = "// this is the first comment\n==\n//this is the second comment"
  let tokens = [Ok(Token(token.EqualEqual, 2)), Ok(Token(token.EOF, 3))]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_whitespace_test() {
  let source = ". \r,; \t "
  let tokens = [
    Ok(Token(token.Dot, 1)),
    Ok(Token(token.Comma, 1)),
    Ok(Token(token.Semicolon, 1)),
    Ok(Token(token.EOF, 1)),
  ]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_new_line_test() -> Nil {
  let source = "+\n!\n-\n "
  let tokens = [
    Ok(Token(token.Plus, 1)),
    Ok(Token(token.Bang, 2)),
    Ok(Token(token.Minus, 3)),
    Ok(Token(token.EOF, 4)),
  ]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_string_test() {
  let source = "\"this is a string\""
  let tokens = [
    Ok(Token(token.String("this is a string"), 1)),
    Ok(Token(token.EOF, 1)),
  ]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_int_number_test() {
  let source = "1234"
  let tokens = [Ok(Token(token.Number(1234.0), 1)), Ok(Token(token.EOF, 1))]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_float_number_test() {
  let source = "12.34"
  let tokens = [Ok(Token(token.Number(12.34), 1)), Ok(Token(token.EOF, 1))]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_identifier_test() {
  let source = "test_1_name"
  let tokens = [
    Ok(Token(token.Identifier("test_1_name"), 1)),
    Ok(Token(token.EOF, 1)),
  ]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_not_lex_unsupport_token_test() {
  let source = "@"
  let tokens = [
    Error(lexer.LexicalError(lexer.UnexpectedCharacter("@"), 1)),
    Ok(Token(token.EOF, 1)),
  ]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

fn compare(lhs: lexer.Lexer, rhs: lexer.Lexer) -> Bool {
  lhs.source == rhs.source
  && lhs.tokens == rhs.tokens
  && lhs.pos == rhs.pos
  && lhs.line == rhs.line
}

fn produce_lexer(
  source: String,
  tokens: List(lexer.LexicalResult),
) -> lexer.Lexer {
  lexer.Lexer(
    source:,
    tokens:,
    pos: string.length(source),
    line: list.count(string.to_graphemes(source), fn(c) { c == "\n" }) + 1,
  )
}
