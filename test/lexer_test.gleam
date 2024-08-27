import gleeunit/should
import lexer
import token

pub fn lex_single_character_tokens_test() {
  let lexer = lexer.from_string(";(+{,.}-*)")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: ";(+{,.}-*)",
    tokens: [
      token.Semicolon,
      token.LeftParen,
      token.Plus,
      token.LeftBrace,
      token.Comma,
      token.Dot,
      token.RightBrace,
      token.Minus,
      token.Star,
      token.RightParen,
      token.EOF,
    ],
    start: 10,
    current: 10,
    line: 0,
  ))
}

pub fn lex_one_or_two_characters_test() {
  let lexer = lexer.from_string("!!= = == <> <=>=")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "!!= = == <> <=>=",
    tokens: [
      token.Bang,
      token.NotEqual,
      token.Equal,
      token.EqualEqual,
      token.Less,
      token.Greater,
      token.LessEqual,
      token.GreaterEqual,
      token.EOF,
    ],
    start: 16,
    current: 16,
    line: 0,
  ))
}

pub fn lex_comments_test() {
  let lexer = lexer.from_string("!// this is a comment")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "!// this is a comment",
    tokens: [token.Bang, token.EOF],
    start: 21,
    current: 21,
    line: 0,
  ))
}

pub fn lex_token_between_comment_lines_test() {
  let lexer =
    lexer.from_string(
      "// this is the first comment\n==\n//this is the second comment",
    )

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "// this is the first comment\n==\n//this is the second comment",
    tokens: [token.EqualEqual, token.EOF],
    start: 60,
    current: 60,
    line: 2,
  ))
}

pub fn lex_whitespace_test() {
  let lexer = lexer.from_string(". \r,; \t ")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: ". \r,; \t ",
    tokens: [token.Dot, token.Comma, token.Semicolon, token.EOF],
    start: 8,
    current: 8,
    line: 0,
  ))
}

pub fn lex_new_line_test() -> Nil {
  let lexer = lexer.from_string("+\n!\n-\n ")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "+\n!\n-\n ",
    tokens: [token.Plus, token.Bang, token.Minus, token.EOF],
    start: 7,
    current: 7,
    line: 3,
  ))
}

pub fn lex_string_test() {
  let lexer = lexer.from_string("\"this is a string\"")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "\"this is a string\"",
    tokens: [token.String("this is a string"), token.EOF],
    start: 18,
    current: 18,
    line: 0,
  ))
}

pub fn lex_unsupport_token_test() {
  let lexer =
    lexer.Lexer(source: "@", tokens: [], start: 0, current: 0, line: 0)

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "@",
    tokens: [token.EOF],
    start: 1,
    current: 1,
    line: 0,
  ))
}