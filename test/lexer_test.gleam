import gleeunit/should
import lexer
import token

pub fn should_lex_single_character_tokens_test() {
  let lexer = lexer.from_string(";(+{,.}-*)")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: ";(+{,.}-*)",
    tokens: [
      Ok(token.Semicolon),
      Ok(token.LeftParen),
      Ok(token.Plus),
      Ok(token.LeftBrace),
      Ok(token.Comma),
      Ok(token.Dot),
      Ok(token.RightBrace),
      Ok(token.Minus),
      Ok(token.Star),
      Ok(token.RightParen),
      Ok(token.EOF),
    ],
    pos: 10,
    line: 0,
  ))
}

pub fn should_lex_one_or_two_characters_test() {
  let lexer = lexer.from_string("!!= = == <> <=>=")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "!!= = == <> <=>=",
    tokens: [
      Ok(token.Bang),
      Ok(token.NotEqual),
      Ok(token.Equal),
      Ok(token.EqualEqual),
      Ok(token.Less),
      Ok(token.Greater),
      Ok(token.LessEqual),
      Ok(token.GreaterEqual),
      Ok(token.EOF),
    ],
    pos: 16,
    line: 0,
  ))
}

pub fn should_lex_comments_test() {
  let lexer = lexer.from_string("!// this is a comment")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "!// this is a comment",
    tokens: [Ok(token.Bang), Ok(token.EOF)],
    pos: 21,
    line: 0,
  ))
}

pub fn should_lex_token_between_comment_lines_test() {
  let lexer =
    lexer.from_string(
      "// this is the first comment\n==\n//this is the second comment",
    )

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "// this is the first comment\n==\n//this is the second comment",
    tokens: [Ok(token.EqualEqual), Ok(token.EOF)],
    pos: 60,
    line: 2,
  ))
}

pub fn should_lex_whitespace_test() {
  let lexer = lexer.from_string(". \r,; \t ")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: ". \r,; \t ",
    tokens: [Ok(token.Dot), Ok(token.Comma), Ok(token.Semicolon), Ok(token.EOF)],
    pos: 8,
    line: 0,
  ))
}

pub fn should_lex_new_line_test() -> Nil {
  let lexer = lexer.from_string("+\n!\n-\n ")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "+\n!\n-\n ",
    tokens: [Ok(token.Plus), Ok(token.Bang), Ok(token.Minus), Ok(token.EOF)],
    pos: 7,
    line: 3,
  ))
}

pub fn should_lex_string_test() {
  let lexer = lexer.from_string("\"this is a string\"")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "\"this is a string\"",
    tokens: [Ok(token.String("this is a string")), Ok(token.EOF)],
    pos: 18,
    line: 0,
  ))
}

pub fn should_lex_int_number_test() {
  let lexer = lexer.from_string("1234")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "1234",
    tokens: [Ok(token.Number(1234.0)), Ok(token.EOF)],
    pos: 4,
    line: 0,
  ))
}

pub fn should_lex_float_number_test() {
  let lexer = lexer.from_string("12.34")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "12.34",
    tokens: [Ok(token.Number(12.34)), Ok(token.EOF)],
    pos: 5,
    line: 0,
  ))
}

pub fn should_lex_identifier_test() {
  let lexer = lexer.from_string("test_1_name")

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "test_1_name",
    tokens: [Ok(token.Identifier("test_1_name")), Ok(token.EOF)],
    pos: 11,
    line: 0,
  ))
}

pub fn should_not_lex_unsupport_token_test() {
  let lexer = lexer.Lexer(source: "@", tokens: [], pos: 0, line: 0)

  lexer.lex_tokens(lexer)
  |> should.equal(lexer.Lexer(
    source: "@",
    tokens: [
      Error(lexer.LexicalError(lexer.UnexpectedCharacter("@"), 0)),
      Ok(token.EOF),
    ],
    pos: 1,
    line: 0,
  ))
}
