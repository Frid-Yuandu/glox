import gleam/list
import gleam/option
import gleam/string
import gleeunit/should
import lexer
import token

pub fn should_lex_single_character_tokens_test() {
  let source = ";(+{,.}-*)"
  let tokens = [
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
  ]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_one_or_two_characters_test() {
  let source = "!!= = == <> <=>="
  let tokens = [
    Ok(token.Bang),
    Ok(token.NotEqual),
    Ok(token.Equal),
    Ok(token.EqualEqual),
    Ok(token.Less),
    Ok(token.Greater),
    Ok(token.LessEqual),
    Ok(token.GreaterEqual),
    Ok(token.EOF),
  ]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_comments_test() {
  let source = "!// this is a comment"
  let tokens = [Ok(token.Bang), Ok(token.EOF)]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_token_between_comment_lines_test() {
  let source = "// this is the first comment\n==\n//this is the second comment"
  let tokens = [Ok(token.EqualEqual), Ok(token.EOF)]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_whitespace_test() {
  let source = ". \r,; \t "
  let tokens = [
    Ok(token.Dot),
    Ok(token.Comma),
    Ok(token.Semicolon),
    Ok(token.EOF),
  ]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_new_line_test() -> Nil {
  let source = "+\n!\n-\n "
  let tokens = [Ok(token.Plus), Ok(token.Bang), Ok(token.Minus), Ok(token.EOF)]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_string_test() {
  let source = "\"this is a string\""
  let tokens = [Ok(token.String("this is a string")), Ok(token.EOF)]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_int_number_test() {
  let source = "1234"
  let tokens = [Ok(token.Number(1234.0)), Ok(token.EOF)]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_float_number_test() {
  let source = "12.34"
  let tokens = [Ok(token.Number(12.34)), Ok(token.EOF)]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_lex_identifier_test() {
  let source = "test_1_name"
  let tokens = [Ok(token.Identifier("test_1_name")), Ok(token.EOF)]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

pub fn should_not_lex_unsupport_token_test() {
  let source = "@"
  let tokens = [
    Error(lexer.LexicalError(lexer.UnexpectedCharacter("@"), 0)),
    Ok(token.EOF),
  ]

  let got = lexer.from_string(source) |> lexer.lex_tokens()
  let want = produce_lexer(source, tokens)
  should.be_true(compare(got, want))
}

fn compare(lhs: lexer.Lexer, rhs: lexer.Lexer) -> Bool {
  let lexer.Lexer(
    source: l_source,
    tokens: l_tokens,
    pos: l_pos,
    line: l_line,
    ..,
  ) = lhs
  let lexer.Lexer(
    source: r_source,
    tokens: r_tokens,
    pos: r_pos,
    line: r_line,
    ..,
  ) = rhs

  l_source == r_source
  && l_tokens == r_tokens
  && l_pos == r_pos
  && l_line == r_line
}

fn produce_lexer(
  source: String,
  tokens: List(lexer.LexicalResult),
) -> lexer.Lexer {
  lexer.Lexer(
    source:,
    tokens:,
    pos: string.length(source),
    line: list.count(string.to_graphemes(source), fn(c) { c == "\n" }),
  )
}
