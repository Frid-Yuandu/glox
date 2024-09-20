import gleeunit/should

import parse/lexer
import parse/token.{Token}

pub fn should_lex_single_character_tokens_test() {
  let source = ";(+{,.}-*)"
  let wanted = [
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
  ]

  lexer.new(source)
  |> lexer.collect()
  |> should.equal(wanted)
}

pub fn should_lex_one_or_two_characters_test() {
  let source = "!!= = == <> <=>="
  let wanted = [
    Ok(Token(token.Bang, 1)),
    Ok(Token(token.NotEqual, 1)),
    Ok(Token(token.Equal, 1)),
    Ok(Token(token.EqualEqual, 1)),
    Ok(Token(token.Less, 1)),
    Ok(Token(token.Greater, 1)),
    Ok(Token(token.LessEqual, 1)),
    Ok(Token(token.GreaterEqual, 1)),
  ]

  lexer.new(source)
  |> lexer.collect()
  |> should.equal(wanted)
}

pub fn should_lex_comments_test() {
  let source = "!// this is a comment"
  let wanted = [Ok(Token(token.Bang, 1))]

  lexer.new(source)
  |> lexer.collect()
  |> should.equal(wanted)
}

pub fn should_lex_token_between_comment_lines_test() {
  let source = "// this is the first comment\n==\n//this is the second comment"
  let wanted = [Ok(Token(token.EqualEqual, 2))]

  lexer.new(source)
  |> lexer.collect()
  |> should.equal(wanted)
}

pub fn should_lex_whitespace_test() {
  let source = ". \r,; \t "
  let wanted = [
    Ok(Token(token.Dot, 1)),
    Ok(Token(token.Comma, 1)),
    Ok(Token(token.Semicolon, 1)),
  ]

  lexer.new(source)
  |> lexer.collect()
  |> should.equal(wanted)
}

pub fn should_lex_new_line_test() -> Nil {
  let source = "+\n!\n-\n "
  let wanted = [
    Ok(Token(token.Plus, 1)),
    Ok(Token(token.Bang, 2)),
    Ok(Token(token.Minus, 3)),
  ]

  lexer.new(source)
  |> lexer.collect()
  |> should.equal(wanted)
}

pub fn should_lex_string_test() {
  let source = "\"this is a string\""
  let wanted = [Ok(Token(token.String("this is a string"), 1))]

  lexer.new(source)
  |> lexer.collect()
  |> should.equal(wanted)
}

pub fn should_lex_int_number_test() {
  let source = "1234"
  let wanted = [Ok(Token(token.Number(1234.0), 1))]

  lexer.new(source)
  |> lexer.collect()
  |> should.equal(wanted)
}

pub fn should_lex_float_number_test() {
  let source = "12.34"
  let wanted = [Ok(Token(token.Number(12.34), 1))]

  lexer.new(source)
  |> lexer.collect()
  |> should.equal(wanted)
}

pub fn should_lex_identifier_test() {
  let source = "test_1_name"
  let wanted = [Ok(Token(token.Identifier("test_1_name"), 1))]

  lexer.new(source)
  |> lexer.collect()
  |> should.equal(wanted)
}

pub fn should_not_lex_unsupport_token_test() {
  let source = "@"
  let wanted = [Error(lexer.LexicalError(lexer.UnexpectedCharacter("@"), 1))]

  lexer.new(source)
  |> lexer.collect()
  |> should.equal(wanted)
}
