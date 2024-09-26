import gleam/iterator
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should

import parse
import parse/expr.{Binary, Grouping, Literal, Unary}
import parse/token.{Token}

// parse literal

pub fn should_parse_primary_number_test() {
  let wanted = Ok(Some(Literal(expr.Number(1.0))))

  [token.Number(1.0)]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_string_test() {
  let wanted = Ok(Some(Literal(expr.String("hello"))))

  [token.String("hello")]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_escape_string_test() {
  let wanted = Ok(Some(Literal(expr.String("\n"))))

  [token.String("\n")]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_true_test() {
  let wanted = Ok(Some(Literal(expr.Bool(True))))

  [token.True]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_false_test() {
  let wanted = Ok(Some(Literal(expr.Bool(False))))

  [token.False]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_nil_test() {
  let wanted = Ok(Some(Literal(expr.NilLiteral)))

  [token.NilLiteral]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_not_parse_empty_parentheses_test() {
  let wanted = Ok(Some(Grouping(None)))

  [token.LeftParen, token.RightParen]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_non_empty_parentheses_test() {
  let wanted = Ok(Some(Grouping(Some(expr.Literal(expr.Number(1.0))))))

  [token.LeftParen, token.Number(1.0), token.RightParen]
  |> parse_wanted
  |> should.equal(wanted)
}

// parse unary

pub fn should_parse_unary_negative_test() {
  let wanted = Ok(Some(Unary(wrap(token.Minus), Literal(expr.Number(2.4)))))

  [token.Minus, token.Number(2.4)]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_unary_bang_test() {
  let wanted = Ok(Some(Unary(wrap(token.Bang), Literal(expr.Bool(True)))))

  [token.Bang, token.True]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_successive_unary_bang_test() {
  let wanted =
    Ok(
      Some(Unary(
        wrap(token.Bang),
        Unary(
          wrap(token.Bang),
          Unary(wrap(token.Bang), Literal(expr.Number(0.0))),
        ),
      )),
    )

  [token.Bang, token.Bang, token.Bang, token.Number(0.0)]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_successive_unary_negative_test() {
  let wanted =
    Ok(
      Some(Unary(
        wrap(token.Minus),
        Unary(
          wrap(token.Minus),
          Unary(wrap(token.Minus), Literal(expr.Number(3.0))),
        ),
      )),
    )

  [token.Minus, token.Minus, token.Minus, token.Number(3.0)]
  |> parse_wanted
  |> should.equal(wanted)
}

// parse factor

pub fn should_parse_single_multiplication_test() {
  let wanted =
    Ok(
      Some(Binary(
        Literal(expr.Number(5.0)),
        wrap(token.Star),
        Literal(expr.Number(3.0)),
      )),
    )

  [token.Number(5.0), token.Star, token.Number(3.0)]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_successive_factor_multiplication_test() {
  let wanted =
    Ok(
      Some(Binary(
        Binary(
          Literal(expr.Number(2.0)),
          wrap(token.Star),
          Literal(expr.Number(4.0)),
        ),
        wrap(token.Star),
        Literal(expr.Number(8.0)),
      )),
    )

  [
    token.Number(2.0),
    token.Star,
    token.Number(4.0),
    token.Star,
    token.Number(8.0),
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_single_division_test() {
  let wanted =
    Ok(
      Some(Binary(
        Literal(expr.Number(10.0)),
        wrap(token.Slash),
        Literal(expr.Number(2.0)),
      )),
    )

  [token.Number(10.0), token.Slash, token.Number(2.0)]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_mixed_multiplication_and_division_test() {
  let wanted =
    Ok(
      Some(Binary(
        Binary(
          Literal(expr.Number(6.0)),
          wrap(token.Slash),
          Literal(expr.Number(3.0)),
        ),
        wrap(token.Star),
        Literal(expr.Number(2.0)),
      )),
    )

  [
    token.Number(6.0),
    token.Slash,
    token.Number(3.0),
    token.Star,
    token.Number(2.0),
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

// parse term

pub fn should_parse_term_add_test() {
  let wanted =
    Ok(
      Some(Binary(
        Literal(expr.Number(1.0)),
        wrap(token.Plus),
        Literal(expr.Number(2.0)),
      )),
    )

  [token.Number(1.0), token.Plus, token.Number(2.0)]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_mixed_add_and_subtrace_test() {
  let wanted =
    Ok(
      Some(Binary(
        Binary(
          Literal(expr.Number(1.0)),
          wrap(token.Plus),
          Literal(expr.Number(0.4)),
        ),
        wrap(token.Minus),
        Literal(expr.Number(3.7)),
      )),
    )

  [
    token.Number(1.0),
    token.Plus,
    token.Number(0.4),
    token.Minus,
    token.Number(3.7),
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_empty_source_test() {
  let wanted = Ok(None)

  []
  |> parse_wanted
  |> should.equal(wanted)
}

// parse comparison

pub fn should_parse_less_than_test() {
  let wanted =
    Ok(
      Some(Binary(
        Literal(expr.Number(1.0)),
        wrap(token.Less),
        Literal(expr.Number(2.0)),
      )),
    )

  [token.Number(1.0), token.Less, token.Number(2.0)]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_greater_than_test() {
  let wanted =
    Ok(
      Some(Binary(
        Literal(expr.Number(3.0)),
        wrap(token.Greater),
        Literal(expr.Number(2.0)),
      )),
    )

  [token.Number(3.0), token.Greater, token.Number(2.0)]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_less_or_equal_test() {
  let wanted =
    Ok(
      Some(Binary(
        Literal(expr.Number(2.0)),
        wrap(token.LessEqual),
        Literal(expr.Number(2.0)),
      )),
    )

  [token.Number(2.0), token.LessEqual, token.Number(2.0)]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_greater_or_equal_test() {
  let wanted =
    Ok(
      Some(Binary(
        Literal(expr.Number(3.0)),
        wrap(token.GreaterEqual),
        Literal(expr.Number(3.0)),
      )),
    )

  [token.Number(3.0), token.GreaterEqual, token.Number(3.0)]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_successive_comparisons_test() {
  let wanted =
    Ok(
      Some(Binary(
        Binary(
          Literal(expr.Number(1.0)),
          wrap(token.LessEqual),
          Literal(expr.Number(2.0)),
        ),
        wrap(token.LessEqual),
        Literal(expr.Number(3.0)),
      )),
    )

  [
    token.Number(1.0),
    token.LessEqual,
    token.Number(2.0),
    token.LessEqual,
    token.Number(3.0),
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

// parse equality
//
pub fn should_parse_simple_equality_test() {
  let wanted =
    Ok(
      Some(Binary(
        Literal(expr.Number(1.0)),
        wrap(token.EqualEqual),
        Literal(expr.Number(1.0)),
      )),
    )

  [token.Number(1.0), token.EqualEqual, token.Number(1.0)]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_inequality_test() {
  let wanted =
    Ok(
      Some(Binary(
        Literal(expr.Number(2.5)),
        wrap(token.NotEqual),
        Literal(expr.Number(3.14)),
      )),
    )

  [token.Number(2.5), token.NotEqual, token.Number(3.14)]
  |> parse_wanted
  |> should.equal(wanted)
}

// helper

fn parse_wanted(wanted: List(token.TokenType)) -> parse.ParseResult {
  wanted
  |> list.map(fn(tok) {
    let tok = wrap(tok)
    Ok(tok)
  })
  |> iterator.from_list
  |> parse.new
  |> parse.parse
}

fn wrap(token_type: token.TokenType) -> token.Token {
  Token(token_type, 1)
}
