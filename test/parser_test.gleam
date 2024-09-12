import gleam/io
import gleam/iterator
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should

import expr.{Binary, Grouping, Literal, Unary}
import parse/token.{Token}
import parser

// pub fn parse_primary_number_test() {
//   let wanted = Ok(Some(Literal(expr.Number(1.0))))

//   [token.Number(1.0), token.EOF]
//   |> parse_wanted
//   |> should.equal(wanted)
// }

// pub fn parse_primary_string_test() {
//   let wanted = Ok(Some(Literal(expr.String("hello"))))

//   [token.String("hello"), token.EOF]
//   |> parse_wanted
//   |> should.equal(wanted)
// }

// pub fn parse_primary_true_test() {
//   let wanted = Ok(Some(Literal(expr.Bool(True))))

//   [token.True, token.EOF]
//   |> parse_wanted
//   |> should.equal(wanted)
// }

// pub fn parse_primary_false_test() {
//   let wanted = Ok(Some(Literal(expr.Bool(False))))

//   [token.False, token.EOF]
//   |> parse_wanted
//   |> should.equal(wanted)
// }

// pub fn parse_primary_nil_test() {
//   let wanted = Ok(Some(Literal(expr.NilLiteral(Nil))))

//   [token.NilLiteral, token.EOF]
//   |> parse_wanted
//   |> should.equal(wanted)
// }

// pub fn parse_term_add_test() {
//   let wanted =
//     Ok(
//       Some(Binary(
//         Literal(expr.Number(1.0)),
//         warp_token_type(token.Plus),
//         Literal(expr.Number(2.0)),
//       )),
//     )

//   [token.Number(1.0), token.Plus, token.Number(2.0), token.EOF]
//   |> parse_wanted
//   |> should.equal(wanted)
// }

// pub fn parse_empty_source_test() {
//   let wanted = Ok(None)

//   [token.EOF]
//   |> parse_wanted
//   |> should.equal(wanted)
// }

// pub fn parse_parentheses_source_test() {
//   let wanted = Ok(Some(Grouping(None)))

//   [token.LeftParen, token.RightParen, token.EOF]
//   |> parse_wanted
//   |> should.equal(wanted)
// }

// FIXME: 
// pub fn parse_unary_neg_number_test() {
//   let wanted =
//     Ok(Some(Unary(warp_token_type(token.Minus), Literal(expr.Number(2.4)))))

//   [token.Minus, token.Number(2.4), token.EOF]
//   |> parse_wanted
//   |> should.equal(wanted)
// }

fn parse_wanted(wanted: List(token.TokenType)) -> parser.ParseResult {
  todo
  // wanted
  // |> list.map(warp_token_type)
  // |> parser.new()
  // |> parser.parse()
}

fn warp_token_type(token_type: token.TokenType) -> token.Token {
  Token(token_type, 1)
}
