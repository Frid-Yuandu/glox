import gleam/list
import gleam/option.{None, Some}
import gleeunit/should

import expr
import parse/token
import parser

// pub fn parse_primary_number_test() {
//   let wanted = Ok(Some(expr.Literal(expr.Number(1.0))))

//   [token.Number(1.0), token.EOF]
//   |> list.map(warp_token_type_as_token)
//   |> parser.from_tokens()
//   |> parser.parse()
//   |> should.equal(wanted)
// }

// pub fn parse_primary_string_test() {
//   let wanted = Ok(Some(expr.Literal(expr.String("hello"))))

//   [token.String("hello"), token.EOF]
//   |> list.map(warp_token_type_as_token)
//   |> parser.from_tokens()
//   |> parser.parse()
//   |> should.equal(wanted)
// }

// pub fn parse_primary_true_test() {
//   let wanted = Ok(Some(expr.Literal(expr.Bool(True))))

//   [token.True, token.EOF]
//   |> list.map(warp_token_type_as_token)
//   |> parser.from_tokens()
//   |> parser.parse()
//   |> should.equal(wanted)
// }

// pub fn parse_primary_false_test() {
//   let wanted = Ok(Some(expr.Literal(expr.Bool(False))))

//   [token.False, token.EOF]
//   |> list.map(warp_token_type_as_token)
//   |> parser.from_tokens()
//   |> parser.parse()
//   |> should.equal(wanted)
// }

// pub fn parse_primary_nil_test() {
//   let wanted = Ok(Some(expr.Literal(expr.NilLiteral(Nil))))

//   [token.NilLiteral, token.EOF]
//   |> list.map(warp_token_type_as_token)
//   |> parser.from_tokens()
//   |> parser.parse()
//   |> should.equal(wanted)
// }

// pub fn parse_term_add_test() {
//   let wanted =
//     Ok(
//       Some(expr.Binary(
//         expr.Literal(expr.Number(1.0)),
//         warp_token_type_as_token(token.Plus),
//         expr.Literal(expr.Number(2.0)),
//       )),
//     )

//   [token.Number(1.0), token.Plus, token.Number(2.0), token.EOF]
//   |> list.map(warp_token_type_as_token)
//   |> parser.from_tokens()
//   |> parser.parse()
//   |> should.equal(wanted)
// }

// pub fn parse_empty_source_test() {
//   let wanted = Ok(None)

//   [token.EOF]
//   |> list.map(warp_token_type_as_token)
//   |> parser.from_tokens()
//   |> parser.parse()
//   |> should.equal(wanted)
// }

pub fn parse_parentheses_source_test() {
  let wanted = Ok(Some(expr.Grouping(None)))

  [token.LeftParen, token.RightParen, token.EOF]
  |> list.map(warp_token_type_as_token)
  |> parser.from_tokens()
  |> parser.parse()
  |> should.equal(wanted)
}

fn warp_token_type_as_token(token_type: token.TokenType) -> token.Token {
  token.Token(token_type, 1)
}
