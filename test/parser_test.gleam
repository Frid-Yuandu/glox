import gleam/bool
import gleam/io
import gleam/iterator
import gleam/list
import gleam/option.{type Option, None, Some}
import gleeunit/should

import parse
import parse/error.{
  type ParseError, ExpectExpression, ExpectRightParenthesis, ExpectSemicolon,
  ExpectValue, ExpectVariableName, ExtraneousParenthesis, ExtraneousSemicolon,
  ParseError, UnexpectedToken,
}
import parse/expr.{Binary, Grouping, Literal, Number, Unary}
import parse/stmt
import parse/token.{Token}

// parse literal

pub fn should_parse_primary_number_test() {
  let wanted = wrap_expression(Literal(expr.Number(1.0)))

  [token.Number(1.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_string_test() {
  let wanted = wrap_expression(Literal(expr.String("hello")))

  [token.String("hello"), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_escape_string_test() {
  let wanted = wrap_expression(Literal(expr.String("\n")))

  [token.String("\n"), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_true_test() {
  let wanted = wrap_expression(Literal(expr.Bool(True)))

  [token.True, token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_false_test() {
  let wanted = wrap_expression(Literal(expr.Bool(False)))

  [token.False, token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_nil_test() {
  let wanted = wrap_expression(Literal(expr.NilLiteral))

  [token.NilLiteral, token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_not_parse_empty_parentheses_test() {
  let wanted = wrap_expression(Grouping(None))

  [token.LeftParen, token.RightParen, token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_non_empty_parentheses_test() {
  let wanted = wrap_expression(Grouping(Some(expr.Literal(expr.Number(1.0)))))

  [token.LeftParen, token.Number(1.0), token.RightParen, token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

// parse unary

pub fn should_parse_unary_negative_test() {
  let wanted =
    wrap_expression(Unary(
      wrap_token_type(token.Minus),
      Literal(expr.Number(2.4)),
    ))

  [token.Minus, token.Number(2.4), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_unary_bang_test() {
  let wanted =
    wrap_expression(Unary(wrap_token_type(token.Bang), Literal(expr.Bool(True))))

  [token.Bang, token.True, token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_successive_unary_bang_test() {
  let wanted =
    wrap_expression(Unary(
      wrap_token_type(token.Bang),
      Unary(
        wrap_token_type(token.Bang),
        Unary(wrap_token_type(token.Bang), Literal(expr.Number(0.0))),
      ),
    ))

  [token.Bang, token.Bang, token.Bang, token.Number(0.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_successive_unary_negative_test() {
  let wanted =
    wrap_expression(Unary(
      wrap_token_type(token.Minus),
      Unary(
        wrap_token_type(token.Minus),
        Unary(wrap_token_type(token.Minus), Literal(expr.Number(3.0))),
      ),
    ))

  [token.Minus, token.Minus, token.Minus, token.Number(3.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

// parse factor

pub fn should_parse_single_multiplication_test() {
  let wanted =
    wrap_expression(Binary(
      Literal(expr.Number(5.0)),
      wrap_token_type(token.Star),
      Literal(expr.Number(3.0)),
    ))

  [token.Number(5.0), token.Star, token.Number(3.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_successive_factor_multiplication_test() {
  let wanted =
    wrap_expression(Binary(
      Binary(
        Literal(expr.Number(2.0)),
        wrap_token_type(token.Star),
        Literal(expr.Number(4.0)),
      ),
      wrap_token_type(token.Star),
      Literal(expr.Number(8.0)),
    ))

  [
    token.Number(2.0),
    token.Star,
    token.Number(4.0),
    token.Star,
    token.Number(8.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_single_division_test() {
  let wanted =
    wrap_expression(Binary(
      Literal(expr.Number(10.0)),
      wrap_token_type(token.Slash),
      Literal(expr.Number(2.0)),
    ))

  [token.Number(10.0), token.Slash, token.Number(2.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_mixed_multiplication_and_division_test() {
  let wanted =
    wrap_expression(Binary(
      Binary(
        Literal(expr.Number(6.0)),
        wrap_token_type(token.Slash),
        Literal(expr.Number(3.0)),
      ),
      wrap_token_type(token.Star),
      Literal(expr.Number(2.0)),
    ))

  [
    token.Number(6.0),
    token.Slash,
    token.Number(3.0),
    token.Star,
    token.Number(2.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

// parse term

pub fn should_parse_term_add_test() {
  let wanted =
    wrap_expression(Binary(
      Literal(expr.Number(1.0)),
      wrap_token_type(token.Plus),
      Literal(expr.Number(2.0)),
    ))

  [token.Number(1.0), token.Plus, token.Number(2.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_mixed_add_and_subtrace_test() {
  let wanted =
    wrap_expression(Binary(
      Binary(
        Literal(expr.Number(1.0)),
        wrap_token_type(token.Plus),
        Literal(expr.Number(0.4)),
      ),
      wrap_token_type(token.Minus),
      Literal(expr.Number(3.7)),
    ))

  [
    token.Number(1.0),
    token.Plus,
    token.Number(0.4),
    token.Minus,
    token.Number(3.7),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_empty_source_test() {
  let wanted = []

  []
  |> parse_wanted
  |> should.equal(wanted)
}

// parse comparison

pub fn should_parse_less_than_test() {
  let wanted =
    wrap_expression(Binary(
      Literal(expr.Number(1.0)),
      wrap_token_type(token.Less),
      Literal(expr.Number(2.0)),
    ))

  [token.Number(1.0), token.Less, token.Number(2.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_greater_than_test() {
  let wanted =
    wrap_expression(Binary(
      Literal(expr.Number(3.0)),
      wrap_token_type(token.Greater),
      Literal(expr.Number(2.0)),
    ))

  [token.Number(3.0), token.Greater, token.Number(2.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_less_or_equal_test() {
  let wanted =
    wrap_expression(Binary(
      Literal(expr.Number(2.0)),
      wrap_token_type(token.LessEqual),
      Literal(expr.Number(2.0)),
    ))

  [token.Number(2.0), token.LessEqual, token.Number(2.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_greater_or_equal_test() {
  let wanted =
    wrap_expression(Binary(
      Literal(expr.Number(3.0)),
      wrap_token_type(token.GreaterEqual),
      Literal(expr.Number(3.0)),
    ))

  [token.Number(3.0), token.GreaterEqual, token.Number(3.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_successive_comparisons_test() {
  let wanted =
    wrap_expression(Binary(
      Binary(
        Literal(expr.Number(1.0)),
        wrap_token_type(token.LessEqual),
        Literal(expr.Number(2.0)),
      ),
      wrap_token_type(token.LessEqual),
      Literal(expr.Number(3.0)),
    ))

  [
    token.Number(1.0),
    token.LessEqual,
    token.Number(2.0),
    token.LessEqual,
    token.Number(3.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

// parse equality
//
pub fn should_parse_simple_equality_test() {
  let wanted =
    wrap_expression(Binary(
      Literal(expr.Number(1.0)),
      wrap_token_type(token.EqualEqual),
      Literal(expr.Number(1.0)),
    ))

  [token.Number(1.0), token.EqualEqual, token.Number(1.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_inequality_test() {
  let wanted =
    wrap_expression(Binary(
      Literal(expr.Number(2.5)),
      wrap_token_type(token.NotEqual),
      Literal(expr.Number(3.14)),
    ))

  [token.Number(2.5), token.NotEqual, token.Number(3.14), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

// parse variable declaration

pub fn should_parse_simple_var_declaration_test() {
  let wanted = [
    Ok(Some(stmt.Declaration(wrap_token_type(token.Identifier("x")), None))),
  ]

  [token.Var, token.Identifier("x"), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_var_declaration_with_initializer_test() {
  let wanted = [
    Ok(
      Some(stmt.Declaration(
        wrap_token_type(token.Identifier("y")),
        Some(Literal(expr.Number(42.0))),
      )),
    ),
  ]

  [
    token.Var,
    token.Identifier("y"),
    token.Equal,
    token.Number(42.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_var_declaration_with_expression_initializer_test() {
  let wanted = [
    Ok(
      Some(stmt.Declaration(
        wrap_token_type(token.Identifier("z")),
        Some(Binary(
          Literal(expr.Number(1.0)),
          wrap_token_type(token.Plus),
          Literal(expr.Number(2.0)),
        )),
      )),
    ),
  ]

  [
    token.Var,
    token.Identifier("z"),
    token.Equal,
    token.Number(1.0),
    token.Plus,
    token.Number(2.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_not_parse_var_declaration_missing_identifier_test() {
  let expected_error = Error(ParseError(ExpectVariableName, 1))

  let parse_result =
    [token.Var, token.Equal, token.Number(1.0), token.Semicolon]
    |> parse_wanted

  should.be_true(contains(parse_result, expected_error))
}

pub fn should_not_parse_var_declaration_missing_semicolon_test() {
  let expected_error = Error(ParseError(ExpectSemicolon, 1))

  let parse_result =
    [token.Var, token.Identifier("x"), token.Equal, token.Number(1.0)]
    |> parse_wanted

  should.be_true(contains(parse_result, expected_error))
}

pub fn should_not_parse_var_declaration_missing_initializer_expression_test() {
  let expected_error = Error(ParseError(ExpectValue, 1))

  let parse_result =
    [token.Var, token.Identifier("x"), token.Equal, token.Semicolon]
    |> parse_wanted

  should.be_true(contains(parse_result, expected_error))
}

// parse error

pub fn should_not_parse_unexpected_token_test() {
  let expected_error = wrap_error(ParseError(UnexpectedToken(token.Plus), 1))

  [token.Plus, token.Semicolon]
  |> parse_wanted
  |> should.equal(expected_error)
}

pub fn should_not_parse_missing_semicolon_test() {
  let expected_error = Error(ParseError(ExpectSemicolon, 1))
  let unexpected_stmt = Ok(Some(stmt.Expression(Literal(Number(1.0)))))
  // Missing semicolon after number

  let parse_result =
    [token.Number(1.0)]
    |> parse_wanted

  should.be_true(
    contains(parse_result, expected_error)
    && !contains(parse_result, unexpected_stmt),
  )
}

pub fn should_not_parse_unclosed_parenthesis_test() {
  let expected_error = Error(ParseError(ExpectRightParenthesis, 1))
  let unexpected_stmt =
    Ok(Some(stmt.Expression(Grouping(Some(Literal(Number(1.0)))))))
  // Unclosed parenthesis

  let parse_result =
    [token.LeftParen, token.Number(1.0), token.Semicolon]
    |> parse_wanted

  should.be_true(
    contains(parse_result, expected_error)
    && !contains(parse_result, unexpected_stmt),
  )
}

pub fn should_not_parse_invalid_binary_operator_test() {
  let unexpected_stmt =
    Ok(
      Some(
        stmt.Expression(Binary(
          Literal(Number(1.0)),
          Token(token.Comma, 1),
          Literal(Number(2.0)),
        )),
      ),
    )
  // Invalid operator

  let parse_result =
    [token.Number(1.0), token.Comma, token.Number(2.0), token.Semicolon]
    |> parse_wanted

  should.be_true(!contains(parse_result, unexpected_stmt))
}

pub fn should_not_parse_missing_expression_test() {
  let expected_error = wrap_error(ParseError(ExpectValue, 1))

  [token.String("foo"), token.Plus, token.Semicolon]
  // Missing expression after operator
  |> parse_wanted
  |> should.equal(expected_error)
}

pub fn should_not_parse_extraneous_semicolon_test() {
  let expected_error = Error(ParseError(ExtraneousSemicolon, 1))
  let expected_stmt = Ok(Some(stmt.Expression(Literal(Number(1.0)))))

  let parse_result =
    [token.Number(1.0), token.Semicolon, token.Semicolon]
    |> parse_wanted

  should.be_true(
    contains(parse_result, expected_error)
    && contains(parse_result, expected_stmt),
  )
}

// FIXME: should synchronize expression when encounter a parse error.
// pub fn should_not_parse_extraneous_parenthesis_test() {
//   let expected_error = Error(ParseError(ExtraneousParenthesis, 1))

//   let parse_result =
//     [token.LeftParen, token.RightParen, token.RightParen, token.Semicolon]
//     |> parse_wanted

//   should.be_true(contains(parse_result, expected_error))
// }

pub fn should_not_parse_print_statement_missing_expression() {
  let expected_error = Error(ParseError(ExpectExpression, 1))

  let parse_result =
    [token.Print, token.Semicolon]
    |> parse_wanted

  should.be_true(contains(parse_result, expected_error))
}

// helper

fn parse_wanted(
  wanted: List(token.TokenType),
) -> List(Result(Option(stmt.Stmt), error.ParseError)) {
  wanted
  |> list.map(fn(tok) { Ok(wrap_token_type(tok)) })
  |> iterator.from_list
  |> parse.new
  |> parse.parse
}

fn wrap_token_type(token_type: token.TokenType) -> token.Token {
  Token(token_type, 1)
}

fn wrap_expression(
  exp: expr.Expr,
) -> List(Result(Option(stmt.Stmt), error.ParseError)) {
  [Ok(Some(stmt.Expression(exp)))]
}

fn wrap_error(
  err: ParseError,
) -> List(Result(Option(stmt.Stmt), error.ParseError)) {
  [Error(err)]
}

fn contains(
  results: List(Result(Option(stmt.Stmt), ParseError)),
  target: Result(Option(stmt.Stmt), ParseError),
) -> Bool {
  list.contains(results, target)
}
