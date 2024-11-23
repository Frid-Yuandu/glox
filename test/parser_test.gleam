import birdie
import gleam/iterator
import gleam/list
import gleam/option.{type Option, None, Some}
import gleeunit/should
import pprint
import simplifile

import parse
import parse/error.{
  type ParseError, ExpectExpression, ExpectRightParentheses, ExpectRightValue,
  ExpectSemicolon, ExpectVariableName, ExtraneousParentheses, ParseError,
  UnexpectedToken,
}
import parse/expr.{
  Binary, Boolean, Grouping, NegativeBool, NegativeNumber, NilLiteral, Number,
}
import parse/stmt
import parse/token.{Token}

// parse literal

pub fn should_parse_primary_number_test() {
  let wanted = wrap_expression(Number(1.0))

  [token.Number(1.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_string_test() {
  let wanted = wrap_expression(expr.String("hello"))

  [token.String("hello"), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_escape_string_test() {
  let wanted = wrap_expression(expr.String("\n"))

  [token.String("\n"), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_true_test() {
  let wanted = wrap_expression(Boolean(True))

  [token.True, token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_false_test() {
  let wanted = wrap_expression(Boolean(False))

  [token.False, token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_primary_nil_test() {
  let wanted = wrap_expression(NilLiteral)

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
  let wanted = wrap_expression(Grouping(Some(Number(1.0))))

  [token.LeftParen, token.Number(1.0), token.RightParen, token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

// parse unary

pub fn should_parse_unary_negative_test() {
  let wanted =
    wrap_expression(NegativeNumber(
      token: wrap_token_type(token.Minus),
      value: Number(2.4),
    ))

  [token.Minus, token.Number(2.4), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_unary_bang_test() {
  let wanted =
    wrap_expression(NegativeBool(
      token: wrap_token_type(token.Bang),
      value: Boolean(True),
    ))

  [token.Bang, token.True, token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_successive_unary_bang_test() {
  let wanted =
    wrap_expression(NegativeBool(
      token: wrap_token_type(token.Bang),
      value: NegativeBool(
        token: wrap_token_type(token.Bang),
        value: NegativeBool(
          token: wrap_token_type(token.Bang),
          value: Number(0.0),
        ),
      ),
    ))

  [token.Bang, token.Bang, token.Bang, token.Number(0.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_successive_unary_negative_test() {
  let wanted =
    wrap_expression(NegativeNumber(
      token: wrap_token_type(token.Minus),
      value: NegativeNumber(
        token: wrap_token_type(token.Minus),
        value: NegativeNumber(
          token: wrap_token_type(token.Minus),
          value: Number(3.0),
        ),
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
      Number(5.0),
      wrap_token_type(token.Star),
      Number(3.0),
    ))

  [token.Number(5.0), token.Star, token.Number(3.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_successive_factor_multiplication_test() {
  let wanted =
    wrap_expression(Binary(
      Binary(Number(2.0), wrap_token_type(token.Star), Number(4.0)),
      wrap_token_type(token.Star),
      Number(8.0),
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
      Number(10.0),
      wrap_token_type(token.Slash),
      Number(2.0),
    ))

  [token.Number(10.0), token.Slash, token.Number(2.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_mixed_multiplication_and_division_test() {
  let wanted =
    wrap_expression(Binary(
      Binary(Number(6.0), wrap_token_type(token.Slash), Number(3.0)),
      wrap_token_type(token.Star),
      Number(2.0),
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
      Number(1.0),
      wrap_token_type(token.Plus),
      Number(2.0),
    ))

  [token.Number(1.0), token.Plus, token.Number(2.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_mixed_add_and_subtrace_test() {
  let wanted =
    wrap_expression(Binary(
      Binary(Number(1.0), wrap_token_type(token.Plus), Number(0.4)),
      wrap_token_type(token.Minus),
      Number(3.7),
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
      Number(1.0),
      wrap_token_type(token.Less),
      Number(2.0),
    ))

  [token.Number(1.0), token.Less, token.Number(2.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_greater_than_test() {
  let wanted =
    wrap_expression(Binary(
      Number(3.0),
      wrap_token_type(token.Greater),
      Number(2.0),
    ))

  [token.Number(3.0), token.Greater, token.Number(2.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_less_or_equal_test() {
  let wanted =
    wrap_expression(Binary(
      Number(2.0),
      wrap_token_type(token.LessEqual),
      Number(2.0),
    ))

  [token.Number(2.0), token.LessEqual, token.Number(2.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_greater_or_equal_test() {
  let wanted =
    wrap_expression(Binary(
      Number(3.0),
      wrap_token_type(token.GreaterEqual),
      Number(3.0),
    ))

  [token.Number(3.0), token.GreaterEqual, token.Number(3.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_successive_comparisons_test() {
  let wanted =
    wrap_expression(Binary(
      Binary(Number(1.0), wrap_token_type(token.LessEqual), Number(2.0)),
      wrap_token_type(token.LessEqual),
      Number(3.0),
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
      Number(1.0),
      wrap_token_type(token.EqualEqual),
      Number(1.0),
    ))

  [token.Number(1.0), token.EqualEqual, token.Number(1.0), token.Semicolon]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_inequality_test() {
  let wanted =
    wrap_expression(Binary(
      Number(2.5),
      wrap_token_type(token.NotEqual),
      Number(3.14),
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
        Some(Number(42.0)),
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
        Some(Binary(Number(1.0), wrap_token_type(token.Plus), Number(2.0))),
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
  let expected_error = Error(ParseError(ExpectRightValue, 1))

  let parse_result =
    [token.Var, token.Identifier("x"), token.Equal, token.Semicolon]
    |> parse_wanted

  should.be_true(contains(parse_result, expected_error))
}

// parse error

pub fn should_not_parse_unexpected_token_test() {
  [token.Plus, token.Semicolon]
  |> parse_wanted
  |> pprint.format
  |> birdie.snap(title: "should not parse unexpected token")
}

pub fn should_not_parse_missing_semicolon_test() {
  let expected_error = Error(ParseError(ExpectSemicolon, 1))
  let unexpected_stmt = Ok(Some(stmt.Expression(Number(1.0))))
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
  let expected_error = Error(ParseError(ExpectRightParentheses, 1))
  let unexpected_stmt = Ok(Some(stmt.Expression(Grouping(Some(Number(1.0))))))
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
        stmt.Expression(Binary(Number(1.0), Token(token.Comma, 1), Number(2.0))),
      ),
    )
  // Invalid operator

  let parse_result =
    [token.Number(1.0), token.Comma, token.Number(2.0), token.Semicolon]
    |> parse_wanted

  should.be_true(!contains(parse_result, unexpected_stmt))
}

pub fn should_not_parse_missing_expression_test() {
  let expected_error = wrap_error(ParseError(ExpectRightValue, 1))

  [token.String("foo"), token.Plus, token.Semicolon]
  // Missing expression after operator
  |> parse_wanted
  |> should.equal(expected_error)
}

pub fn should_not_parse_extraneous_parenthesis_test() {
  let expected_error = Error(ParseError(ExtraneousParentheses, 1))

  let parse_result =
    [token.RightParen, token.Semicolon]
    |> parse_wanted

  should.be_true(contains(parse_result, expected_error))
}

pub fn should_not_parse_print_statement_missing_expression() {
  let expected_error = Error(ParseError(ExpectExpression, 1))

  let parse_result =
    [token.Print, token.Semicolon]
    |> parse_wanted

  should.be_true(contains(parse_result, expected_error))
}

// parse block

pub fn should_parse_empty_block_test() {
  let wanted = [Ok(Some(stmt.Block([])))]

  [token.LeftBrace, token.RightBrace]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_block_with_single_statement_test() {
  let wanted = [Ok(Some(stmt.Block([stmt.Expression(Number(1.0))])))]

  [token.LeftBrace, token.Number(1.0), token.Semicolon, token.RightBrace]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_block_with_multiple_statements_test() {
  let wanted = [
    Ok(
      Some(
        stmt.Block([
          stmt.Expression(Number(1.0)),
          stmt.Print(Number(2.0)),
          stmt.Declaration(
            wrap_token_type(token.Identifier("x")),
            Some(Number(3.0)),
          ),
        ]),
      ),
    ),
  ]

  [
    token.LeftBrace,
    token.Number(1.0),
    token.Semicolon,
    token.Print,
    token.Number(2.0),
    token.Semicolon,
    token.Var,
    token.Identifier("x"),
    token.Equal,
    token.Number(3.0),
    token.Semicolon,
    token.RightBrace,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_nested_blocks_test() {
  let wanted = [
    Ok(
      Some(
        stmt.Block([
          stmt.Expression(Number(1.0)),
          stmt.Block([stmt.Expression(Number(2.0))]),
        ]),
      ),
    ),
  ]

  [
    token.LeftBrace,
    token.Number(1.0),
    token.Semicolon,
    token.LeftBrace,
    token.Number(2.0),
    token.Semicolon,
    token.RightBrace,
    token.RightBrace,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_complex_nested_blocks__test() {
  let assert Ok(content) =
    simplifile.read("./test/test_source/nested_block.lox")

  content
  |> parse.from_source
  |> parse.parse
  |> pprint.format
  |> birdie.snap(
    title: "parse complex nested blocks with multiple statements in book",
  )
}

pub fn should_not_parse_unclosed_block_test() {
  let expected_error = Error(ParseError(error.ExpectRightBrace, 1))

  let parse_result =
    [token.LeftBrace, token.Number(1.0), token.Semicolon]
    |> parse_wanted

  should.be_true(contains(parse_result, expected_error))
}

// Additional variable declaration tests

pub fn should_parse_multiple_var_declarations_test() {
  let wanted = [
    Ok(Some(stmt.Declaration(wrap_token_type(token.Identifier("x")), None))),
    Ok(
      Some(stmt.Declaration(
        wrap_token_type(token.Identifier("y")),
        Some(Number(2.0)),
      )),
    ),
  ]

  [
    token.Var,
    token.Identifier("x"),
    token.Semicolon,
    token.Var,
    token.Identifier("y"),
    token.Equal,
    token.Number(2.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_var_declaration_with_complex_initializer_test() {
  let wanted = [
    Ok(
      Some(stmt.Declaration(
        wrap_token_type(token.Identifier("z")),
        Some(Binary(
          Number(1.0),
          wrap_token_type(token.Plus),
          Binary(Number(2.0), wrap_token_type(token.Star), Number(3.0)),
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
    token.Star,
    token.Number(3.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_not_parse_var_declaration_with_invalid_initializer_test() {
  let expected_error = Error(ParseError(ExpectRightValue, 1))

  let parse_result =
    [token.Var, token.Identifier("x"), token.Equal, token.Semicolon]
    |> parse_wanted

  should.be_true(contains(parse_result, expected_error))
}

// parse if statement

pub fn should_parse_if_statement_without_else_test() {
  let wanted = [
    Ok(Some(stmt.If(expr.Number(1.0), stmt.Print(expr.Number(2.0)), None))),
  ]

  [
    token.If,
    token.LeftParen,
    token.Number(1.0),
    token.RightParen,
    token.Print,
    token.Number(2.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_if_statement_with_else_test() {
  let wanted = [
    Ok(
      Some(stmt.If(
        expr.Number(1.0),
        stmt.Print(expr.Number(2.0)),
        Some(stmt.Print(expr.Number(3.0))),
      )),
    ),
  ]

  [
    token.If,
    token.LeftParen,
    token.Number(1.0),
    token.RightParen,
    token.Print,
    token.Number(2.0),
    token.Semicolon,
    token.Else,
    token.Print,
    token.Number(3.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_not_parse_if_statement_missing_left_paren_test() {
  [token.If, token.RightParen, token.Number(1.0), token.Semicolon]
  |> parse_wanted
  |> pprint.format
  |> birdie.snap(title: "error in parse if statement missing left parentheses")
}

pub fn should_not_parse_if_statement_missing_condition_test() {
  [
    token.If,
    token.LeftParen,
    token.RightParen,
    token.Number(1.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> pprint.format
  |> birdie.snap(title: "error in parse if statement missing condition")
}

pub fn should_not_parse_if_statement_missing_right_paren_test() {
  [
    token.If,
    token.LeftParen,
    token.Number(1.0),
    token.Number(2.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> pprint.format
  |> birdie.snap(title: "error in parse if statement missing right parentheses")
}

pub fn should_not_parse_if_statement_missing_body_test() {
  [token.If, token.LeftParen, token.Number(1.0), token.RightParen]
  |> parse_wanted
  |> pprint.format
  |> birdie.snap(title: "error in parse if statement missing body")
}

pub fn should_not_parse_if_statement_with_invalid_condition_test() {
  [
    token.If,
    token.LeftParen,
    token.Plus,
    token.RightParen,
    token.Number(1.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> pprint.format
  |> birdie.snap(title: "error in parse invalid condition in if statement")
}

// parse while statement

pub fn should_parse_while_statement_test() {
  let wanted = [
    Ok(Some(stmt.While(expr.Number(1.0), stmt.Print(expr.Number(2.0))))),
  ]

  [
    token.While,
    token.LeftParen,
    token.Number(1.0),
    token.RightParen,
    token.Print,
    token.Number(2.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_nested_if_while_statement_test() {
  let wanted = [
    Ok(
      Some(stmt.If(
        expr.Number(1.0),
        stmt.While(expr.Number(2.0), stmt.Print(expr.Number(3.0))),
        None,
      )),
    ),
  ]

  [
    token.If,
    token.LeftParen,
    token.Number(1.0),
    token.RightParen,
    token.While,
    token.LeftParen,
    token.Number(2.0),
    token.RightParen,
    token.Print,
    token.Number(3.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_not_parse_while_statement_missing_condition_test() {
  [
    token.While,
    token.LeftParen,
    token.RightParen,
    token.Number(1.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> pprint.format
  |> birdie.snap(title: "error in parse while statement missing condition")
}

pub fn should_not_parse_while_statement_missing_left_paren_test() {
  [token.While, token.RightParen, token.Number(1.0), token.Semicolon]
  |> parse_wanted
  |> pprint.format
  |> birdie.snap(
    title: "error in parse while statement missing left parentheses",
  )
}

pub fn should_not_parse_while_statement_missing_right_paren_test() {
  let expected_error = Error(ParseError(ExpectRightParentheses, 1))

  let parse_result =
    [
      token.While,
      token.LeftParen,
      token.Number(1.0),
      token.Number(2.0),
      token.Semicolon,
    ]
    |> parse_wanted

  should.be_true(contains(parse_result, expected_error))
}

pub fn should_not_parse_while_statement_missing_body_test() {
  [token.While, token.LeftParen, token.Number(1.0), token.RightParen]
  |> parse_wanted
  |> pprint.format
  |> birdie.snap(title: "error in parse while statement missing body")
}

pub fn should_not_parse_while_statement_with_invalid_condition_test() {
  [
    token.While,
    token.LeftParen,
    token.Minus,
    token.RightParen,
    token.Number(1.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> pprint.format
  |> birdie.snap(title: "error in parse invalid condition in while statement")
}

// parse for statement

pub fn should_parse_for_statement_test() {
  let wanted = [
    Ok(
      Some(
        stmt.Block([
          stmt.Declaration(
            wrap_token_type(token.Identifier("i")),
            Some(Number(0.0)),
          ),
          stmt.While(
            Binary(
              expr.Variable(wrap_token_type(token.Identifier("i"))),
              wrap_token_type(token.Less),
              Number(10.0),
            ),
            stmt.Block([
              stmt.Print(expr.Variable(wrap_token_type(token.Identifier("i")))),
              stmt.Expression(expr.Assign(
                wrap_token_type(token.Identifier("i")),
                Binary(
                  expr.Variable(wrap_token_type(token.Identifier("i"))),
                  wrap_token_type(token.Plus),
                  Number(1.0),
                ),
              )),
            ]),
          ),
        ]),
      ),
    ),
  ]

  [
    token.For,
    token.LeftParen,
    token.Var,
    token.Identifier("i"),
    token.Equal,
    token.Number(0.0),
    token.Semicolon,
    token.Identifier("i"),
    token.Less,
    token.Number(10.0),
    token.Semicolon,
    token.Identifier("i"),
    token.Equal,
    token.Identifier("i"),
    token.Plus,
    token.Number(1.0),
    token.RightParen,
    token.Print,
    token.Identifier("i"),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_for_statement_without_initializer_test() {
  let wanted = [
    Ok(
      Some(
        stmt.Block([
          stmt.EmptyExpression,
          stmt.While(
            Binary(
              expr.Variable(wrap_token_type(token.Identifier("i"))),
              wrap_token_type(token.Less),
              Number(10.0),
            ),
            stmt.Block([
              stmt.Print(Number(1.0)),
              stmt.Expression(expr.Assign(
                wrap_token_type(token.Identifier("i")),
                Binary(
                  expr.Variable(wrap_token_type(token.Identifier("i"))),
                  wrap_token_type(token.Plus),
                  Number(1.0),
                ),
              )),
            ]),
          ),
        ]),
      ),
    ),
  ]

  [
    token.For,
    token.LeftParen,
    token.Semicolon,
    token.Identifier("i"),
    token.Less,
    token.Number(10.0),
    token.Semicolon,
    token.Identifier("i"),
    token.Equal,
    token.Identifier("i"),
    token.Plus,
    token.Number(1.0),
    token.RightParen,
    token.Print,
    token.Number(1.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_for_statement_without_condition_test() {
  let wanted = [
    Ok(
      Some(
        stmt.Block([
          stmt.Declaration(
            wrap_token_type(token.Identifier("i")),
            Some(Number(0.0)),
          ),
          stmt.While(
            Boolean(True),
            stmt.Block([
              stmt.Print(Number(1.0)),
              stmt.Expression(expr.Assign(
                wrap_token_type(token.Identifier("i")),
                Binary(
                  expr.Variable(wrap_token_type(token.Identifier("i"))),
                  wrap_token_type(token.Plus),
                  Number(1.0),
                ),
              )),
            ]),
          ),
        ]),
      ),
    ),
  ]

  [
    token.For,
    token.LeftParen,
    token.Var,
    token.Identifier("i"),
    token.Equal,
    token.Number(0.0),
    token.Semicolon,
    token.Semicolon,
    token.Identifier("i"),
    token.Equal,
    token.Identifier("i"),
    token.Plus,
    token.Number(1.0),
    token.RightParen,
    token.Print,
    token.Number(1.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_for_statement_without_increment_test() {
  let wanted = [
    Ok(
      Some(
        stmt.Block([
          stmt.Declaration(
            wrap_token_type(token.Identifier("i")),
            Some(Number(0.0)),
          ),
          stmt.While(
            Binary(
              expr.Variable(wrap_token_type(token.Identifier("i"))),
              wrap_token_type(token.Less),
              Number(10.0),
            ),
            stmt.Block([stmt.Print(Number(1.0)), stmt.EmptyExpression]),
          ),
        ]),
      ),
    ),
  ]

  [
    token.For,
    token.LeftParen,
    token.Var,
    token.Identifier("i"),
    token.Equal,
    token.Number(0.0),
    token.Semicolon,
    token.Identifier("i"),
    token.Less,
    token.Number(10.0),
    token.Semicolon,
    token.RightParen,
    token.Print,
    token.Number(1.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_parse_empty_for_statement_test() {
  let wanted = [
    Ok(
      Some(
        stmt.Block([
          stmt.EmptyExpression,
          stmt.While(
            Boolean(True),
            stmt.Block([stmt.EmptyExpression, stmt.EmptyExpression]),
          ),
        ]),
      ),
    ),
  ]

  [
    token.For,
    token.LeftParen,
    token.Semicolon,
    token.Semicolon,
    token.RightParen,
    token.Semicolon,
  ]
  |> parse_wanted
  |> should.equal(wanted)
}

pub fn should_not_parse_for_statement_missing_left_paren_test() {
  [
    token.For,
    token.Var,
    token.Identifier("i"),
    token.Equal,
    token.Number(0.0),
    token.Semicolon,
    token.RightParen,
  ]
  |> parse_wanted
  |> pprint.format
  |> birdie.snap(title: "error in parse for statement missing left parentheses")
}

pub fn should_not_parse_for_statement_missing_right_paren_test() {
  [
    token.For,
    token.LeftParen,
    token.Var,
    token.Identifier("i"),
    token.Equal,
    token.Number(0.0),
    token.Semicolon,
    token.Semicolon,
    token.Print,
    token.Number(1.0),
    token.Semicolon,
  ]
  |> parse_wanted
  |> pprint.format
  |> birdie.snap(
    title: "error in parse for statement missing right parentheses",
  )
}

pub fn should_not_parse_for_statement_missing_semicolons_test() {
  [
    token.For,
    token.LeftParen,
    token.Var,
    token.Identifier("i"),
    token.Equal,
    token.Number(0.0),
    token.RightParen,
  ]
  |> parse_wanted
  |> pprint.format
  |> birdie.snap(title: "error in parse for statement missing semicolons")
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
