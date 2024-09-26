//// Syntax formal of Lox:
////    program -> statement* ;
////
////    statement -> expr_stmt | print_stmt ;
////
////    expr_stmt -> expression ";" ;
////
////    print_stmt -> "print" expression ";" ;
////
////    expression -> equality ;
////
////    equality -> comparison ( ( "!=" | "==" ) comparison )* ;
////
////    comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
////
////    term -> factor ( ( "+" | "-" ) factor )* ;
////
////    factor -> unary ( ( "/" | "*" ) unary)* ;
////
////    unary -> ( "!" | "-" ) unary | literal;
////
////    literal -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;

import gleam/bool
import gleam/list
import gleam/option.{type Option, None, Some}

import gleam/iterator.{type Iterator}
import parse/error.{
  type LexicalError, type ParseError, ExpectExpression, ExpectValue,
  ExtraneousParenthesis, LexError, LexicalError, ParseError,
  UnclosingParenthesis,
}
import parse/expr.{type Expr}
import parse/lexer.{type LexResult}
import parse/token.{type Token, type TokenType, Token}

// TODO: Could this transform to a iterator too?
pub type Parser {
  Parser(
    tokens: Iterator(LexResult),
    result: ParseResult,
    lex_errors: List(LexicalError),
    tok0: Option(Token),
    tok1: Option(Token),
  )
}

pub type ParseResult =
  Result(Option(Expr), ParseError)

pub fn new(tokens: Iterator(LexResult)) -> Parser {
  Parser(tokens:, result: Ok(None), lex_errors: [], tok0: None, tok1: None)
  |> advance
  |> advance
}

fn advance(parser: Parser) -> Parser {
  case iterator.step(parser.tokens) {
    iterator.Next(Ok(token), rest) ->
      Parser(..parser, tokens: rest, tok0: parser.tok1, tok1: Some(token))
    iterator.Next(Error(err), rest) ->
      Parser(
        ..parser,
        tokens: rest,
        lex_errors: list.append(parser.lex_errors, [err]),
      )
      |> advance
    iterator.Done -> Parser(..parser, tok0: parser.tok1, tok1: None)
  }
}

pub fn parse(parser: Parser) -> ParseResult {
  let parser = expression(parser)
  let parser = case parser.lex_errors {
    [] -> parser
    [err, ..] ->
      Parser(..parser, result: Error(ParseError(LexError(err), err.line)))
  }
  parser.result
}

fn program(parser: Parser) -> Parser {
  todo
}

fn statement(parser: Parser) -> Parser {
  todo
}

fn expr_stmt(parser: Parser) -> Parser {
  todo
}

fn print_stmt(parser: Parser) -> Parser {
  todo
}

fn expression(parser: Parser) -> Parser {
  equality(parser)
}

fn equality(parser: Parser) -> Parser {
  comparison(parser)
  |> parse_successive_binary([token.NotEqual, token.EqualEqual], comparison)
}

fn comparison(parser: Parser) -> Parser {
  term(parser)
  |> parse_successive_binary(
    match: [token.Greater, token.GreaterEqual, token.Less, token.LessEqual],
    with: term,
  )
}

fn term(parser: Parser) -> Parser {
  factor(parser)
  |> parse_successive_binary(match: [token.Minus, token.Plus], with: factor)
}

fn factor(parser: Parser) -> Parser {
  unary(parser)
  |> parse_successive_binary(match: [token.Slash, token.Star], with: unary)
}

fn parse_successive_binary(
  parser: Parser,
  match operators: List(TokenType),
  with func: fn(Parser) -> Parser,
) -> Parser {
  use <- bool.guard(is_successive(parser, operators), parser)

  let assert Ok(Some(left)) = parser.result
  let assert Some(op) = parser.tok0

  let right_parser = func(advance(parser))
  case right_parser.result {
    Ok(Some(right)) -> {
      let new_exp = expr.Binary(left, op, right)
      Parser(..right_parser, result: Ok(Some(new_exp)))
      |> parse_successive_binary(operators, func)
    }
    Ok(None) ->
      Parser(..right_parser, result: Error(ParseError(ExpectValue, op.line)))
    Error(_) -> right_parser
  }
}

fn is_successive(parser: Parser, operators: List(TokenType)) -> Bool {
  let have_left_operand = case parser.result {
    Ok(Some(_)) -> True
    _ -> False
  }
  let current_token = option.unwrap(parser.tok0, Token(token.EOF, 1)).type_
  let match_operator = list.contains(operators, current_token)
  !have_left_operand || !match_operator
}

fn unary(parser: Parser) -> Parser {
  case parser.tok0 {
    Some(Token(token.Bang, _) as op) | Some(Token(token.Minus, _) as op) -> {
      let right_parser = unary(advance(parser))
      case right_parser.result {
        Ok(Some(right)) -> {
          let new_exp = expr.Unary(op, right)
          Parser(..right_parser, result: Ok(Some(new_exp)))
          // |> unary
        }
        Ok(None) ->
          Parser(
            ..right_parser,
            result: Error(ParseError(ExpectValue, op.line)),
          )
        Error(_) -> right_parser
      }
    }
    _ -> primary(parser)
  }
}

fn primary(parser: Parser) -> Parser {
  case parser.tok0 {
    Some(Token(token.Number(n), _)) ->
      Parser(..parser, result: Ok(Some(expr.Literal(expr.Number(n)))))
      |> advance
    Some(Token(token.String(s), _)) ->
      Parser(..parser, result: Ok(Some(expr.Literal(expr.String(s)))))
      |> advance
    Some(Token(token.True, _)) ->
      Parser(..parser, result: Ok(Some(expr.Literal(expr.Bool(True)))))
      |> advance
    Some(Token(token.False, _)) ->
      Parser(..parser, result: Ok(Some(expr.Literal(expr.Bool(False)))))
      |> advance
    Some(Token(token.NilLiteral, _)) ->
      Parser(..parser, result: Ok(Some(expr.Literal(expr.NilLiteral))))
      |> advance
    Some(Token(token.LeftParen, line)) ->
      case parser.tok1 {
        // consume empty grouping
        Some(Token(token.RightParen, _)) ->
          Parser(..parser, result: Ok(Some(expr.Grouping(None))))
          |> advance
          |> advance
        // consume non-empty grouping
        _ -> {
          let sub_parser =
            expression(advance(Parser(..parser, result: Ok(None))))
          case sub_parser.result, sub_parser.tok0 {
            Ok(e), Some(Token(token.RightParen, _)) ->
              Parser(..sub_parser, result: Ok(Some(expr.Grouping(e))))
              |> advance
            Ok(_), _ ->
              Parser(
                ..sub_parser,
                result: Error(ParseError(UnclosingParenthesis, line)),
              )
            Error(_), _ -> sub_parser
          }
        }
      }
    Some(Token(token.RightParen, line)) ->
      Parser(..parser, result: Error(ParseError(ExtraneousParenthesis, line)))
    Some(t) ->
      Parser(..parser, result: Error(ParseError(ExpectExpression, t.line)))
    _ -> parser
  }
}

// Used in future.
fn synchronize(parser: Parser) -> Parser {
  let parser = advance(parser)

  case parser.tok0, parser.tok1 {
    Some(Token(token.Semicolon, _)), _ | None, _ -> parser
    _, Some(Token(token.Class, _))
    | _, Some(Token(token.Fun, _))
    | _, Some(Token(token.Var, _))
    | _, Some(Token(token.For, _))
    | _, Some(Token(token.If, _))
    | _, Some(Token(token.While, _))
    | _, Some(Token(token.Print, _))
    | _, Some(Token(token.Return, _))
    -> parser
    _, _ -> synchronize(parser)
  }
}
