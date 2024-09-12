import gleam/bool
import gleam/io
import gleam/iterator.{type Iterator}
import gleam/list
import gleam/option.{type Option, None, Some}

import expr.{type Expr}
import parse/lexer.{type LexResult, type LexicalError}
import parse/token.{type Token, type TokenType, Token}

pub type Parser {
  Parser(
    tokens: Iterator(LexResult),
    result: ParseResult,
    lex_errors: List(LexicalError),
    tok0: Option(Token),
    tok1: Option(Token),
  )
}

pub type ParseError {
  ParseError(error: ParseErrorType, line: Int)
}

pub type ParseErrorType {
  ExpectedValue
  UnexpectedToken(TokenType)
  LexError(lexer.LexicalErrorType)
}

pub type ParseResult =
  Result(Option(Expr), ParseError)

pub fn new(tokens: Iterator(LexResult)) -> Parser {
  Parser(tokens:, result: Ok(None), lex_errors: [], tok0: None, tok1: None)
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
  todo
}

fn expression(parser: Parser) -> Parser {
  equality(parser)
}

fn equality(parser: Parser) -> Parser {
  comparison(parser)
  |> parse_successive_equality
}

fn parse_successive_equality(parser: Parser) -> Parser {
  case parser.result, parser.tok0 {
    Ok(Some(left)), Some(Token(token.NotEqual, _) as op)
    | Ok(Some(left)), Some(Token(token.EqualEqual, _) as op)
    -> {
      let right_parser = comparison(advance(parser))
      case right_parser.result {
        Ok(Some(right)) -> {
          let new_exp = expr.Binary(left, op, right)
          Parser(..right_parser, result: Ok(Some(new_exp)))
        }
        Ok(None) ->
          Parser(
            ..right_parser,
            result: Error(ParseError(ExpectedValue, op.line)),
          )
        Error(_) -> right_parser
      }
    }
    // no successive equality or previous is error, continue
    _, _ -> parser
  }
}

fn comparison(parser: Parser) -> Parser {
  todo
  //   term(parser)
  //   |> parse_binary_op(
  //     [token.Greater, token.GreaterEqual, token.Less, token.LessEqual],
  //     term,
  //   )
}
// fn term(parser: Parser) -> Parser {
//   factor(parser)
//   |> parse_binary_op([token.Minus, token.Plus], factor)
// }

// fn factor(parser: Parser) -> Parser {
//   unary(parser)
//   |> parse_binary_op([token.Slash, token.Star], unary)
// }

// fn parse_binary_op(
//   parser: Parser,
//   expected: List(TokenType),
//   callback: fn(Parser) -> Parser,
// ) -> Parser {
//   case parser.tokens {
//     [op, ..rest] -> {
//       use <- bool.guard(!list.contains(expected, op.token_type), parser)

//       let sub_p = callback(Parser(..parser, tokens: rest))
//       let result = case parser.result, sub_p.result {
//         Ok(Some(left)), Ok(Some(right)) ->
//           Ok(Some(expr.Binary(left, op, right)))
//         _, _ -> Error(ParseError)
//       }
//       Parser(..parser, result:)
//     }
//     _ -> parser
//   }
// }

// fn unary(parser: Parser) -> Parser {
//   case parser.tokens {
//     [Token(token.Bang, _) as op, ..rest] | [Token(token.Minus, _) as op, ..rest] ->
//       case unary(Parser(..parser, tokens: rest)) {
//         Parser(result: Ok(Some(right)), ..) ->
//           Parser(..parser, result: Ok(Some(expr.Unary(op, right))))
//         _ -> Parser(..parser, result: Error(ParseError))
//       }
//     _ -> primary(parser)
//   }
// }

// fn primary(parser: Parser) -> Parser {
//   case parser.tokens {
//     [Token(token.Number(n), _), ..rest] ->
//       Parser(tokens: rest, result: Ok(Some(expr.Literal(expr.Number(n)))))
//     [Token(token.String(s), _), ..rest] ->
//       Parser(tokens: rest, result: Ok(Some(expr.Literal(expr.String(s)))))
//     [Token(token.True, _), ..rest] ->
//       Parser(tokens: rest, result: Ok(Some(expr.Literal(expr.Bool(True)))))
//     [Token(token.False, _), ..rest] ->
//       Parser(tokens: rest, result: Ok(Some(expr.Literal(expr.Bool(False)))))
//     [Token(token.NilLiteral, _), ..rest] ->
//       Parser(tokens: rest, result: Ok(Some(expr.Literal(expr.NilLiteral(Nil)))))
//     [Token(token.LeftParen, _), ..rest] -> {
//       let sub_parser = expression(from_tokens(rest))
//       case sub_parser {
//         Parser([Token(token.RightParen, _), ..rest], result: Ok(e)) ->
//           Parser(tokens: rest, result: Ok(Some(expr.Grouping(e))))
//         _ -> Parser(tokens: rest, result: Error(ParseError))
//       }
//     }
//     _ -> parser
//   }
// }
