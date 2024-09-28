//// Syntax formal of Lox:
////    (The whole program is a series of statements.)
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

import gleam
import gleam/bool
import gleam/io
import gleam/iterator.{type Iterator}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

import parse/error.{
  type LexicalError, type ParseError, ExpectExpression, ExpectRightParenthesis,
  ExpectSemicolon, ExpectValue, ExtraneousParenthesis, ExtraneousSemicolon,
  LexError, LexicalError, ParseError,
}
import parse/expr.{type Expr}
import parse/lexer.{type LexResult}
import parse/stmt.{type Stmt, Expression, Print}
import parse/token.{type Token, type TokenType, Token}

// TODO: Could this transform to a iterator too?
pub type Parser {
  Parser(
    tokens: Iterator(LexResult),
    lex_errors: List(LexicalError),
    tok0: Option(Token),
    tok1: Option(Token),
  )
}

// the result of a parsing stage may also Expr and Stmt
pub type Result(t) =
  gleam.Result(t, ParseError)

pub fn new(tokens: Iterator(LexResult)) -> Iterator(Result(Option(Stmt))) {
  Parser(tokens:, lex_errors: [], tok0: None, tok1: None)
  |> advance
  |> advance
  |> to_iter
}

fn to_iter(parser: Parser) -> Iterator(Result(Option(Stmt))) {
  use parser <- iterator.unfold(parser)

  case statement(parser) {
    #(Ok(None), _) -> iterator.Done
    #(rst, parser) -> iterator.Next(rst, parser)
  }
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

pub fn parse(iter: Iterator(Result(Option(Stmt)))) -> List(Result(Option(Stmt))) {
  iterator.to_list(iter)
}

fn statement(parser: Parser) -> #(Result(Option(Stmt)), Parser) {
  case parser.tok0 {
    Some(Token(token.Print, _)) -> print_stmt(parser)
    _ -> expr_stmt(parser)
  }
}

fn expr_stmt(parser: Parser) -> #(Result(Option(Stmt)), Parser) {
  let #(rst, new_parser) = expression(parser)

  use <- bool.lazy_guard(when: result.is_error(rst), return: fn() {
    let assert Error(err) = rst
    #(Error(err), new_parser)
  })
  let assert Ok(maybe_exp) = rst

  case new_parser.tok0 {
    Some(Token(token.Semicolon, line)) ->
      case maybe_exp {
        Some(exp) -> #(Ok(Some(Expression(exp))), advance(new_parser))
        None -> #(
          Error(ParseError(ExtraneousSemicolon, line)),
          advance(new_parser),
        )
      }

    Some(t) -> #(Error(ParseError(ExpectSemicolon, t.line)), new_parser)

    // Valid none expression, and fallback the error line to the line of
    // previous token to handle the terminal semicolon lack.
    //
    // A none expression is a single of EOF.
    None ->
      case maybe_exp {
        Some(_) -> {
          let assert Some(Token(_, line)) = parser.tok0
          #(Error(ParseError(ExpectSemicolon, line)), new_parser)
        }
        None -> #(Ok(None), new_parser)
      }
  }
}

fn print_stmt(parser: Parser) -> #(Result(Option(Stmt)), Parser) {
  let #(rst, parser) = print_stmt_inner(parser)
  case rst {
    Ok(s) -> #(Ok(Some(s)), parser)
    Error(err) -> #(Error(err), parser)
  }
}

fn print_stmt_inner(parser: Parser) -> #(Result(Stmt), Parser) {
  // If this assertion panic, there must be a wrong call
  let assert Some(print_kw) = parser.tok0
  // Consume print keyword and parse expression
  let #(rst, new_parser) = expression(advance(parser))

  let stmt_rst = {
    use maybe_exp <- result.try(rst)
    case new_parser.tok0 {
      Some(Token(token.Semicolon, _)) ->
        case maybe_exp {
          Some(exp) -> Ok(Print(exp))
          None -> Error(ParseError(ExpectExpression, print_kw.line))
        }
      _ -> Error(ParseError(ExpectSemicolon, print_kw.line))
    }
  }

  #(stmt_rst, new_parser)
}

fn expression(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  let #(rst, parser) = equality(parser)
  let rst = case parser.lex_errors {
    [] -> rst
    [err, ..] -> Error(ParseError(LexError(err), err.line))
  }

  #(rst, parser)
}

fn equality(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  comparison(parser)
  |> parse_successive_binary([token.NotEqual, token.EqualEqual], comparison)
}

fn comparison(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  term(parser)
  |> parse_successive_binary(
    match: [token.Greater, token.GreaterEqual, token.Less, token.LessEqual],
    with: term,
  )
}

fn term(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  factor(parser)
  |> parse_successive_binary(match: [token.Minus, token.Plus], with: factor)
}

fn factor(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  unary(parser)
  |> parse_successive_binary(match: [token.Slash, token.Star], with: unary)
}

fn parse_successive_binary(
  pair: #(Result(Option(Expr)), Parser),
  match operators: List(TokenType),
  with parse_func: fn(Parser) -> #(Result(Option(Expr)), Parser),
) -> #(Result(Option(Expr)), Parser) {
  let #(rst, parser) = pair
  use <- bool.guard(!is_successive(parser, rst, operators), pair)

  // Safe unwrap after guarantee
  let assert Ok(Some(left)) = rst
  let assert Some(op) = parser.tok0

  let #(right_rst, right_parser) as right_pair = parse_func(advance(parser))
  case right_rst {
    Ok(Some(right)) ->
      // parse successive pattern exhaustively
      #(Ok(Some(expr.Binary(left, op, right))), right_parser)
      |> parse_successive_binary(match: operators, with: parse_func)

    Ok(None) -> #(Error(ParseError(ExpectValue, op.line)), right_parser)
    Error(_) -> right_pair
  }
}

/// is_successive guarantee the left operand is valid parse_expression as well as
/// the current token matches the given operators.
fn is_successive(
  parser: Parser,
  rst: Result(Option(Expr)),
  operators: List(TokenType),
) -> Bool {
  // Guarantee left operand is a valid non-empty expression
  let valid_left_operand =
    rst |> option.from_result |> option.flatten |> option.is_some

  let match_operator = case parser.tok0 {
    Some(Token(t, _)) -> list.contains(operators, t)
    None -> False
  }

  valid_left_operand && match_operator
}

fn unary(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  case parser.tok0 {
    Some(Token(token.Bang, _) as op) | Some(Token(token.Minus, _) as op) -> {
      let #(rst, right_parser) as pair = unary(advance(parser))
      case rst {
        Ok(Some(right)) -> {
          let new_exp = expr.Unary(op, right)
          #(Ok(Some(new_exp)), right_parser)
          // |> unary
        }
        Ok(None) -> #(Error(ParseError(ExpectValue, op.line)), right_parser)
        Error(_) -> pair
      }
    }

    // Non-unary operator
    _ -> primary(parser)
  }
}

fn primary(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  case parser.tok0 {
    // Literal
    Some(Token(token.Number(n), _)) -> #(
      Ok(Some(expr.Literal(expr.Number(n)))),
      advance(parser),
    )
    Some(Token(token.String(s), _)) -> #(
      Ok(Some(expr.Literal(expr.String(s)))),
      advance(parser),
    )
    Some(Token(token.True, _)) -> #(
      Ok(Some(expr.Literal(expr.Bool(True)))),
      advance(parser),
    )
    Some(Token(token.False, _)) -> #(
      Ok(Some(expr.Literal(expr.Bool(False)))),
      advance(parser),
    )
    Some(Token(token.NilLiteral, _)) -> #(
      Ok(Some(expr.Literal(expr.NilLiteral))),
      advance(parser),
    )

    // Grouping
    Some(Token(token.LeftParen, line)) ->
      case parser.tok1 {
        // Empty grouping
        Some(Token(token.RightParen, _)) -> #(
          Ok(Some(expr.Grouping(None))),
          advance(advance(parser)),
        )

        // Non-empty grouping
        _ -> {
          let #(inner_expr, sub_parser) = expression(advance(parser))
          case inner_expr, sub_parser.tok0 {
            Ok(e), Some(Token(token.RightParen, _)) -> #(
              Ok(Some(expr.Grouping(e))),
              advance(sub_parser),
            )
            Ok(_), _ -> #(
              Error(ParseError(ExpectRightParenthesis, line)),
              sub_parser,
            )
            Error(_), _ -> #(inner_expr, sub_parser)
          }
        }
      }

    Some(Token(token.RightParen, line)) -> #(
      Error(ParseError(ExtraneousParenthesis, line)),
      parser,
    )

    Some(t) -> #(
      Error(ParseError(error.UnexpectedToken(t.type_), t.line)),
      advance(parser),
    )

    // End of tokens stream
    None -> #(Ok(None), parser)
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
// fn match(left: Option(Token), right: Option(Token)) -> Bool {
//   case left, right {
//     Some(Token(tl, _)), Some(Token(tr, _)) if tl == tr -> True
//     None, None -> True
//     _, _ -> False
//   }
// }
