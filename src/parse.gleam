//// This module encapsulates the parsing logic for the Lox programming language,
//// translating a sequence of tokens into an Abstract Syntax Tree (AST). It
//// follows the formal grammar of Lox, dealing with statements, expressions,
//// and error handling.
////
//// ## Core Features
////
//// - **Token Processing**: Sequentially processes tokens to construct the
////   program's structure.
//// - **Grammar Adherence**: Aligns with Lox's grammar rules for statements,
////   expressions, and operator precedences.
//// - **Error Management**: Detects and reports lexical and syntax errors,
////   including recovery mechanisms.
////
//// ## Syntax formal of Lox
////
////    program -> declaration* ;
////    declaration -> var_decl | statement;
////    var_decl -> "var" IDENTIFIER ( "=" expression )? ";" ;
////    statement -> expr_stmt | print_stmt ;
////    expr_stmt -> expression ";" ;
////    print_stmt -> "print" expression ";" ;
////    expression -> equality ;
////    equality -> comparison ( ( "!=" | "==" ) comparison )* ;
////    comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
////    term -> factor ( ( "+" | "-" ) factor )* ;
////    factor -> unary ( ( "/" | "*" ) unary)* ;
////    unary -> ( "!" | "-" ) unary | literal;
////    literal -> "true" | "false" | "nil"
////             | NUMBER | STRING | "(" expression ")" | IDENTIFIER ;
////
//// ## Public API
////
//// - **`new(tokens: Iterator(LexResult)) -> Iterator(Result(Option(Stmt)))`**
////   Initializes the parser with an iterator of lexical analysis results.
////   Returns an iterator of parsing outcomes, each being a statement wrapped
////   in a `Result` and an optional indicating the end of input.
////
//// - **`parse(iter: Iterator(Result(Option(Stmt)))) -> List(Result(Option(Stmt)))`**
////   Consumes an iterator of parse results, collecting them into a list for
////   further processing or inspection.
////
//// - **Other Exposed Types**
////   - `Parser`: Represents the parser state and drives the parsing process.
////   - `Result(t)`: Wraps parsing results or errors, using `ParseError` to
////     communicate syntax issues.
////   - Functions like `statement`, `expr_stmt`, and `print_stmt` directly
////     contribute to parsing statements and expressions, although they are
////     typically used internally.
////
//// ## Usage Example
////
//// ```gleam
//// use parse.*
////
//// // Assuming `tokens` is an iterator of tokens obtained from lexing source code
//// let parse_outcomes = tokens |> new() |> parse()
////
//// // Iterate over outcomes to handle parsed statements or errors
//// let valid_statements = list.filter(parse_outcomes, result.is_ok)
//// ...
//// ```
////
//// ## Future Enhancements
//// - Refinement in error recovery strategies to better handle malformed code.
//// - Extension to support upcoming Lox language features or custom language extensions.

import gleam
import gleam/bool
import gleam/iterator.{type Iterator}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

import parse/error.{
  type LexicalError, type ParseError, ExpectExpression, ExpectRightParenthesis,
  ExpectSemicolon, ExpectValue, ExpectVariableName, ExtraneousParenthesis,
  ExtraneousSemicolon, LexError, LexicalError, ParseError, UnexpectedToken,
}

import parse/expr.{type Expr}
import parse/lexer.{type LexResult}
import parse/stmt.{type Stmt, Declaration, Expression, Print}
import parse/token.{type Token, type TokenType, Token}

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

  case declaration(parser) {
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

fn declaration(parser: Parser) -> #(Result(Option(Stmt)), Parser) {
  let #(rst, parser) = case parser.tok0 {
    Some(Token(token.Var, _)) -> var_declaration(parser)
    _ -> statement(parser)
  }
  case rst, parser {
    Error(_), parser -> #(rst, synchronize(parser))
    Ok(_), _ -> #(rst, parser)
  }
}

fn var_declaration(parser: Parser) -> #(Result(Option(Stmt)), Parser) {
  let #(rst, parser) = var_declaration_inner(parser)
  case rst {
    Ok(s) -> #(Ok(Some(s)), parser)
    Error(err) -> #(Error(err), parser)
  }
}

/// var_declaration_inner parse a result of statement, because a `None` variable
/// declaration doesn't make sense.
fn var_declaration_inner(parser: Parser) -> #(Result(Stmt), Parser) {
  let assert Some(var_kw) = parser.tok0
  let parser = advance(parser)

  use <- bool.guard(!match(parser, token.Identifier("")), #(
    Error(ParseError(ExpectVariableName, var_kw.line)),
    parser,
  ))
  let assert Some(var_name) = parser.tok0
  let parser = advance(parser)

  // FIXME: should consume semicolon correctly
  case parser.tok0 {
    // initialized variable declaration
    Some(Token(token.Equal, eq_line)) -> {
      let #(rst, parser) = advance(parser) |> expression
      let parse_decl_rst = case rst {
        Ok(Some(_) as initializer) ->
          case parser.tok0 {
            Some(Token(token.Semicolon, _)) ->
              Ok(Declaration(var_name, initializer))
            _ -> Error(ParseError(ExpectSemicolon, var_kw.line))
          }
        Ok(None) -> Error(ParseError(ExpectValue, eq_line))
        Error(err) -> Error(err)
      }
      let new_parser = case parse_decl_rst {
        Ok(_) -> advance(parser)
        _ -> parser
      }

      #(parse_decl_rst, new_parser)
    }

    // variable declaration without initialization
    Some(Token(token.Semicolon, _)) -> #(
      Ok(Declaration(var_name, None)),
      advance(parser),
    )

    _ -> #(Error(ParseError(ExpectSemicolon, var_kw.line)), parser)
  }
}

fn statement(parser: Parser) -> #(Result(Option(Stmt)), Parser) {
  case parser.tok0 {
    Some(Token(token.Print, _)) -> print_stmt(parser)
    _ -> expr_stmt(parser)
  }
}

fn expr_stmt(parser: Parser) -> #(Result(Option(Stmt)), Parser) {
  let #(expr_rst, new_parser) = expression(parser)

  let rst = {
    // Try to consume semicolon
    use maybe_exp <- result.try(expr_rst)
    case new_parser.tok0 {
      Some(Token(token.Semicolon, line)) ->
        case maybe_exp {
          Some(exp) -> Ok(Some(Expression(exp)))
          None -> Error(ParseError(ExtraneousSemicolon, line))
        }

      Some(t) -> Error(ParseError(ExpectSemicolon, t.line))

      // Valid none expression, and fallback the error line to the line of
      // previous token to handle the terminal semicolon lack.
      //
      // A none expression is a single of EOF.
      None ->
        case maybe_exp {
          Some(_) -> {
            let assert Some(Token(_, line)) = parser.tok0
            Error(ParseError(ExpectSemicolon, line))
          }
          None -> Ok(None)
        }
    }
  }

  let new_parser = case new_parser.tok0 {
    Some(Token(token.Semicolon, _)) -> advance(new_parser)
    _ -> new_parser
  }

  #(rst, new_parser)
}

fn print_stmt(parser: Parser) -> #(Result(Option(Stmt)), Parser) {
  let #(rst, parser) = print_stmt_inner(parser)
  case rst {
    Ok(s) -> #(Ok(Some(s)), parser)
    Error(err) -> #(Error(err), parser)
  }
}

fn print_stmt_inner(parser: Parser) -> #(Result(Stmt), Parser) {
  // remain the print token in order to get line for error report.
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
    // end parsing if encounter semicolon
    Some(Token(token.Semicolon, _)) -> #(Ok(None), parser)

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

    // Variable
    Some(Token(token.Identifier(_), _) as tok) -> #(
      Ok(Some(expr.Variable(tok))),
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
      Error(ParseError(UnexpectedToken(t.type_), t.line)),
      advance(parser),
    )

    // End of tokens stream
    None -> #(Ok(None), parser)
  }
}

fn synchronize(parser: Parser) -> Parser {
  let #(tok0, tok1) = #(parser.tok0, parser.tok1)
  let parser = advance(parser)

  case tok0, tok1 {
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

fn match(parser: Parser, token_type: TokenType) -> Bool {
  case parser.tok0, token_type {
    Some(Token(token.Number(_), _)), token.Number(_) -> True
    Some(Token(token.String(_), _)), token.String(_) -> True
    Some(Token(token.Identifier(_), _)), token.Identifier(_) -> True
    Some(Token(type_, _)), _ if type_ == token_type -> True
    _, _ -> False
  }
}
