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
//// This part shows the priority of the statements and expressions.
////
////    program -> declaration* ;
////    declaration -> var_decl | statement;
////    var_decl -> "var" IDENTIFIER ( "=" expression )? ";" ;
////    statement -> expr_stmt
////                | for_stmt
////                | if_stmt
////                | print_stmt
////                | while_stmt
////                | block;
////    expr_stmt -> expression ";" ;
////    if_stmt -> "if" "(" expression ")" statement
////               ( "else" statement )? ;
////    print_stmt -> "print" expression ";" ;
////    while_stmt -> "while" "(" expression ")" statement ;
////    block -> "{" declaration* "}" ;
////
////    expression -> assignment ｜ equality ;
////    assignment -> IDENTIFIER "=" assignment ;
////                | logic_or ;
////    logic_or -> logic_and ( "or" logic_and )* ;
////    logic_and -> equality ( "and" equality )* ;
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

import gleam
import gleam/bool
import gleam/iterator.{type Iterator}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

import parse/error.{
  type LexicalError, type ParseError, ExpectExpression, ExpectLeftParentheses,
  ExpectLeftValue, ExpectRightParentheses, ExpectRightValue, ExpectSemicolon,
  ExpectStatement, ExpectVariableName, ExtraneousParenthesis,
  ExtraneousSemicolon, InvalidAssignmentTarget, LexError, LexicalError,
  ParseError, UnexpectedToken,
}
import parse/expr.{
  type Expr, Assign, Binary, Boolean, Grouping, LogicAnd, LogicOr, NegativeBool,
  NegativeNumber, NilLiteral, Number, String, Variable,
}
import parse/lexer.{type LexResult}
import parse/stmt.{type Stmt, Block, Declaration, Expression, If, Print, While}
import parse/token.{type Token, type TokenType, Token}
import prelude.{with_ok}

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

pub fn new(tokens: Iterator(LexResult)) -> Parser {
  Parser(tokens:, lex_errors: [], tok0: None, tok1: None)
  |> advance
  |> advance
}

pub fn from_source(source: String) -> Parser {
  lexer.new(source)
  |> new
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

pub fn parse(parser: Parser) -> List(Result(Option(Stmt))) {
  parser
  |> to_iter
  |> iterator.to_list
}

fn to_iter(parser: Parser) -> Iterator(Result(Option(Stmt))) {
  use parser <- iterator.unfold(parser)

  case declaration(parser) {
    #(Ok(None), _) -> iterator.Done
    #(rst, parser) -> iterator.Next(rst, parser)
  }
}

// Parsing enterpoint
pub fn declaration(parser: Parser) -> #(Result(Option(Stmt)), Parser) {
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
  let assert Some(Token(token.Var, var_line)) = parser.tok0
  let parser = advance(parser)

  use var_name, parser <- match_identifier(parser, #(
    Error(ParseError(ExpectVariableName, var_line)),
    parser,
  ))

  case parser.tok0 {
    // variable declaration with initializer
    Some(Token(token.Equal, eq_line)) -> {
      let #(rst, parser) = expression(advance(parser))

      use maybe_init <- with_ok(rst, parser)

      use <- bool.guard(when: option.is_none(maybe_init), return: #(
        Error(ParseError(ExpectRightValue, eq_line)),
        advance(parser),
      ))

      // Don't need to unwrap maybe_init here, because the constructor receive a option.
      use _, parser <- match(parser, type_of: token.Semicolon, otherwise: #(
        Error(ParseError(ExpectSemicolon, var_line)),
        parser,
      ))

      #(Ok(Declaration(name: var_name, initializer: maybe_init)), parser)
    }

    // variable declaration without initialization
    Some(Token(token.Semicolon, _)) -> #(
      Ok(Declaration(var_name, None)),
      advance(parser),
    )

    _ -> #(Error(ParseError(ExpectSemicolon, var_line)), parser)
  }
}

fn statement(parser: Parser) -> #(Result(Option(Stmt)), Parser) {
  case parser.tok0 {
    Some(Token(token.If, line)) -> if_stmt(advance(parser), line)
    Some(Token(token.While, line)) -> while_stmt(advance(parser), line)
    Some(Token(token.Print, line)) -> print_stmt(advance(parser), line)
    Some(Token(token.LeftBrace, line)) -> block(advance(parser), line)
    _ -> expr_stmt(parser)
  }
}

/// if_stmt eagerly looks for the "else"" token, thus the else branch will be
/// bound to the nearest if statement.
fn if_stmt(
  parser: Parser,
  start_at line: Int,
) -> #(Result(Option(Stmt)), Parser) {
  let #(rst, parser) = if_stmt_inner(parser, line)
  use rst <- with_ok(in: rst, processer: parser)

  #(Ok(Some(rst)), parser)
}

fn if_stmt_inner(parser, start_at line: Int) -> #(Result(Stmt), Parser) {
  use condition, end_line, parser <- ensure_condition_valid_return_with_pos(
    parser,
    start_at: line,
  )

  // parse branches
  let #(then_branch_rst, parser) = statement(parser)
  use maybe_then <- with_ok(then_branch_rst, parser)

  use <- bool.guard(when: option.is_none(maybe_then), return: #(
    Error(ParseError(ExpectStatement, end_line)),
    parser,
  ))
  let assert Some(then_branch) = maybe_then

  use else_kw, parser <- match(parser, type_of: token.Else, otherwise: #(
    Ok(If(condition:, then_branch:, else_branch: None)),
    parser,
  ))
  let #(else_branch_rst, parser) = statement(parser)
  use maybe_else <- with_ok(else_branch_rst, parser)
  case maybe_else {
    None -> #(Error(ParseError(ExpectStatement, else_kw.line)), parser)
    else_branch -> #(Ok(If(condition:, then_branch:, else_branch:)), parser)
  }
}

fn while_stmt(
  parser: Parser,
  start_at line: Int,
) -> #(Result(Option(Stmt)), Parser) {
  use condition, end_line, parser <- ensure_condition_valid_return_with_pos(
    parser,
    start_at: line,
  )

  let #(body_rst, parser) = statement(parser)
  use maybe_body <- with_ok(in: body_rst, processer: parser)

  case maybe_body {
    Some(body) -> #(Ok(Some(While(condition:, body:))), parser)
    None -> #(Error(ParseError(ExpectStatement, end_line)), parser)
  }
}

fn ensure_condition_valid_return_with_pos(
  parser,
  start_at line: Int,
  with fun: fn(Expr, Int, Parser) -> #(Result(any), Parser),
) -> #(Result(any), Parser) {
  let #(cond_rst, parser) = parse_condition(parser, line)
  use #(condition, end_line) <- with_ok(in: cond_rst, processer: parser)
  fun(condition, end_line, parser)
}

fn parse_condition(
  parser,
  start_at line: Int,
) -> #(gleam.Result(#(Expr, Int), ParseError), Parser) {
  use left_p, parser <- match(parser, type_of: token.LeftParen, otherwise: #(
    Error(ParseError(ExpectLeftParentheses, line)),
    parser,
  ))

  // ensure parse Ok and condition is not none. 
  let #(cond_rst, parser) = expression(parser)
  use maybe_cond <- with_ok(cond_rst, parser)
  use <- bool.guard(when: option.is_none(maybe_cond), return: #(
    Error(ParseError(ExpectExpression, left_p.line)),
    parser,
  ))
  let assert Some(cond) = maybe_cond

  use right_p, parser <- match(parser, type_of: token.RightParen, otherwise: #(
    Error(ParseError(ExpectRightParentheses, left_p.line)),
    parser,
  ))

  #(Ok(#(cond, right_p.line)), parser)
}

fn block(parser: Parser, start_at line: Int) -> #(Result(Option(Stmt)), Parser) {
  let #(rst, parser) = block_inner(parser, line, [])
  use statements <- with_ok(rst, parser)

  #(Ok(Some(Block(statements))), advance(parser))
}

fn block_inner(
  parser: Parser,
  start_at line: Int,
  acc acc: List(Stmt),
) -> #(Result(List(Stmt)), Parser) {
  case parser.tok0 {
    None -> #(Error(ParseError(error.ExpectRightBrace, line)), parser)
    Some(Token(token.RightBrace, _)) -> #(Ok(acc), parser)
    Some(_) -> {
      let #(rst, parser) = declaration(parser)
      case rst {
        Error(err) -> #(Error(err), parser)
        Ok(None) -> block_inner(parser, line, acc)
        Ok(Some(s)) -> block_inner(parser, line, list.append(acc, [s]))
      }
    }
  }
}

fn print_stmt(
  parser: Parser,
  start_at line: Int,
) -> #(Result(Option(Stmt)), Parser) {
  let #(rst, parser) = print_stmt_inner(parser, line)
  case rst {
    Ok(s) -> #(Ok(Some(s)), parser)
    Error(err) -> #(Error(err), parser)
  }
}

fn print_stmt_inner(
  parser: Parser,
  start_at line: Int,
) -> #(Result(Stmt), Parser) {
  let #(rst, parser) = expression(parser)

  use maybe_expr <- with_ok(rst, parser)
  case parser.tok0, maybe_expr {
    Some(Token(token.Semicolon, _)), Some(expr) -> #(
      Ok(Print(expr)),
      advance(parser),
    )
    Some(Token(token.Semicolon, _)), None -> #(
      Error(ParseError(ExpectExpression, line)),
      advance(parser),
    )

    _, _ -> #(Error(ParseError(ExpectSemicolon, line)), parser)
  }
}

pub fn expression(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  let #(rst, parser) = assignment(parser)
  let rst = case parser.lex_errors {
    [] -> rst
    [err, ..] -> Error(ParseError(LexError(err), err.line))
  }

  #(rst, parser)
}

fn expr_stmt(parser: Parser) -> #(Result(Option(Stmt)), Parser) {
  let expr_begin_line =
    parser.tok0
    |> option.map(fn(tok) { tok.line })
    |> option.unwrap(0)

  let #(expr_rst, parser) = expression(parser)

  use maybe_exp <- with_ok(expr_rst, parser)
  case parser.tok0, maybe_exp {
    Some(Token(token.Semicolon, _)), Some(expr) -> #(
      Ok(Some(Expression(expr))),
      advance(parser),
    )
    Some(Token(token.Semicolon, line)), None -> #(
      Error(ParseError(ExtraneousSemicolon, line)),
      advance(parser),
    )

    Some(t), _ -> #(Error(ParseError(ExpectSemicolon, t.line)), parser)

    // Valid none expression, representing a single of EOF.
    None, Some(_) -> #(
      Error(ParseError(ExpectSemicolon, expr_begin_line)),
      parser,
    )
    None, None -> #(Ok(None), parser)
  }
}

fn assignment(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  let #(rst, parser) as left_val = logic_or(parser)

  // Successive double equal has consumed by the equality(), so here should be
  // the single equal.
  use equals, parser <- match(parser, token.Equal, left_val)
  // why advance?
  let #(value_rst, parser) = assignment(advance(parser))

  let rst = {
    use maybe_expr <- result.try(rst)
    // Detect whether assignment target a valid left-value expression
    case maybe_expr {
      Some(Variable(name)) ->
        case value_rst {
          Ok(Some(value)) -> Ok(Some(Assign(name, value)))
          Ok(None) -> Error(ParseError(ExpectRightValue, equals.line))
          Error(_) -> value_rst
        }

      Some(_) -> Error(ParseError(InvalidAssignmentTarget, equals.line))
      _ -> Error(ParseError(ExpectLeftValue, equals.line))
    }
  }

  #(rst, parser)
}

fn logic_or(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  logic_and(parser)
  |> parse_successive_binary(
    match: [token.Or],
    constructor: LogicOr,
    with: logic_and,
  )
}

fn logic_and(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  equality(parser)
  |> parse_successive_binary(
    match: [token.And],
    constructor: LogicAnd,
    with: equality,
  )
}

fn equality(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  comparison(parser)
  |> parse_successive_binary(
    match: [token.NotEqual, token.EqualEqual],
    constructor: Binary,
    with: comparison,
  )
}

fn comparison(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  term(parser)
  |> parse_successive_binary(
    match: [token.Greater, token.GreaterEqual, token.Less, token.LessEqual],
    constructor: Binary,
    with: term,
  )
}

fn term(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  factor(parser)
  |> parse_successive_binary(
    match: [token.Minus, token.Plus],
    constructor: Binary,
    with: factor,
  )
}

fn factor(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  unary(parser)
  |> parse_successive_binary(
    match: [token.Slash, token.Star],
    constructor: Binary,
    with: unary,
  )
}

fn parse_successive_binary(
  pair: #(Result(Option(Expr)), Parser),
  match operators: List(TokenType),
  constructor constructor: fn(Expr, Token, Expr) -> Expr,
  with parse_func: fn(Parser) -> #(Result(Option(Expr)), Parser),
) -> #(Result(Option(Expr)), Parser) {
  let #(rst, parser) = pair
  use left, op <- unless_is_successive_bianry(parser, rst, operators, pair)

  let #(right_rst, right_parser) as right_pair = parse_func(advance(parser))
  case right_rst {
    Ok(Some(right)) ->
      // parse successive pattern exhaustively
      #(Ok(Some(constructor(left, op, right))), right_parser)
      |> parse_successive_binary(
        match: operators,
        constructor:,
        with: parse_func,
      )

    Ok(None) -> #(Error(ParseError(ExpectRightValue, op.line)), right_parser)
    Error(_) -> right_pair
  }
}

fn unless_is_successive_bianry(parser, rst, operators, return, callback) {
  use <- bool.guard(!is_successive(parser, rst, operators), return)
  // Safe unwrap after guarantee
  let assert Ok(Some(left)) = rst
  let assert Some(op) = parser.tok0

  callback(left, op)
}

/// is_successive guarantee the left operand is valid parse_expression as well as
/// the current token matches the given operators.
fn is_successive(
  parser: Parser,
  rst: Result(Option(Expr)),
  operators: List(TokenType),
) -> Bool {
  // Guarantee left operand is a valid non-empty expression
  let valid_left_operand = case rst {
    Ok(Some(_)) -> True
    _ -> False
  }

  let match_operator = case parser.tok0 {
    Some(Token(t, _)) -> list.contains(operators, t)
    None -> False
  }

  valid_left_operand && match_operator
}

fn unary(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  case parser.tok0 {
    Some(Token(token.Bang, _) as op) -> {
      let #(rst, right_parser) as pair = unary(advance(parser))
      case rst {
        Ok(Some(right)) -> {
          let new_exp = NegativeBool(token: op, value: right)
          #(Ok(Some(new_exp)), right_parser)
        }
        Ok(None) -> #(
          Error(ParseError(ExpectRightValue, op.line)),
          right_parser,
        )
        Error(_) -> pair
      }
    }
    Some(Token(token.Minus, _) as op) -> {
      let #(rst, right_parser) as pair = unary(advance(parser))
      case rst {
        Ok(Some(right)) -> {
          let new_exp = NegativeNumber(token: op, value: right)
          #(Ok(Some(new_exp)), right_parser)
        }
        Ok(None) -> #(
          Error(ParseError(ExpectRightValue, op.line)),
          right_parser,
        )
        Error(_) -> pair
      }
    }

    _ -> primary(parser)
  }
}

fn primary(parser: Parser) -> #(Result(Option(Expr)), Parser) {
  case parser.tok0 {
    // end parsing if encounter semicolon, don't consume semicolon here, it should
    // be consumed at statement level.
    Some(Token(token.Semicolon, _)) -> #(Ok(None), parser)

    // Literal
    Some(Token(token.Number(n), _)) -> #(Ok(Some(Number(n))), advance(parser))
    Some(Token(token.String(s), _)) -> #(Ok(Some(String(s))), advance(parser))
    Some(Token(token.True, _)) -> #(Ok(Some(Boolean(True))), advance(parser))
    Some(Token(token.False, _)) -> #(Ok(Some(Boolean(False))), advance(parser))
    Some(Token(token.NilLiteral, _)) -> #(Ok(Some(NilLiteral)), advance(parser))

    // Variable
    Some(Token(token.Identifier(_), _) as tok) -> #(
      Ok(Some(Variable(tok))),
      advance(parser),
    )

    // Grouping
    Some(Token(token.LeftParen, line)) ->
      case parser.tok1 {
        // Empty grouping
        Some(Token(token.RightParen, _)) -> #(
          Ok(Some(Grouping(None))),
          advance(advance(parser)),
        )

        // Non-empty grouping
        _ -> {
          let #(inner_expr, sub_parser) = expression(advance(parser))
          case inner_expr, sub_parser.tok0 {
            Ok(e), Some(Token(token.RightParen, _)) -> #(
              Ok(Some(Grouping(e))),
              advance(sub_parser),
            )
            Ok(_), _ -> #(
              Error(ParseError(ExpectRightParentheses, line)),
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

fn match_identifier(
  parser: Parser,
  otherwise return: b,
  with callback: fn(Token, Parser) -> b,
) -> b {
  match(
    parser,
    type_of: token.Identifier(""),
    otherwise: return,
    with: callback,
  )
}

// Using this function to consume specific token and advance as possible.
// NOTE: Carefully, avoid redundant token consume.
fn match(
  parser: Parser,
  type_of token_type: TokenType,
  otherwise return: b,
  with fun: fn(Token, Parser) -> b,
) -> b {
  use <- bool.guard(!is_token_type(parser.tok0, token_type), return)
  let assert Some(tok) = parser.tok0
  let parser = advance(parser)

  fun(tok, parser)
}

fn is_token_type(
  maybe_expected: Option(Token),
  of token_type: TokenType,
) -> Bool {
  case maybe_expected, token_type {
    Some(Token(token.Number(_), _)), token.Number(_) -> True
    Some(Token(token.String(_), _)), token.String(_) -> True
    Some(Token(token.Identifier(_), _)), token.Identifier(_) -> True
    Some(Token(type_, _)), _ if type_ == token_type -> True
    _, _ -> False
  }
}
