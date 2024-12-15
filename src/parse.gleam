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
////                | for_stmt (syntax sugar)
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
////    unary -> ( "!" | "-" ) unary | call ;
////    call -> primary ( "(" arguments? ")" )* ;
////    arguments -> expression ( "," expression )* ;
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

import parse/error.{
  type LexicalError, type ParseError, BreakNotInLoop, ExpectCallable,
  ExpectComma, ExpectExpression, ExpectLeftParentheses, ExpectLeftValue,
  ExpectRightParentheses, ExpectRightValue, ExpectSemicolon, ExpectStatement,
  ExpectVariableName, ExtraneousParentheses, InvalidAssignmentTarget, LexError,
  LexicalError, ParseError, TooManyArguments,
}
import parse/expr.{
  type Expr, Assign, Binary, Boolean, Grouping, LogicAnd, LogicOr, NegativeBool,
  NegativeNumber, NilLiteral, Number, String, Variable,
}
import parse/lexer.{type LexResult}
import parse/stmt.{
  type Stmt, Block, Break, Declaration, EmptyExpression, Expression, If, Print,
  While,
}
import parse/token.{type Token, type TokenType, Token}
import prelude.{ensure_exist, loop, with_ok}

pub type Parser {
  Parser(
    tokens: Iterator(LexResult),
    lex_errors: List(LexicalError),
    tok0: Option(Token),
    tok1: Option(Token),
    loop_depth: Int,
    line: Int,
  )
}

// the result of a parsing stage may also Expr and Stmt
pub type Result(t) =
  gleam.Result(t, ParseError)

type ExprPair =
  #(Result(Option(Expr)), Parser)

type StmtPair =
  #(Result(Option(Stmt)), Parser)

pub fn new(tokens: Iterator(LexResult)) -> Parser {
  Parser(
    tokens:,
    lex_errors: [],
    tok0: None,
    tok1: None,
    loop_depth: 0,
    line: 1,
  )
  |> advance
  |> advance
}

pub fn from_source(source: String) -> Parser {
  lexer.new(source)
  |> new
}

fn advance(parser: Parser) -> Parser {
  case iterator.step(parser.tokens) {
    iterator.Next(Ok(token), rest) -> {
      let parser = case token.line > parser.line {
        True -> Parser(..parser, line: token.line)
        False -> parser
      }
      Parser(..parser, tokens: rest, tok0: parser.tok1, tok1: Some(token))
    }
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

fn begin_loop(parser: Parser) -> Parser {
  Parser(..parser, loop_depth: parser.loop_depth + 1)
}

fn end_loop(parser: Parser) -> Parser {
  case parser {
    Parser(loop_depth:, ..) if loop_depth > 0 ->
      Parser(..parser, loop_depth: loop_depth - 1)
    _ -> panic as "Call end loop with no loop depth"
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
pub fn declaration(parser: Parser) -> StmtPair {
  let #(rst, parser) = case parser.tok0 {
    Some(Token(token.Var, _)) -> var_declaration(parser)
    _ -> statement(parser)
  }
  case rst, parser {
    Error(_), parser -> #(rst, synchronize(parser))
    Ok(_), _ -> #(rst, parser)
  }
}

fn var_declaration(parser: Parser) -> StmtPair {
  let #(rst, parser) = var_declaration_inner(parser)
  case rst {
    Ok(s) -> #(Ok(Some(s)), parser)
    Error(err) -> #(Error(err), parser)
  }
}

/// var_declaration_inner parse a result of statement, because a `None` variable
/// declaration doesn't make sense.
fn var_declaration_inner(parser: Parser) -> #(Result(Stmt), Parser) {
  let assert Some(Token(token.Var, _)) = parser.tok0
  let parser = advance(parser)

  use var_name, parser <- match_identifier(
    parser,
    Error(ParseError(ExpectVariableName, parser.line)),
  )

  case parser.tok0 {
    // variable declaration with initializer
    Some(Token(token.Equal, _)) -> {
      let #(rst, parser) = expression(advance(parser))

      use maybe_init <- with_ok(rst, parser)

      use _ <- ensure_exist(
        in: maybe_init,
        otherwise: Error(ParseError(ExpectRightValue, parser.line)),
        processer: parser,
      )

      // Don't need to unwrap maybe_init here, because the constructor receive a option.
      use _, parser <- match(
        parser:,
        type_of: token.Semicolon,
        otherwise: Error(ParseError(ExpectSemicolon, parser.line)),
      )

      #(Ok(Declaration(name: var_name, initializer: maybe_init)), parser)
    }

    // variable declaration without initialization
    Some(Token(token.Semicolon, _)) -> #(
      Ok(Declaration(var_name, None)),
      advance(parser),
    )

    _ -> #(Error(ParseError(ExpectSemicolon, parser.line)), parser)
  }
}

fn statement(parser: Parser) -> StmtPair {
  case parser.tok0 {
    Some(Token(token.If, _)) -> if_stmt(advance(parser))
    Some(Token(token.For, _)) -> parser |> advance |> begin_loop |> for_stmt
    Some(Token(token.While, _)) -> parser |> advance |> begin_loop |> while_stmt
    Some(Token(token.Print, _)) -> print_stmt(advance(parser))
    Some(Token(token.LeftBrace, _)) -> block(advance(parser))
    Some(Token(token.Break, _)) -> break_stmt(advance(parser))
    _ -> expr_stmt(parser)
  }
}

fn break_stmt(parser: Parser) -> StmtPair {
  case parser.tok0, parser.loop_depth {
    Some(Token(token.Semicolon, _)), n if n > 0 -> #(
      Ok(Some(Break)),
      advance(parser),
    )
    Some(Token(token.Semicolon, _)), _ -> #(
      Error(ParseError(BreakNotInLoop, parser.line)),
      parser,
    )
    _, _ -> #(Error(ParseError(ExpectSemicolon, parser.line)), parser)
  }
}

/// if_stmt eagerly looks for the "else"" token, thus the else branch will be
/// bound to the nearest if statement.
fn if_stmt(parser: Parser) -> StmtPair {
  let #(rst, parser) = if_stmt_inner(parser)
  use rst <- with_ok(in: rst, processer: parser)

  #(Ok(Some(rst)), parser)
}

fn if_stmt_inner(parser) -> #(Result(Stmt), Parser) {
  use condition, parser <- ensure_condition_valid(parser)

  // parse branches
  let #(then_branch_rst, parser) = statement(parser)
  use maybe_then <- with_ok(then_branch_rst, parser)

  use then_branch <- ensure_exist(
    in: maybe_then,
    otherwise: Error(ParseError(ExpectStatement, parser.line)),
    processer: parser,
  )

  use _else_keyword, parser <- match(
    parser:,
    type_of: token.Else,
    otherwise: Ok(If(condition:, then_branch:, else_branch: None)),
  )
  let #(else_branch_rst, parser) = statement(parser)
  use maybe_else <- with_ok(else_branch_rst, parser)
  case maybe_else {
    None -> #(Error(ParseError(ExpectStatement, parser.line)), parser)
    else_branch -> #(Ok(If(condition:, then_branch:, else_branch:)), parser)
  }
}

fn for_stmt(parser: Parser) -> StmtPair {
  let #(rst, parser) = {
    use _, parser <- match(
      parser:,
      type_of: token.LeftParen,
      otherwise: Error(ParseError(ExpectLeftParentheses, parser.line)),
    )

    // parse initializer statement
    let #(init_rst, parser) = case parser.tok0 {
      Some(Token(token.Semicolon, _)) -> #(
        Ok(Some(EmptyExpression)),
        advance(parser),
      )
      Some(Token(token.Var, _)) -> var_declaration(parser)
      _ -> expr_stmt(parser)
    }
    use maybe_init <- with_ok(in: init_rst, processer: parser)
    use initializer <- ensure_exist(
      in: maybe_init,
      otherwise: Error(ParseError(ExpectStatement, parser.line)),
      processer: parser,
    )

    // parse condition expression then consume semicolon
    let #(cond_rst, parser) = expression(parser)
    use maybe_cond <- with_ok(in: cond_rst, processer: parser)
    let cond = option.unwrap(maybe_cond, Boolean(True))
    use _, parser <- match(
      parser:,
      type_of: token.Semicolon,
      otherwise: Error(ParseError(ExpectSemicolon, parser.line)),
    )

    // parse increment expression then consume right parentheses
    let #(inc_rst, parser) = case parser.tok0 {
      Some(Token(token.RightParen, _)) -> #(Ok(None), parser)
      _ -> expression(parser)
    }
    use maybe_inc <- with_ok(in: inc_rst, processer: parser)
    let increment =
      maybe_inc |> option.map(Expression) |> option.unwrap(EmptyExpression)
    use _rp, parser <- match(
      parser:,
      type_of: token.RightParen,
      otherwise: Error(ParseError(ExpectRightParentheses, parser.line)),
    )

    let #(body_rst, parser) = statement(parser)
    use maybe_body <- with_ok(in: body_rst, processer: parser)
    let body_with_inc =
      option.values([maybe_body]) |> list.append([increment]) |> Block

    let loop = While(condition: cond, body: body_with_inc)
    let for_loop_with_init = Block([initializer, loop])

    #(Ok(Some(for_loop_with_init)), parser)
  }
  #(rst, end_loop(parser))
}

fn while_stmt(parser: Parser) -> StmtPair {
  let #(rst, parser) = {
    use condition, parser <- ensure_condition_valid(parser)

    let #(body_rst, parser) = statement(parser)
    use maybe_body <- with_ok(in: body_rst, processer: parser)

    case maybe_body {
      Some(body) -> #(Ok(Some(While(condition:, body:))), parser)
      None -> #(Error(ParseError(ExpectStatement, parser.line)), parser)
    }
  }
  #(rst, end_loop(parser))
}

fn ensure_condition_valid(
  parser,
  with fun: fn(Expr, Parser) -> #(Result(any), Parser),
) -> #(Result(any), Parser) {
  let #(cond_rst, parser) = parse_condition(parser)
  use condition <- with_ok(in: cond_rst, processer: parser)
  fun(condition, parser)
}

fn parse_condition(parser: Parser) -> #(gleam.Result(Expr, ParseError), Parser) {
  use _, parser <- match(
    parser:,
    type_of: token.LeftParen,
    otherwise: Error(ParseError(ExpectLeftParentheses, parser.line)),
  )

  // ensure parse Ok and condition is not none.
  let #(cond_rst, parser) = expression(parser)
  use maybe_cond <- with_ok(cond_rst, parser)
  use cond <- ensure_exist(
    in: maybe_cond,
    otherwise: Error(ParseError(ExpectExpression, parser.line)),
    processer: parser,
  )

  use _, parser <- match(
    parser:,
    type_of: token.RightParen,
    otherwise: Error(ParseError(ExpectRightParentheses, parser.line)),
  )

  #(Ok(cond), parser)
}

fn block(parser: Parser) -> StmtPair {
  let #(rst, parser) = block_inner(parser, [])
  use statements <- with_ok(rst, parser)

  #(Ok(Some(Block(statements))), advance(parser))
}

fn block_inner(
  parser: Parser,
  acc acc: List(Stmt),
) -> #(Result(List(Stmt)), Parser) {
  case parser.tok0 {
    None -> #(Error(ParseError(error.ExpectRightBrace, parser.line)), parser)
    Some(Token(token.RightBrace, _)) -> #(Ok(acc), parser)
    Some(_) -> {
      let #(rst, parser) = declaration(parser)
      case rst {
        Error(err) -> #(Error(err), parser)
        Ok(None) -> block_inner(parser, acc)
        Ok(Some(s)) -> block_inner(parser, list.append(acc, [s]))
      }
    }
  }
}

fn print_stmt(parser: Parser) -> StmtPair {
  let #(rst, parser) = print_stmt_inner(parser)
  case rst {
    Ok(s) -> #(Ok(Some(s)), parser)
    Error(err) -> #(Error(err), parser)
  }
}

fn print_stmt_inner(parser: Parser) -> #(Result(Stmt), Parser) {
  let #(rst, parser) = expression(parser)

  use maybe_expr <- with_ok(rst, parser)
  case parser.tok0, maybe_expr {
    Some(Token(token.Semicolon, _)), Some(expr) -> #(
      Ok(Print(expr)),
      advance(parser),
    )
    Some(Token(token.Semicolon, _)), None -> #(
      Error(ParseError(ExpectExpression, parser.line)),
      advance(parser),
    )

    _, _ -> #(Error(ParseError(ExpectSemicolon, parser.line)), parser)
  }
}

pub fn expression(parser: Parser) -> ExprPair {
  let #(rst, parser) = assignment(parser)
  let rst = case parser.lex_errors {
    [] -> rst
    [err, ..] -> Error(ParseError(LexError(err), err.line))
  }

  #(rst, parser)
}

fn expr_stmt(parser: Parser) -> StmtPair {
  let #(expr_rst, parser) = expression(parser)

  use maybe_exp <- with_ok(expr_rst, parser)
  case parser.tok0, maybe_exp {
    Some(Token(token.Semicolon, _)), Some(expr) -> #(
      Ok(Some(Expression(expr))),
      advance(parser),
    )
    Some(Token(token.Semicolon, _)), None -> #(
      Ok(Some(EmptyExpression)),
      advance(parser),
    )

    Some(t), _ -> #(Error(ParseError(ExpectSemicolon, t.line)), parser)

    // Valid none expression, representing a single of EOF.
    None, Some(_) -> #(Error(ParseError(ExpectSemicolon, parser.line)), parser)
    None, None -> #(Ok(None), parser)
  }
}

fn assignment(parser: Parser) -> ExprPair {
  let #(rst, parser) = logic_or(parser)
  use maybe_expr <- with_ok(in: rst, processer: parser)

  // Successive double equal has consumed by the previous procedure, so here
  // should be the single equal.
  use _, parser <- match(parser, token.Equal, rst)
  use target <- ensure_exist(
    in: maybe_expr,
    otherwise: Error(ParseError(ExpectLeftValue, parser.line)),
    processer: parser,
  )

  let #(value_rst, parser) = assignment(parser)
  use maybe_value <- with_ok(in: value_rst, processer: parser)
  use value <- ensure_exist(
    in: maybe_value,
    otherwise: Error(ParseError(ExpectRightValue, parser.line)),
    processer: parser,
  )

  let rst = case target {
    Variable(name) -> Ok(Some(Assign(name, value)))
    _ -> Error(ParseError(InvalidAssignmentTarget, parser.line))
  }
  #(rst, parser)
}

fn logic_or(parser: Parser) -> ExprPair {
  logic_and(parser)
  |> parse_successive_binary(
    match: [token.Or],
    constructor: LogicOr,
    with: logic_and,
  )
}

fn logic_and(parser: Parser) -> ExprPair {
  equality(parser)
  |> parse_successive_binary(
    match: [token.And],
    constructor: LogicAnd,
    with: equality,
  )
}

fn equality(parser: Parser) -> ExprPair {
  comparison(parser)
  |> parse_successive_binary(
    match: [token.NotEqual, token.EqualEqual],
    constructor: Binary,
    with: comparison,
  )
}

fn comparison(parser: Parser) -> ExprPair {
  term(parser)
  |> parse_successive_binary(
    match: [token.Greater, token.GreaterEqual, token.Less, token.LessEqual],
    constructor: Binary,
    with: term,
  )
}

fn term(parser: Parser) -> ExprPair {
  factor(parser)
  |> parse_successive_binary(
    match: [token.Minus, token.Plus],
    constructor: Binary,
    with: factor,
  )
}

fn factor(parser: Parser) -> ExprPair {
  unary(parser)
  |> parse_successive_binary(
    match: [token.Slash, token.Star],
    constructor: Binary,
    with: unary,
  )
}

fn parse_successive_binary(
  pair: ExprPair,
  match operators: List(TokenType),
  constructor constructor: fn(Expr, Token, Expr) -> Expr,
  with parse_func: fn(Parser) -> ExprPair,
) -> ExprPair {
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

    Ok(None) -> #(
      Error(ParseError(ExpectRightValue, parser.line)),
      right_parser,
    )
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

fn unary(parser: Parser) -> ExprPair {
  case parser.tok0 {
    Some(Token(token.Bang, _) as op) -> {
      let #(rst, right_parser) as pair = unary(advance(parser))
      case rst {
        Ok(Some(right)) -> {
          let new_exp = NegativeBool(token: op, value: right)
          #(Ok(Some(new_exp)), right_parser)
        }
        Ok(None) -> #(
          Error(ParseError(ExpectRightValue, parser.line)),
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
          Error(ParseError(ExpectRightValue, parser.line)),
          right_parser,
        )
        Error(_) -> pair
      }
    }

    _ -> call(parser)
  }
}

fn call(parser: Parser) -> ExprPair {
  let init = primary(parser)

  loop(
    when: fn(init: ExprPair) {
      let #(_, current_parser) = init
      is_token_type(current_parser.tok0, of: token.LeftParen)
    },
    init:,
    do: fn(init: ExprPair) {
      // In the body, expression must be callee rather than plain object.
      let #(callee_or_primary_rst, parser) = init
      use _, parser <- match(
        parser:,
        type_of: token.LeftParen,
        otherwise: Error(ParseError(ExpectLeftParentheses, parser.line)),
      )
      use maybe_callee <- with_ok(in: callee_or_primary_rst, processer: parser)
      use callee <- ensure_exist(
        in: maybe_callee,
        otherwise: Error(ParseError(ExpectCallable, parser.line)),
        processer: parser,
      )
      finish_call(parser, callee)
    },
  )
}

fn finish_call(parser: Parser, callee: Expr) -> ExprPair {
  let #(args_rst, parser) = consume_arguments(parser, [])
  use arguments <- with_ok(in: args_rst, processer: parser)
  use paren, parser <- match(
    parser:,
    type_of: token.RightParen,
    otherwise: Error(ParseError(ExpectRightParentheses, parser.line)),
  )
  let call = expr.Call(callee:, paren:, arguments:)
  #(Ok(Some(call)), parser)
}

fn consume_arguments(
  parser: Parser,
  arguments: List(Expr),
) -> #(Result(List(Expr)), Parser) {
  case is_token_type(parser.tok0, token.RightParen) {
    True -> #(Ok(arguments), parser)
    False ->
      case list.length(arguments) {
        n if n >= 255 -> #(
          Error(ParseError(TooManyArguments, parser.line)),
          parser,
        )
        _ -> {
          let #(rst, parser) = expression(parser)
          use maybe_expr <- with_ok(in: rst, processer: parser)
          use _, parser <- match(
            parser:,
            type_of: token.Comma,
            otherwise: Error(ParseError(ExpectComma, parser.line)),
          )

          maybe_expr
          |> option.map(fn(e) { list.append(arguments, [e]) })
          |> option.unwrap(arguments)
          |> consume_arguments(parser, _)
        }
      }
  }
}

fn primary(parser: Parser) -> ExprPair {
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
    Some(Token(token.LeftParen, _)) ->
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
              Error(ParseError(ExpectRightParentheses, parser.line)),
              sub_parser,
            )
            Error(_), _ -> #(inner_expr, sub_parser)
          }
        }
      }

    Some(Token(token.RightParen, _)) -> #(
      Error(ParseError(ExtraneousParentheses, parser.line)),
      parser,
    )

    Some(Token(token.Semicolon, _)) | None -> #(Ok(None), parser)
    _ -> #(Ok(None), parser)
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
  with fun: fn(Token, Parser) -> #(b, Parser),
) -> #(b, Parser) {
  match(parser:, type_of: token.Identifier(""), otherwise: return, with: fun)
}

// Using this function to consume specific token and advance as possible.
// NOTE: Carefully, avoid redundant token consume.
fn match(
  parser parser: Parser,
  type_of token_type: TokenType,
  otherwise return: b,
  with fun: fn(Token, Parser) -> #(b, Parser),
) -> #(b, Parser) {
  use <- bool.guard(!is_token_type(parser.tok0, token_type), #(return, parser))
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
