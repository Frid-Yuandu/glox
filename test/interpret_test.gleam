import birdie
import gleam/option.{None, Some}
import pprint

import interpreter.{type Interpreter, Interpreter}
import interpreter/environment
import interpreter/io_controller
import parse/expr
import parse/stmt
import parse/token.{Token}

pub fn should_interpret_literal_test() {
  let given_stmt = [
    stmt.Expression(expr.String("test\tstring")),
    stmt.Expression(expr.Boolean(False)),
    stmt.Expression(expr.Number(1.0)),
    stmt.Expression(expr.NilLiteral),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot(title: "literal expression statements")
}

// test unary expressions

pub fn should_interpret_apply_bang_on_false_as_true_test() {
  let given_stmt = [
    stmt.Expression(expr.NegativeBool(
      token: Token(token.Bang, 1),
      value: expr.Boolean(False),
    )),
    stmt.Expression(expr.NegativeBool(
      token: Token(token.Bang, 1),
      value: expr.NilLiteral,
    )),
    stmt.Expression(expr.NegativeBool(
      token: Token(token.Bang, 1),
      value: expr.Number(23_333.0),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("unary expression statements")
}

// test binary expressions

pub fn should_interpret_binary_calculations_test() {
  let given_stmt = [
    stmt.Expression(expr.Binary(
      expr.Number(3.0),
      Token(token.Plus, 1),
      expr.Number(4.0),
    )),
    stmt.Expression(expr.Binary(
      expr.String("foo"),
      Token(token.Plus, 1),
      expr.String("bar"),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(7.0),
      Token(token.Minus, 1),
      expr.Number(3.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(3.0),
      Token(token.Star, 1),
      expr.Number(2.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(10.0),
      Token(token.Slash, 1),
      expr.Number(2.0),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("binary calculations expression statements")
}

pub fn should_interpret_equal_equal_test() {
  let given_stmt = [
    stmt.Expression(expr.Binary(
      expr.Number(10.0),
      Token(token.Greater, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.GreaterEqual, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(2.0),
      Token(token.Less, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.LessEqual, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.NotEqual, 1),
      expr.Number(3.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.EqualEqual, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(10.0),
      Token(token.Greater, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.GreaterEqual, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(2.0),
      Token(token.Less, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.LessEqual, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.NotEqual, 1),
      expr.Number(3.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.EqualEqual, 1),
      expr.Number(5.0),
    )),
    stmt.Expression(expr.Binary(
      expr.Number(5.0),
      Token(token.EqualEqual, 1),
      expr.Number(5.0),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("equalities expression statements")
}

// error cases in binary expressions

pub fn should_error_on_add_number_and_non_number_types_test() {
  let given_stmt = [
    stmt.Expression(expr.Binary(
      expr.Number(1.0),
      Token(token.Plus, 1),
      expr.Boolean(True),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("error in add number with non-number")
}

pub fn should_error_in_addition_non_number_or_string_types_test() {
  let given_stmt = [
    stmt.Expression(expr.Binary(
      expr.NilLiteral,
      Token(token.Plus, 1),
      expr.Boolean(False),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("error in add not number or string type")
}

pub fn should_error_on_addition_string_and_non_string_test() {
  let given_stmt = [
    stmt.Expression(expr.Binary(
      expr.String("foo"),
      Token(token.Plus, 1),
      expr.Number(1.0),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("error in add wrong type to string")
}

pub fn should_error_on_subtraction_non_numbers_test() {
  let given_stmt = [
    stmt.Expression(expr.Binary(
      expr.String("foo"),
      Token(token.Minus, 1),
      expr.Number(1.0),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("error in subtract wrong type to string")
}

pub fn should_error_in_division_by_zero_test() {
  let given_stmt = [
    stmt.Expression(expr.Binary(
      expr.Number(10.0),
      Token(token.Slash, 1),
      expr.Number(0.0),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("error in divide by zero")
}

// test grouping expressions

pub fn should_interpret_grouping_expression_test() {
  let given_stmt = [
    stmt.Expression(
      expr.Grouping(
        Some(expr.Binary(
          expr.Number(3.0),
          Token(token.Plus, 1),
          expr.Number(4.0),
        )),
      ),
    ),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("non-empty gourping expression")
}

pub fn should_interpret_empty_grouping_test() {
  let given_stmt = [stmt.Expression(expr.Grouping(None))]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("empty grouping expression")
}

// test if statement

pub fn should_interpret_if_statement_with_then_branch_test() {
  let given_stmt = [
    stmt.If(
      condition: expr.Boolean(True),
      then_branch: stmt.Print(expr.String("then")),
      else_branch: None,
    ),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("if statement with then branch only")
}

pub fn should_interpret_if_statement_with_else_branch_test() {
  let given_stmt = [
    stmt.If(
      condition: expr.Boolean(False),
      then_branch: stmt.Print(expr.String("then")),
      else_branch: Some(stmt.Print(expr.String("else"))),
    ),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("if statement with else branch")
}

pub fn should_interpret_nested_if_statements_test() {
  let given_stmt = [
    stmt.If(
      condition: expr.Boolean(True),
      then_branch: stmt.If(
        condition: expr.Boolean(True),
        then_branch: stmt.Print(expr.String("nested then")),
        else_branch: None,
      ),
      else_branch: None,
    ),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("nested if statements")
}

pub fn should_interpret_if_with_block_statement_test() {
  let given_stmt = [
    stmt.If(
      condition: expr.Boolean(True),
      then_branch: stmt.Block([
        stmt.Print(expr.String("first")),
        stmt.Print(expr.String("second")),
      ]),
      else_branch: None,
    ),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("if statement with block")
}

pub fn should_interpret_if_with_expression_condition_test() {
  let given_stmt = [
    stmt.If(
      condition: expr.Binary(
        expr.Number(5.0),
        Token(token.Greater, 1),
        expr.Number(3.0),
      ),
      then_branch: stmt.Print(expr.String("condition true")),
      else_branch: Some(stmt.Print(expr.String("condition false"))),
    ),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("if statement with expression condition")
}

pub fn should_interpret_if_with_truthy_values_test() {
  let given_stmt = [
    // Numbers are truthy
    stmt.If(
      condition: expr.Number(1.0),
      then_branch: stmt.Print(expr.String("number is truthy")),
      else_branch: None,
    ),
    // Strings are truthy
    stmt.If(
      condition: expr.String("hello"),
      then_branch: stmt.Print(expr.String("string is truthy")),
      else_branch: None,
    ),
    // nil is falsy
    stmt.If(
      condition: expr.NilLiteral,
      then_branch: stmt.Print(expr.String("won't print")),
      else_branch: Some(stmt.Print(expr.String("nil is falsy"))),
    ),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("if statement with different truthy values")
}

// test logic operation expressions

pub fn should_interpret_true_and_true_test() {
  let given_stmt = [
    stmt.Expression(expr.LogicAnd(
      expr.Boolean(True),
      Token(token.And, 1),
      expr.Boolean(True),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("true AND true")
}

pub fn should_interpret_true_and_false_test() {
  let given_stmt = [
    stmt.Expression(expr.LogicAnd(
      expr.Boolean(True),
      Token(token.And, 1),
      expr.Boolean(False),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("true AND false")
}

pub fn should_short_circuit_false_and_true_test() {
  let given_stmt = [
    stmt.Expression(expr.LogicAnd(
      expr.Boolean(False),
      Token(token.And, 1),
      expr.Boolean(True),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("false AND true (short-circuit)")
}

pub fn should_interpret_chained_and_operations_test() {
  let given_stmt = [
    stmt.Expression(expr.LogicAnd(
      expr.LogicAnd(expr.Boolean(True), Token(token.And, 1), expr.Boolean(True)),
      Token(token.And, 1),
      expr.Boolean(True),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("chained AND operations")
}

pub fn should_interpret_and_with_truthy_values_test() {
  let given_stmt = [
    stmt.Expression(expr.LogicAnd(
      expr.Number(1.0),
      Token(token.And, 1),
      expr.String("hello"),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("AND with truthy values")
}

// Logic OR Tests
pub fn should_short_circuit_true_or_true_test() {
  let given_stmt = [
    stmt.Expression(expr.LogicOr(
      expr.Boolean(True),
      Token(token.Or, 1),
      expr.Boolean(True),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("true OR true (short-circuit)")
}

pub fn should_short_circuit_true_or_false_test() {
  let given_stmt = [
    stmt.Expression(expr.LogicOr(
      expr.Boolean(True),
      Token(token.Or, 1),
      expr.Boolean(False),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("true OR false (short-circuit)")
}

pub fn should_interpret_false_or_true_test() {
  let given_stmt = [
    stmt.Expression(expr.LogicOr(
      expr.Boolean(False),
      Token(token.Or, 1),
      expr.Boolean(True),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("false OR true")
}

pub fn should_interpret_false_or_false_test() {
  let given_stmt = [
    stmt.Expression(expr.LogicOr(
      expr.Boolean(False),
      Token(token.Or, 1),
      expr.Boolean(False),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("false OR false")
}

pub fn should_interpret_chained_or_operations_test() {
  let given_stmt = [
    stmt.Expression(expr.LogicOr(
      expr.LogicOr(expr.Boolean(False), Token(token.Or, 1), expr.Boolean(False)),
      Token(token.Or, 1),
      expr.Boolean(True),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("chained OR operations")
}

// Mixed Logic Tests
pub fn should_interpret_and_or_combination_test() {
  let given_stmt = [
    stmt.Expression(expr.LogicOr(
      expr.LogicAnd(expr.Boolean(True), Token(token.And, 1), expr.Boolean(True)),
      Token(token.Or, 1),
      expr.Boolean(False),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("AND-OR combination")
}

pub fn should_interpret_or_and_combination_test() {
  let given_stmt = [
    stmt.Expression(expr.LogicAnd(
      expr.LogicOr(expr.Boolean(False), Token(token.Or, 1), expr.Boolean(True)),
      Token(token.And, 1),
      expr.Boolean(True),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("OR-AND combination")
}

pub fn should_interpret_nil_in_logic_operations_test() {
  let given_stmt = [
    stmt.Expression(expr.LogicAnd(
      expr.NilLiteral,
      Token(token.And, 1),
      expr.Boolean(True),
    )),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("nil in logical operations")
}

// test while statements

pub fn should_interpret_basic_while_loop_test() {
  let given_stmt = [
    // var i = 0; while (i < 3) { print i; i = i + 1; }
    stmt.Declaration(Token(token.Identifier("i"), 1), Some(expr.Number(0.0))),
    stmt.While(
      condition: expr.Binary(
        expr.Variable(Token(token.Identifier("i"), 1)),
        Token(token.Less, 1),
        expr.Number(3.0),
      ),
      body: stmt.Block([
        stmt.Print(expr.Variable(Token(token.Identifier("i"), 1))),
        stmt.Expression(expr.Assign(
          Token(token.Identifier("i"), 1),
          expr.Binary(
            expr.Variable(Token(token.Identifier("i"), 1)),
            Token(token.Plus, 1),
            expr.Number(1.0),
          ),
        )),
      ]),
    ),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("basic while loop")
}

pub fn should_interpret_while_with_false_condition_test() {
  let given_stmt = [
    stmt.While(
      condition: expr.Boolean(False),
      body: stmt.Print(expr.String("should not print")),
    ),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("while with false condition")
}

pub fn should_interpret_nested_while_loops_test() {
  let given_stmt = [
    // var i = 0; while (i < 2) { var j = 0; while (j < 2) { print i + j; j = j + 1; } i = i + 1; }
    stmt.Declaration(Token(token.Identifier("i"), 1), Some(expr.Number(0.0))),
    stmt.While(
      condition: expr.Binary(
        expr.Variable(Token(token.Identifier("i"), 1)),
        Token(token.Less, 1),
        expr.Number(2.0),
      ),
      body: stmt.Block([
        stmt.Declaration(
          Token(token.Identifier("j"), 1),
          Some(expr.Number(0.0)),
        ),
        stmt.While(
          condition: expr.Binary(
            expr.Variable(Token(token.Identifier("j"), 1)),
            Token(token.Less, 1),
            expr.Number(2.0),
          ),
          body: stmt.Block([
            stmt.Print(expr.Binary(
              expr.Variable(Token(token.Identifier("i"), 1)),
              Token(token.Plus, 1),
              expr.Variable(Token(token.Identifier("j"), 1)),
            )),
            stmt.Expression(expr.Assign(
              Token(token.Identifier("j"), 1),
              expr.Binary(
                expr.Variable(Token(token.Identifier("j"), 1)),
                Token(token.Plus, 1),
                expr.Number(1.0),
              ),
            )),
          ]),
        ),
        stmt.Expression(expr.Assign(
          Token(token.Identifier("i"), 1),
          expr.Binary(
            expr.Variable(Token(token.Identifier("i"), 1)),
            Token(token.Plus, 1),
            expr.Number(1.0),
          ),
        )),
      ]),
    ),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("nested while loops")
}

// TODO: this case is not propriate, because no break statement is implemented.
// pub fn should_interpret_while_with_break_condition_test() {
//   let given_stmt = [
//     // var x = 0; while (true) { print x; x = x + 1; if (x > 2) break; }
//     stmt.Declaration(Token(token.Identifier("x"), 1), Some(expr.Number(0.0))),
//     stmt.While(
//       condition: expr.Boolean(True),
//       body: stmt.Block([
//         stmt.Print(expr.Variable(Token(token.Identifier("x"), 1))),
//         stmt.Expression(expr.Assign(
//           Token(token.Identifier("x"), 1),
//           expr.Binary(
//             expr.Variable(Token(token.Identifier("x"), 1)),
//             Token(token.Plus, 1),
//             expr.Number(1.0),
//           ),
//         )),
//         stmt.If(
//           condition: expr.Binary(
//             expr.Variable(Token(token.Identifier("x"), 1)),
//             Token(token.Greater, 1),
//             expr.Number(2.0),
//           ),
//           then_branch: stmt.Expression(expr.Boolean(False)),
//           else_branch: None,
//         ),
//       ]),
//     ),
//   ]
//   snapshot( "while with break condition")
// }

pub fn should_interpret_while_with_logical_condition_test() {
  let given_stmt = [
    // var x = 0; while (x < 2 and x >= 0) { print x; x = x + 1; }
    stmt.Declaration(Token(token.Identifier("x"), 1), Some(expr.Number(0.0))),
    stmt.While(
      condition: expr.LogicAnd(
        expr.Binary(
          expr.Variable(Token(token.Identifier("x"), 1)),
          Token(token.Less, 1),
          expr.Number(2.0),
        ),
        Token(token.And, 1),
        expr.Binary(
          expr.Variable(Token(token.Identifier("x"), 1)),
          Token(token.GreaterEqual, 1),
          expr.Number(0.0),
        ),
      ),
      body: stmt.Block([
        stmt.Print(expr.Variable(Token(token.Identifier("x"), 1))),
        stmt.Expression(expr.Assign(
          Token(token.Identifier("x"), 1),
          expr.Binary(
            expr.Variable(Token(token.Identifier("x"), 1)),
            Token(token.Plus, 1),
            expr.Number(1.0),
          ),
        )),
      ]),
    ),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("while with logical condition")
}

pub fn should_interpret_while_with_variable_modification_test() {
  let given_stmt = [
    // var sum = 0; var n = 3; while (n > 0) { sum = sum + n; n = n - 1; }
    stmt.Declaration(Token(token.Identifier("sum"), 1), Some(expr.Number(0.0))),
    stmt.Declaration(Token(token.Identifier("n"), 1), Some(expr.Number(3.0))),
    stmt.While(
      condition: expr.Binary(
        expr.Variable(Token(token.Identifier("n"), 1)),
        Token(token.Greater, 1),
        expr.Number(0.0),
      ),
      body: stmt.Block([
        stmt.Expression(expr.Assign(
          Token(token.Identifier("sum"), 1),
          expr.Binary(
            expr.Variable(Token(token.Identifier("sum"), 1)),
            Token(token.Plus, 1),
            expr.Variable(Token(token.Identifier("n"), 1)),
          ),
        )),
        stmt.Expression(expr.Assign(
          Token(token.Identifier("n"), 1),
          expr.Binary(
            expr.Variable(Token(token.Identifier("n"), 1)),
            Token(token.Minus, 1),
            expr.Number(1.0),
          ),
        )),
        stmt.Print(expr.Variable(Token(token.Identifier("sum"), 1))),
      ]),
    ),
  ]

  given_stmt
  |> execute_case
  |> pprint.format
  |> snapshot("while with variable modification")
}

// helper
fn execute_case(statement) {
  let #(rst, _) =
    new_test_interpreter()
    |> interpreter.execute(statement)
  rst
}

fn new_test_interpreter() -> Interpreter(fn() -> String) {
  Interpreter(env: environment.new(), io: io_controller.io_capturer())
}

fn snapshot(any: a, title title: String) {
  pprint.format(any)
  |> birdie.snap(title)
}
