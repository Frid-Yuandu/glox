import birdie
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import parse
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
  ]

  "5 == 5;"
  |> execute_from_source(title: "equalities expression statements")
}

// error cases in binary expressions

pub fn should_error_on_add_number_and_non_number_types_test() {
  "1 + true;"
  |> execute_from_source(title: "error in add number with non-number")
}

pub fn should_error_in_addition_non_number_or_string_types_test() {
  "nil + false;"
  |> execute_from_source(title: "error in add not number or string type")
}

pub fn should_error_on_addition_string_and_non_string_test() {
  "\"foo\" + 1;"
  |> execute_from_source(title: "error in add wrong type to string")
}

pub fn should_error_on_subtraction_non_numbers_test() {
  "\"foo\" - 1;"
  |> execute_from_source(title: "error in subtract wrong type to string")
}

pub fn should_error_in_division_by_zero_test() {
  "10 / 0;"
  |> execute_from_source(title: "error in divide by zero")
}

// test grouping expressions

pub fn should_interpret_grouping_expression_test() {
  "(3 + 4);"
  |> execute_from_source(title: "non-empty gourping expression")
}

pub fn should_interpret_empty_grouping_test() {
  "();"
  |> execute_from_source(title: "empty grouping expression")
}

// test if statement

pub fn should_interpret_if_statement_with_then_branch_test() {
  "if (true)
print \"then\";"
  |> execute_from_source(title: "if statement with then branch only")
}

pub fn should_interpret_if_statement_with_else_branch_test() {
  "if (false)
print \"then\";
print \"else\";"
  |> execute_from_source(title: "if statement with else branch")
}

pub fn should_interpret_nested_if_statements_test() {
  "if (true)
if (true)
print \"nested then\";"
  |> execute_from_source(title: "nested if statements")
}

pub fn should_interpret_if_with_block_statement_test() {
  "if (true)
{
print \"first\";
print \"second\";
}"
  |> execute_from_source(title: "if statement with block")
}

pub fn should_interpret_if_with_expression_condition_test() {
  "if (5 > 3)
print \"condition true\";
else
print \"condition false\";"
  |> execute_from_source(title: "if statement with expression condition")
}

pub fn should_interpret_if_with_truthy_values_test() {
  "if (1)
print \"number is truthy\";

if (\"hello\")
print \"string is truthy\";

if (nil)
print \"won't print\";
print \"nil is falsy\";"
  |> execute_from_source(title: "if statement with different truthy values")
}

// test logic operation expressions

pub fn should_interpret_true_and_true_test() {
  "true and true;"
  |> execute_from_source(title: "true AND true")
}

pub fn should_interpret_true_and_false_test() {
  "true and false;"
  |> execute_from_source(title: "true AND false")
}

pub fn should_short_circuit_false_and_true_test() {
  "false and true ;"
  |> execute_from_source(title: "false AND true (short-circuit)")
}

pub fn should_interpret_chained_and_operations_test() {
  "true and true and true;"
  |> execute_from_source(title: "chained AND operations")
}

pub fn should_interpret_and_with_truthy_values_test() {
  "1 and \"hello\";"
  |> execute_from_source(title: "AND with truthy values")
}

// Logic OR Tests
pub fn should_short_circuit_true_or_true_test() {
  "true or true;"
  |> execute_from_source(title: "true OR true (short-circuit)")
}

// TODO: can refactor as a function call with side effect, when function support.
pub fn should_short_circuit_true_or_false_test() {
  "true or false;"
  |> execute_from_source(title: "true OR false (short-circuit)")
}

pub fn should_interpret_false_or_true_test() {
  "false or true;"
  |> execute_from_source(title: "false OR true")
}

pub fn should_interpret_false_or_false_test() {
  "false or false;"
  |> execute_from_source(title: "false OR false")
}

pub fn should_interpret_chained_or_operations_test() {
  "false or false or true;"
  |> execute_from_source(title: "chained OR operations")
}

// Mixed Logic Tests
pub fn should_interpret_and_or_combination_test() {
  "true and true or false;"
  |> execute_from_source(title: "AND-OR combination")
}

pub fn should_interpret_or_and_combination_test() {
  //  could not be:
  //   stmt.Expression(expr.LogicAnd(
  //     expr.LogicOr(expr.Boolean(False), Token(token.Or, 1), expr.Boolean(True)),
  //     Token(token.And, 1),
  //     expr.Boolean(True),
  //   )),
  // because logic or has higher priority.
  "(false or true) and true;"
  |> execute_from_source(title: "OR-AND combination")
}

pub fn should_interpret_nil_in_logic_operations_test() {
  "nil and true;"
  |> execute_from_source(title: "nil in logical operations")
}

// test while statements

pub fn should_interpret_basic_while_loop_test() {
  "var i = 0; while (i < 3) { print i; i = i + 1; }"
  |> execute_from_source(title: "basic while loop")
}

pub fn should_interpret_while_with_false_condition_test() {
  "while (false) { print \"should not print\"; }"
  |> execute_from_source(title: "while with false condition")
}

pub fn should_interpret_nested_while_loops_test() {
  "var i = 0; while (i < 2) {
  var j = 0;
  while (j < 2) { print i + j; j = j + 1; }
  i = i + 1;
}"
  |> execute_from_source(title: "nested while loops")
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
  "var x = 0; while (x < 2 and x >= 0) { print x; x = x + 1; }"
  |> execute_from_source(title: "while with logical condition")
}

pub fn should_interpret_while_with_variable_modification_test() {
  "var sum = 0; var n = 3; while (n > 0) { sum = sum + n; n = n - 1; }"
  |> execute_from_source(title: "while with variable modification")
}

// helper
fn execute_from_source(source source, title title) {
  let assert #(stmts, []) =
    source
    |> parse.from_source
    |> parse.parse
    |> result.partition

  let #(interpret_rst, _) =
    stmts
    |> option.values
    |> list.reverse
    |> interpreter.execute(new_test_interpreter(), _)

  snapshot(interpret_rst, title: title)
}

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
