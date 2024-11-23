import gleam/list
import gleam/option
import gleam/result

import interpreter.{type Interpreter, Break, Effect, Interpreter, Pure, Void}
import interpreter/environment
import interpreter/io_controller
import parse

import birdie
import pprint

// Literal Tests
pub fn should_interpret_string_literal_test() {
  "\"test\\tstring\";"
  |> execute_from_source(title: "string literal expression")
}

pub fn should_interpret_boolean_literal_test() {
  "false;"
  |> execute_from_source(title: "boolean literal expression")
}

pub fn should_interpret_number_literal_test() {
  "1.0;"
  |> execute_from_source(title: "number literal expression")
}

pub fn should_interpret_nil_literal_test() {
  "nil;"
  |> execute_from_source(title: "nil literal expression")
}

// Unary Expression Tests
pub fn should_interpret_bang_on_false_test() {
  "!false;"
  |> execute_from_source(title: "bang on false")
}

pub fn should_interpret_bang_on_nil_test() {
  "!nil;"
  |> execute_from_source(title: "bang on nil")
}

pub fn should_interpret_bang_on_number_test() {
  "!23333.0;"
  |> execute_from_source(title: "bang on number")
}

// Binary Calculation Tests
pub fn should_interpret_number_addition_test() {
  "3.0 + 4.0;"
  |> execute_from_source(title: "number addition")
}

pub fn should_interpret_string_concatenation_test() {
  "\"foo\" + \"bar\";"
  |> execute_from_source(title: "string concatenation")
}

pub fn should_interpret_number_subtraction_test() {
  "7.0 - 3.0;"
  |> execute_from_source(title: "number subtraction")
}

pub fn should_interpret_number_multiplication_test() {
  "3.0 * 2.0;"
  |> execute_from_source(title: "number multiplication")
}

pub fn should_interpret_number_division_test() {
  "10.0 / 2.0;"
  |> execute_from_source(title: "number division")
}

// Comparison Tests
pub fn should_interpret_greater_than_test() {
  "10.0 > 5.0;"
  |> execute_from_source(title: "greater than comparison")
}

pub fn should_interpret_greater_equal_test() {
  "5.0 >= 5.0;"
  |> execute_from_source(title: "greater equal comparison")
}

pub fn should_interpret_less_than_test() {
  "2.0 < 5.0;"
  |> execute_from_source(title: "less than comparison")
}

pub fn should_interpret_less_equal_test() {
  "5.0 <= 5.0;"
  |> execute_from_source(title: "less equal comparison")
}

pub fn should_interpret_not_equal_test() {
  "5.0 != 3.0;"
  |> execute_from_source(title: "not equal comparison")
}

pub fn should_interpret_equal_equal_test() {
  "5.0 == 5.0;"
  |> execute_from_source(title: "equal equal comparison")
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
pub fn should_interpret_while_with_break_condition_test() {
  "var x = 0; while (true) { print x; x = x + 1; if (x > 2) break; }"
  |> execute_from_source(title: "while with break condition")
}

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

  let #(rst, interpreter) =
    stmts
    |> option.values
    |> list.reverse
    |> interpreter.execute(new_test_interpreter(), _)

  #(reveal_effect(rst), interpreter.env)
  |> snapshot(title: title)
}

fn reveal_effect(
  result: interpreter.ExecuteResult(fn() -> String),
) -> interpreter.ExecuteResult(String) {
  case result {
    Error(err) -> Error(err)
    Ok(value) ->
      case value {
        Break -> Break
        Void -> Void
        Pure(obj) -> Pure(obj)
        Effect(side_effect:, value:) ->
          Effect(
            side_effect: side_effect
              |> list.map(fn(fun) { fun() })
              |> list.reverse,
            value:,
          )
      }
      |> Ok
  }
}

fn new_test_interpreter() -> Interpreter(fn() -> String) {
  Interpreter(env: environment.new(), io: io_controller.io_capturer())
}

fn snapshot(any: a, title title: String) {
  pprint.format(any)
  |> birdie.snap(title)
}
