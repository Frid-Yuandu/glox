# Project Instructions

This project implements a REPL (Read-Eval-Print Loop) for a custom programming language. The current implementation includes basic parsing, interpretation, and error handling.

## Current Features

1. REPL functionality ([src/repl.gleam](cci:7://file:///home/frid_yuandu/coding/gleam-project/glox/src/repl.gleam:0:0-0:0))
2. Basic parsing ([src/parse](cci:7://file:///home/frid_yuandu/coding/gleam-project/glox/src/parse:0:0-0:0))
3. Interpretation ([src/interpreter](cci:7://file:///home/frid_yuandu/coding/gleam-project/glox/src/interpreter:0:0-0:0))
4. Error handling (runtime and parse errors)
5. Basic statements: If, Block, Expression, Print, Declaration, While

## Future Features

The following features are planned for future implementation:

1. For loop
2. Break statement
3. Function declaration and calling
4. Class definition and inheritance
5. Enhanced REPL output for statements

## Development Guidelines

1. Follow the existing code structure and naming conventions.
2. Implement new features in their respective modules (e.g., new statements in [src/parse/stmt.gleam](cci:7://file:///home/frid_yuandu/coding/gleam-project/glox/src/parse/stmt.gleam:0:0-0:0)).
3. Update the interpreter ([src/interpreter](cci:7://file:///home/frid_yuandu/coding/gleam-project/glox/src/interpreter:0:0-0:0)) to handle new language constructs.
4. Extend error handling as needed for new features.
5. Add tests for new functionality in the `test` directory.

## Testing

Use the existing test structure in [test/interpret_test.gleam](cci:7://file:///home/frid_yuandu/coding/gleam-project/glox/test/interpret_test.gleam:0:0-0:0) as a guide for adding new tests.

## REPL Enhancements

When implementing more detailed output for statements in the REPL, update the [src/repl.gleam](cci:7://file:///home/frid_yuandu/coding/gleam-project/glox/src/repl.gleam:0:0-0:0) file, focusing on the `loop` function and potentially adding new helper functions for improved output formatting.
