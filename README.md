# Glox: A Lox Programming Language Interpreter in Gleam

[![Package Version](https://img.shields.io/hexpm/v/glox)](https://hex.pm/packages/glox)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glox/)

Glox is an interpreter for the Lox programming language, written in Gleam. Lox is a modern, dynamically-typed language with a focus on simplicity and ease of use.

You can get more information about Lox and how to design interpreters at [*crafting interpreters*](https://craftinginterpreters.com/). This is a great book, thanks to the author Robert Nystrom.

## Overview

Glox aims to provide a fast, reliable, and easy-to-use implementation of the Lox language. It is designed to be highly extensible, allowing developers to easily add new features and functionality.

## Features

- [x] Basic Lox language syntax support
- [x] Includes a built-in lexer, parser, and interpreter
- [x] Provides a simple REPL for executing Lox code
- [x] Supports basic statements: If, Block, Expression, Print, Declaration, While
- [x] Error handling for runtime and parse errors
- [ ] Full Lox language support (see [INSTRUCTION.md](INSTRUCTION.md) for planned features)

## Usage

To use Glox, clone the repository and run the following commands:

```sh
gleam run   # Start the REPL

# Or interpret a file
gleam export erlang-shipment
build/erlang-shipment/entrypoint.sh run /path/to/lox/file
```

## Development

For detailed information on the project structure, current features, and planned enhancements, please refer to the [INSTRUCTION.md](INSTRUCTION.md) file in the project root directory.

## Documentation (TODO)

For more information on using Glox, including a full API reference and documentation on the Lox language, please see the Glox documentation.

## Contributing

Glox is an open-source project, and contributions are welcome. If you're interested in helping out, please see the CONTRIBUTING.md (TODO) file for more information.

## License

Glox is released under the MIT license. See the [LICENSE](LICENSE) file for more information.
