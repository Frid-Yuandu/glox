# glox

[![Package Version](https://img.shields.io/hexpm/v/glox)](https://hex.pm/packages/glox)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glox/)

This is an interpreter of Lox programming language written in Gleam.

You can get more information about Lox and how to design interpreters at [*crafting interpreters*](https://craftinginterpreters.com/). This is a great book, thanks to the author Robert Nystrom.


## Usage

```sh
git clone https://github.com/Frid-Yuandu/glox.git
cd glox
touch test.lox
# Writh some code in test.lox.
gleam run test.lox
```


Further documentation can be found at <https://hexdocs.pm/glox>.

## Design Notes & Implementation Details

You can read the design notes and implementation details [here](https://github.com/blob/master/DESIGN_NOTES.md).

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
