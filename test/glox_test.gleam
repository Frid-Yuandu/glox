import birdie
import gleam/io
import gleeunit
import pprint

import glox
import interpreter

import simplifile

// import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn should_parse_and_interpret_nested_block__test() {
  let assert Ok(content) =
    simplifile.read("./test/test_source/nested_block.lox")
  let interpreter = interpreter.new()

  glox.run(interpreter, content)
  |> pprint.format
  |> birdie.snap(title: "run nested block from file")
}
