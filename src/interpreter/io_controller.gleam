//// This module provide a interface to control io behaviour. Delegating io
//// behaviour via this interface, make the program more testable.

import gleam/io

pub type IOInterface(a) {
  IOInterface(write_stdout: fn(String) -> a, wirte_stderr: fn(String) -> a)
}

pub fn default_io() -> IOInterface(Nil) {
  IOInterface(write_stdout: io.println, wirte_stderr: io.println_error)
}

pub fn io_capturer() -> IOInterface(fn() -> String) {
  let closure = fn(str) {
    let captured = str
    fn() { captured }
  }

  IOInterface(write_stdout: closure, wirte_stderr: closure)
}
