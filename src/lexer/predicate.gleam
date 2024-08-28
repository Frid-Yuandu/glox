import gleam/list

const single_chars = ["(", ")", "{", "}", ",", ".", "-", "+", ";", "*"]

const single_or_double_chars = ["!", "=", ">", "<", "/"]

const digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

const alphas = [
  "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p",
  "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "A", "B", "C", "D", "E", "F",
  "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V",
  "W", "X", "Y", "Z", "_",
]

pub fn is_alphanumeric(char: String) -> Bool {
  is_digit(char) || is_alpha(char)
}

pub fn is_digit(char: String) -> Bool {
  list.contains(digits, char)
}

pub fn is_alpha(char: String) -> Bool {
  list.contains(alphas, char)
}

pub fn is_single_char(char: String) -> Bool {
  list.contains(single_chars, char)
}

pub fn is_single_or_double_char(char: String) -> Bool {
  list.contains(single_or_double_chars, char)
}

pub fn is_whitespace(char) -> Bool {
  char == " " || char == "\r" || char == "\t"
}

pub fn is_newline(char) -> Bool {
  char == "\n"
}

pub fn is_quotation_mark(char) -> Bool {
  char == "\""
}
