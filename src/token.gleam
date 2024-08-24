import gleam/float
import gleam/int

pub type Token {
  // Single-character tokens.
  LeftParen
  RightParen
  LeftBrace
  RightBrace
  Comma
  Dot
  Minus
  Plus
  Semicolon
  Slash
  Star

  // One or two character tokens.
  Bang
  NotEqual
  Equal
  EqualEqual
  Greater
  GreaterEqual
  Less
  LessEqual

  // Literals.
  Identifier(String)
  String(String)
  Number(Number)

  // Keywords.
  And
  Class
  Else
  False
  Fun
  For
  If
  Nil
  Or
  Print
  Return
  Super
  This
  True
  Var
  While

  EOF
}

pub opaque type Number {
  Int(Int)
  Float(Float)
}

fn number_to_string(number: Number) -> String {
  case number {
    Int(x) -> int.to_string(x)
    Float(x) -> float.to_string(x)
  }
}

pub fn to_string(token: Token) -> String {
  let #(token_type, lexeme) = case token {
    LeftParen -> #("LeftParen", "(")
    RightParen -> #("RightParen", ")")
    LeftBrace -> #("LeftBrace", "{")
    RightBrace -> #("RightBrace", "}")
    Comma -> #("Comma", ",")
    Dot -> #("Dot", ".")
    Minus -> #("Minus", "-")
    Plus -> #("Plus", "+")
    Semicolon -> #("Semicolon", ";")
    Slash -> #("Slash", "/")
    Star -> #("Star", "*")

    Bang -> #("Bang", "!")
    NotEqual -> #("NotEqual", "!=")
    Equal -> #("Equal", "=")
    EqualEqual -> #("EqualEqual", "==")
    Greater -> #("Greater", ">")
    GreaterEqual -> #("GreaterEqual", ">=")
    Less -> #("Less", "<")
    LessEqual -> #("LessEqual", "<=")

    Identifier(name) -> #("Identifier", name)
    String(str) -> #("String", "\"" <> str <> "\"")
    Number(num) -> #("Number", number_to_string(num))

    And -> #("And", "and")
    Class -> #("Class", "class")
    Else -> #("Else", "else")
    False -> #("False", "false")
    Fun -> #("Fun", "fun")
    For -> #("For", "for")
    If -> #("If", "if")
    Nil -> #("Nil", "nil")
    Or -> #("Or", "or")
    Print -> #("Print", "print")
    Return -> #("Return", "return")
    Super -> #("Super", "super")
    This -> #("This", "this")
    True -> #("True", "true")
    Var -> #("Var", "var")
    While -> #("While", "while")

    EOF -> #("EOF", "")
  }
  let literal = case token {
    String(str) -> str
    Number(num) -> number_to_string(num)
    _ -> "null"
  }

  token_type <> lexeme <> literal
}
