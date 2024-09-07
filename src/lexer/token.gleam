import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}

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
  Number(Float)

  // Keywords.
  And
  Class
  Else
  False
  Fun
  For
  If
  NilLiteral
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

pub fn to_string(token: Token) -> String {
  let token_type = token_type_string(token)
  let lexeme = to_lexeme(token)
  let literal = option.unwrap(to_literal(token), "null")
  token_type <> " " <> lexeme <> " " <> literal
}

pub fn token_type_string(token: Token) -> String {
  case token {
    LeftParen -> "LeftParen"
    RightParen -> "RightParen"
    LeftBrace -> "LeftBrace"
    RightBrace -> "RightBrace"
    Comma -> "Comma"
    Dot -> "Dot"
    Minus -> "Minus"
    Plus -> "Plus"
    Semicolon -> "Semicolon"
    Slash -> "Slash"
    Star -> "Star"

    Bang -> "Bang"
    NotEqual -> "NotEqual"
    Equal -> "Equal"
    EqualEqual -> "EqualEqual"
    Greater -> "Greater"
    GreaterEqual -> "GreaterEqual"
    Less -> "Less"
    LessEqual -> "LessEqual"

    Identifier(_) -> "Identifier"
    String(_) -> "String"
    Number(_) -> "Number"

    And -> "And"
    Class -> "Class"
    Else -> "Else"
    False -> "False"
    Fun -> "Fun"
    For -> "For"
    If -> "If"
    NilLiteral -> "Nil"
    Or -> "Or"
    Print -> "Print"
    Return -> "Return"
    Super -> "Super"
    This -> "This"
    True -> "True"
    Var -> "Var"
    While -> "While"

    EOF -> "EOF"
  }
}

pub fn to_lexeme(token: Token) -> String {
  case token {
    LeftParen -> "("
    RightParen -> ")"
    LeftBrace -> "{"
    RightBrace -> "}"
    Comma -> ","
    Dot -> "."
    Minus -> "-"
    Plus -> "+"
    Semicolon -> ";"
    Slash -> "/"
    Star -> "*"

    Bang -> "!"
    NotEqual -> "!="
    Equal -> "="
    EqualEqual -> "=="
    Greater -> ">"
    GreaterEqual -> ">="
    Less -> "<"
    LessEqual -> "<="

    Identifier(name) -> name
    String(value) -> "\"" <> value <> "\""
    Number(number) -> float.to_string(number)

    And -> "and"
    Class -> "class"
    Else -> "else"
    False -> "false"
    Fun -> "fun"
    For -> "for"
    If -> "if"
    NilLiteral -> "nil"
    Or -> "or"
    Print -> "print"
    Return -> "return"
    Super -> "super"
    This -> "this"
    True -> "true"
    Var -> "var"
    While -> "while"

    EOF -> ""
  }
}

pub fn to_literal(token: Token) -> Option(String) {
  case token {
    String(string) -> Some(string)
    Number(number) -> Some(float.to_string(number))
    _ -> None
  }
}

pub fn parse_number(str) -> Result(Token, Nil) {
  case float.parse(str) {
    Ok(number) -> Ok(Number(number))
    Error(_) ->
      case int.parse(str) {
        Ok(integer) -> Ok(Number(int.to_float(integer)))
        Error(_) -> Error(Nil)
      }
  }
}
