import error
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import token.{type Token}

pub type Lexer {
  Lexer(
    source: String,
    tokens: List(Token),
    start: Int,
    current: Int,
    line: Int,
  )
}

pub fn from_string(source: String) -> Lexer {
  Lexer(source:, tokens: [], start: 0, current: 0, line: 0)
}

pub fn get_tokens(lexer: Lexer) -> List(Token) {
  lexer.tokens
}

fn shift(lexer, offset) -> Lexer {
  Lexer(..lexer, start: lexer.start + offset, current: lexer.current + offset)
}

fn peak(lexer: Lexer) -> Option(String) {
  let Lexer(source:, current:, ..) = lexer
  case is_at_end(lexer) {
    True -> None
    False -> Some(string.slice(from: source, at_index: current, length: 1))
  }
}

fn is_at_end(lexer: Lexer) -> Bool {
  lexer.current >= string.length(lexer.source)
}

pub fn lex_tokens(lexer: Lexer) -> Lexer {
  let Lexer(tokens:, current:, line:, ..) = lexer
  case is_at_end(lexer) {
    True -> Lexer(..lexer, tokens: list.append(tokens, [token.EOF]))
    False -> {
      let char0 = peak(lexer)

      let new_lexer: Lexer = case char0 {
        // single character
        Some("(")
        | Some(")")
        | Some("{")
        | Some("}")
        | Some(",")
        | Some(".")
        | Some("-")
        | Some("+")
        | Some(";")
        | Some("*") -> lex_single_character(lexer)

        // one or two character
        Some("!") | Some("=") | Some(">") | Some("<") | Some("/") ->
          lex_one_or_two_characters(lexer)

        // whitespace and newline
        Some(" ") | Some("\r") | Some("\t") -> shift(lexer, 1)
        Some("\n") ->
          Lexer(
            ..lexer,
            start: current + 1,
            current: current + 1,
            line: line + 1,
          )

        Some("\"") -> lex_string(lexer)

        _ -> {
          error.error(line: int.to_string(line), with: "Unexpected character.")
          shift(lexer, 1)
        }
      }

      lex_tokens(new_lexer)
    }
  }
}

fn lex_single_character(lexer) -> Lexer {
  case peak(lexer) {
    Some("(") -> Some(token.LeftParen)
    Some(")") -> Some(token.RightParen)
    Some("{") -> Some(token.LeftBrace)
    Some("}") -> Some(token.RightBrace)
    Some(",") -> Some(token.Comma)
    Some(".") -> Some(token.Dot)
    Some("-") -> Some(token.Minus)
    Some("+") -> Some(token.Plus)
    Some(";") -> Some(token.Semicolon)
    Some("*") -> Some(token.Star)
    _ -> None
  }
  |> consume_single_character(lexer, _)
}

fn lex_one_or_two_characters(lexer) -> Lexer {
  let #(char0, char1) = #(peak(lexer), peak(shift(lexer, 1)))

  case char0, char1 {
    Some("!"), Some("=") ->
      consume_double_character(lexer, Some(token.NotEqual))
    Some("="), Some("=") ->
      consume_double_character(lexer, Some(token.EqualEqual))
    Some(">"), Some("=") ->
      consume_double_character(lexer, Some(token.GreaterEqual))
    Some("<"), Some("=") ->
      consume_double_character(lexer, Some(token.LessEqual))
    Some("/"), Some("/") -> {
      consume_comment(lexer)
    }
    Some("!"), _ -> consume_single_character(lexer, Some(token.Bang))
    Some("="), _ -> consume_single_character(lexer, Some(token.Equal))
    Some(">"), _ -> consume_single_character(lexer, Some(token.Greater))
    Some("<"), _ -> consume_single_character(lexer, Some(token.Less))
    Some("/"), _ -> consume_single_character(lexer, Some(token.Slash))
    _, _ -> consume_single_character(lexer, None)
  }
}

fn consume_comment(lexer: Lexer) -> Lexer {
  case peak(lexer) {
    // Do not consume the newline in order to update lexer's line.
    None | Some("\n") -> lexer
    Some(_) -> consume_comment(shift(lexer, 1))
  }
}

fn lex_string(lexer) -> Lexer {
  shift(lexer, 1) |> consume_string
}

fn consume_string(lexer) -> Lexer {
  let Lexer(source:, line:, start:, current:, tokens:) = lexer

  case peak(lexer) {
    // The closing "
    Some("\"") -> {
      // Get the raw substring trimmed quotation marks.
      let content =
        string.slice(source, at_index: start, length: current - start)
      let tokens = list.append(tokens, [token.String(content)])
      Lexer(..lexer, start: current + 1, current: current + 1, tokens:)
    }
    Some("\n") ->
      Lexer(..lexer, line: line + 1, current: current + 1)
      |> consume_string
    Some(_) ->
      Lexer(..lexer, current: current + 1)
      |> consume_string
    None -> {
      error.error(line: int.to_string(line), with: "Unterminated string.")
      lexer
    }
  }
}

fn consume_single_character(lexer, token_type) -> Lexer {
  consume_characters(lexer, token_type, 1)
}

fn consume_double_character(lexer, token_type) -> Lexer {
  consume_characters(lexer, token_type, 2)
}

fn consume_characters(
  lexer: Lexer,
  token_type: Option(token.Token),
  length: Int,
) -> Lexer {
  let new_tokens = case token_type {
    Some(t) -> list.append(lexer.tokens, [t])
    None -> lexer.tokens
  }
  Lexer(..lexer, tokens: new_tokens)
  |> shift(length)
}
