import error
import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import lexer/predicate.{
  is_alpha, is_alphanumeric, is_digit, is_newline, is_quotation_mark,
  is_single_char, is_single_or_double_char, is_whitespace,
}
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

fn shift(lexer: Lexer) -> Lexer {
  Lexer(..lexer, start: lexer.start + 1, current: lexer.current + 1)
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
  let Lexer(tokens:, line:, ..) = lexer
  case peak(lexer) {
    Some(char) -> {
      let new_lexer: Lexer = {
        use <- bool.lazy_guard(is_single_char(char), fn() {
          lex_single_character(lexer)
        })
        use <- bool.lazy_guard(is_single_or_double_char(char), fn() {
          lex_one_or_two_characters(lexer)
        })

        use <- bool.lazy_guard(is_whitespace(char), fn() { shift(lexer) })
        use <- bool.lazy_guard(is_newline(char), fn() {
          Lexer(..lexer, line: line + 1) |> shift
        })

        use <- bool.lazy_guard(is_quotation_mark(char), fn() {
          lex_string(lexer)
        })
        use <- bool.lazy_guard(is_digit(char), fn() { lex_number(lexer) })
        use <- bool.lazy_guard(is_alpha(char), fn() { lex_identifier(lexer) })

        consume_unsupport_character(lexer)
      }

      lex_tokens(new_lexer)
    }
    None -> Lexer(..lexer, tokens: list.append(tokens, [token.EOF]))
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
  let #(char0, char1) = #(peak(lexer), peak(shift(lexer)))

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
    Some(_) -> consume_comment(shift(lexer))
  }
}

fn lex_string(lexer) -> Lexer {
  shift(lexer) |> consume_string
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

fn lex_number(lexer) -> Lexer {
  let lexer = consume_number(lexer)
  let lexer = case is_need_consume_fraction(lexer) {
    True -> consume_number(Lexer(..lexer, current: lexer.current + 1))
    False -> lexer
  }

  let Lexer(source:, start:, current:, tokens:, ..) = lexer
  let length = current - start
  let parsed_tok =
    string.slice(source, at_index: start, length:)
    |> token.parse_number
  case parsed_tok {
    Ok(tok) ->
      Lexer(..lexer, start: current, tokens: list.append(tokens, [tok]))

    Error(_) -> {
      error.error(int.to_string(lexer.line), "Failed to parse number.")
      Lexer(..lexer, start: current)
    }
  }
}

fn consume_number(lexer) -> Lexer {
  let is_continue = peak(lexer) |> option.unwrap("") |> is_digit
  case is_continue {
    True -> consume_number(Lexer(..lexer, current: lexer.current + 1))
    False -> lexer
  }
}

fn is_need_consume_fraction(lexer) -> Bool {
  let next_lexer = Lexer(..lexer, current: lexer.current + 1)
  let is_next_digit = peak(next_lexer) |> option.unwrap("") |> is_digit

  case peak(lexer), is_next_digit {
    Some("."), True -> True
    _, _ -> False
  }
}

fn lex_identifier(lexer) -> Lexer {
  let Lexer(source:, start:, current:, tokens:, ..) = lexer
  let is_continue = peak(lexer) |> option.unwrap("") |> is_alphanumeric
  case is_continue {
    True -> lex_identifier(Lexer(..lexer, current: current + 1))
    False -> {
      let name = string.slice(source, at_index: start, length: current - start)
      Lexer(
        ..lexer,
        tokens: list.append(tokens, [token.Identifier(name)]),
        start: current,
      )
    }
  }
}

fn consume_unsupport_character(lexer: Lexer) -> Lexer {
  error.error(line: int.to_string(lexer.line), with: "Unexpected character.")
  shift(lexer)
}

fn consume_single_character(lexer, token_type) -> Lexer {
  case token_type {
    Some(t) -> Lexer(..lexer, tokens: list.append(lexer.tokens, [t]))
    None -> lexer
  }
  |> shift
}

fn consume_double_character(lexer, token_type) -> Lexer {
  case token_type {
    Some(t) -> Lexer(..lexer, tokens: list.append(lexer.tokens, [t]))
    None -> lexer
  }
  |> shift
  |> shift
}
