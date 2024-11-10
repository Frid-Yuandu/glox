import gleam/option.{type Option, None, Some}

pub fn ensure_exist(
  in maybe: Option(any),
  otherwise return: Result(mapped, anyerr),
  processer processer: env,
  with fun: fn(any) -> #(Result(mapped, anyerr), env),
) -> #(Result(mapped, anyerr), env) {
  case maybe {
    Some(inner) -> fun(inner)
    None -> #(return, processer)
  }
}

// deprecate
pub fn with_ok_old(
  in result: Result(any, any_err),
  processer processer: env,
  with fun: fn(any, env) -> #(Result(mapped, any_err), env),
) -> #(Result(mapped, any_err), env) {
  case result {
    Ok(inner) -> fun(inner, processer)
    Error(err) -> #(Error(err), processer)
  }
}

pub fn with_ok(
  in result: Result(any, any_err),
  processer processer: env,
  with fun: fn(any) -> #(Result(mapped, any_err), env),
) -> #(Result(mapped, any_err), env) {
  case result {
    Ok(inner) -> fun(inner)
    Error(err) -> #(Error(err), processer)
  }
}
