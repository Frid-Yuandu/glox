pub fn with_ok(
  in result: Result(any, any_err),
  processer processer: env,
  with fun: fn(any, env) -> #(Result(mapped, any_err), env),
) -> #(Result(mapped, any_err), env) {
  case result {
    Ok(inner) -> fun(inner, processer)
    Error(err) -> #(Error(err), processer)
  }
}
