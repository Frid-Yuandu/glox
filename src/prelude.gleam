// import gleam/option.{type Option, None, Some}

// pub fn with_some(
//   in maybe: Option(any),
//   processer processer: env,
//   with fun: fn(any, env) -> #(Option(mapped), env),
// ) -> #(Option(mapped), env) {
//   case maybe {
//     Some(inner) -> fun(inner, processer)
//     None -> #(None, processer)
//   }
// }

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
