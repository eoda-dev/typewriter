# ---
raise_type_check_error <- function(name, value, check_type, call = rlang::caller_env()) {
  value_text <- rlang::quo_text(value)
  check_type_text <- ifelse(
    rlang::is_primitive(check_type),
    rlang::prim_name(check_type),
    rlang::quo_text(check_type)
  )
  cli::cli_abort(
    c(
      "Type check failed.",
      i = "field: {name}",
      x = "value: {value_text}",
      x = "test: {check_type_text}"
    ),
    call = call
  )
}
