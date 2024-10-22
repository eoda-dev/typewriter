# ---
create_type_check_error_message <- function(error) {
  fn <- error$type_check_failed
  if (rlang::is_primitive(fn)) {
    text_fn <- rlang::quo_text(fn)
  } else {
    text_fn <- rlang::quo_text(rlang::fn_body(fn))
  }
  msg <- c(
    "# ---",
    glue::glue("Type check failed: {error$name} = {rlang::quo_text(error$value)}"),
    text_fn
  )
  return(msg)
}

# ---
map_type_check_errors <- function(errors) {
  unlist(purrr::map(unname(errors), create_type_check_error_message))
}
