# ---
get_fn_text <- function(fn) {
  if (rlang::is_primitive(fn)) {
    return(rlang::quo_text(fn))
  }

  return(rlang::quo_text(rlang::fn_body(fn)))
}

# ---
#' @importFrom utils capture.output str
create_type_check_error_message <- function(error) {
  value_text <- paste0(capture.output(str(error$value)), collapse = "\n")
  fn <- error$type_check_failed
  base_func <- attributes(fn)$base_func
  fn_text <- get_fn_text(fn)
  if (is_not_null(base_func)) {
    fn_text <- get_fn_text(base_func)
  }

  msg <- c(
    glue::glue("# ---\nType check failed for '{error$name}'"),
    paste("value:", value_text),
    paste("type:", error$type),
    paste("length:", error$len),
    paste("expected:", fn_text)
  )
  return(msg)
}

# ---
map_type_check_errors <- function(errors) {
  unlist(Map(create_type_check_error_message, unname(errors)))
}
