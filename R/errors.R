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
  fn_text <- get_fn_text(fn)
  msg <- c(
    glue::glue("# ---\nType check failed for '{error$name}'"),
    paste("value:", value_text),
    paste("type:", error$type),
    paste("length:", error$len),
    fn_text
  )
  return(msg)
}

# ---
map_type_check_errors <- function(errors) {
  unlist(purrr::map(unname(errors), create_type_check_error_message))
}
