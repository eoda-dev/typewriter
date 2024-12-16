# ---
get_fn_text <- function(fn) {
  if (rlang::is_primitive(fn) | is.list(fn)) {
    return(rlang::quo_text(fn))
  }

  return(rlang::quo_text(rlang::fn_body(fn)))
}

# ---
#' @importFrom utils capture.output str
create_type_check_error_message <- function(error) {
  value <- error$value
  value_text <- paste0(capture.output(str(value)), collapse = "\n")
  fn <- error$type_check_fn
  base_func <- attributes(fn)$base_func
  fn_text <- get_fn_text(fn)
  if (is_not_null(base_func)) {
    fn_text <- get_fn_text(base_func)
  }

  class_text <- paste(class(value), collapse = ", ")

  msg <- c(
    glue::glue("---\nType check failed for '{error$name}'"),
    paste("value:", value_text),
    paste("type:", typeof(value)),
    paste("class:", class_text),
    paste("length:", length(value)),
    ifelse(is.null(error$msg), paste("expected:", fn_text), error$msg)
  )
  return(msg)
}

# ---
map_type_check_errors <- function(errors) {
  unlist(Map(create_type_check_error_message, unname(errors)))
}
