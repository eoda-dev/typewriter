# ---
#' @importFrom utils capture.output str
create_type_check_error_message <- function(error) {
  value_text <- paste0(capture.output(str(error$value)), collapse = "\n")
  fn <- error$type_check_failed
  if (rlang::is_primitive(fn)) {
    text_fn <- rlang::quo_text(fn)
  } else {
    text_fn <- rlang::quo_text(rlang::fn_body(fn))
  }
  text_fn <- gsub("\\(.x\\)|\\(x\\)", glue::glue("({error$name})"), text_fn)
  msg <- c(
    glue::glue("# ---\nType check failed for '{error$name}'"),
    paste("value:", value_text),
    paste("type:", error$type),
    paste("length:", error$len),
    text_fn
  )
  return(msg)
}

# ---
map_type_check_errors <- function(errors) {
  unlist(purrr::map(unname(errors), create_type_check_error_message))
}
