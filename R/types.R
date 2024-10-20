is_any <- function(x) TRUE

# ---
#' Mark a field as optional
#' @param fn type check function
#' @export
is_optional <- function(fn) {
  fn_name <- deparse(substitute(fn))
  eval(parse(text = paste0("function(x) ", fn_name, "(x) | is.null(x)")))
}

# ---
optional_field <- function(type_check_fn) {
  function(x) {
    type_check_fn(x) | is.null(x) | rlang::is_na(x)
  }
}

#' Check/assert a model inside a model
#' @param model_obj model to check
#' @export
is_model <- function(model_obj) {
  function(x) is.list(model_obj(x))
}
