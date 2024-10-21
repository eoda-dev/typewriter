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

#' Check a model type
#' @param model_fn A model factory function.
#' @export
is_rdantic_model <- function(model_fn) {
  function(x) {
    is.list(model_validate(x, model_fn))
  }
}

# ---
dtype_integer <- function(n = NULL) {
  fn <- function(x) {
    typeof(x) == "integer" & length(x) == n
  }
  structure(fn, dtype = "integer", n = n)
}
