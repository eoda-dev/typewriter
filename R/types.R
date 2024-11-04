# ---
#' Type predicate `any`
#' @param x Object to be tested.
#' @returns `TRUE`
#' @examples
#' my_model <- base_model(
#'   a = is_any,
#'   b = is.integer
#' )
#'
#' # Succeeds
#' my_model(a = 10, b = 20L)
#'
#' # Also succeeds
#' my_model(a = "Hi", b = 20L)
#'
#' @export
is_any <- function(x) TRUE

# ---
# DEPRECATED
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

#' Type predicate `rdantic model`
#' @param model_fn A model factory function created with [base_model()].
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
