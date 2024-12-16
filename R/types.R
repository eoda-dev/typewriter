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
#Any <- function(n = NULL) {
#  if (is.null(x)) {
#    return(is_any)
#  }
#  return(function(x) length(x) == n)
#}

#' Type predicate `typewriter model`
#' @param model_fn A model factory function created with [base_model()].
#' @export
is_typewriter_model <- function(model_fn) {
  function(x) {
    is.list(model_validate(x, model_fn))
  }
}

# ---
is_logical <- function(x) {
  all(is.logical(x) & !is.na(x))
}

# ---
create_model_field <- function(
    dtype = c("integer", "double", "character", "logical", "list", "raw", "complex", "any"),
    n = NULL,
    optional = FALSE) {
  # Body
  dtype <- match.arg(dtype)
  if (dtype == "any") {
    return(model_field(fn = is_any))
  }

  base_fn <- function(x) typeof(x) == dtype
  if (dtype == "logical") {
    base_fn <- is_logical
  }

  error_msg <- paste("value must be of type", dtype)
  check_type <- base_fn
  if (is_not_null(n)) {
    error_msg <- paste0(error_msg, "(", n, ")")
    check_type <- function(x) base_fn(x) & length(x) == n
  }

  model_field(
    fn = check_type,
    optional = optional,
    error_msg = error_msg
  )
}

# ---
# Examples:
#   * integer
#   * integer:1
#   * optional:integer:1
model_field_from_str <- function(str) {
  optional <- FALSE
  if (startsWith(str, "optional:")) {
    optional <- TRUE
    str <- sub("optional:", "", str)
  }

  values <- unlist(strsplit(str, ":"))
  dtype <- values[1]
  n <- NULL
  if (length(values) == 2) {
    n <- values[2]
  }

  create_model_field(dtype, n, optional)
}

# ---
# Examples:
#   * integer()
#   * integer(1)
model_field_from_vec <- function(vec) {
  dtype = typeof(vec)
  n = length(vec)
  if (n == 0) {
    n <- NULL
  }

  create_model_field(dtype, n)
}

# ---
is_dtype_str <- function(obj) {
  if (length(obj) == 1 & typeof(obj) == "character") {
    if (nchar(obj) > 0) return(TRUE)
  }

  FALSE
}

# ---
as_model_field <- function(x) {
  if (is_dtype_str(x)) {
    return(model_field_from_str(x))
  }

  if (is.function(x)) {
    return(model_field(fn = x))
  }

  model_field_from_vec(x)
}

#' Mark a parameter as optional
#' @param obj A type definition object.
#' @returns A [model_field()] with `optional = TRUE`.
#' @export
optional <- function(obj) {
  if (!inherits(obj, CLASS_MODEL_FIELD)) {
    obj <- as_model_field(obj)
  }

  obj$optional <- TRUE
  return(obj)
}

# ---
#' Create a model field/type definition
#' @param x A type check function or type string.
#' @param default A default value.
#' @returns A [model_field()].
#' @export
dtype <- function(x, default = NA) {
  field <- as_model_field(x)
  field$default <- default
  return(field)
}
