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
Any <- function(n = NULL) {
  if (is.null(x)) {
    return(is_any)
  }
  return(function(x) length(x) == n)
}

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
  type_str <- match.arg(dtype)
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

# ---
#' Create a model field
#' @param x A type check function or type string.
#' @param default A default value.
#' @param optional description
#' @returns A type check function
#' @export
dtype <- function(x, default = NA, optional = FALSE) {
  field <- as_model_field(x)
  field$default <- default
  field$optional <- optional
  return(field)
}

# ---
# Helper
# DEPRECATED
as_type_check_func <- function(type_check) {
  if (is.character(type_check)) {
    type_check <- type_check_fn_from_str(type_check)
  }

  rlang::as_function(type_check)
}
#### DEPRECATED ####

# ---
#' Mark a parameter as optional
#' @param type_check_fn Type check function or type string.
#' @example examples/api/type-is-optional.R
#' @returns type check function
#' @export
# TODO: Refactor to support model_fields
optional <- function(type_check_fn) {
  if (inherits(type_check_fn, CLASS_MODEL_FIELD)) {
    stop(CLASS_MODEL_FIELD, " objects are not supported.")
  }

  type_check_fn <- as_type_check_func(type_check_fn)
  new_type_check_fn <- structure(
    function(x) type_check_fn(x) | rlang::is_na(x),
    base_func = type_check_fn
  )
  new_type_check_fn
}

# ---
#' Allow multiple types
#' @param ... Type check functions or type strings
#' @returns A type check function
#' @example examples/api/types-union.R
#' @export
either <- function(...) {
  fns <- lapply(list(...), as_type_check_func)
  structure(
    function(x) {
      any(unlist(lapply(fns, function(fn) fn(x))))
    },
    base_func = fns
  )
}

# ---
# Helper
base_type <- function(
    type_str = c("integer", "double", "character", "logical", "list", "raw", "complex", "any"),
    n = NULL,
    default = NA) {
  match.arg(type_str)
  if (type_str == "any") {
    return(is_any)
  }

  body <- substitute(typeof(x) == dtype, list(dtype = type_str))

  if (is_not_null(n)) {
    body <- substitute(
      typeof(x) == dtype & length(x) == n,
      list(dtype = type_str, n = as.integer(n))
    )
  }

  fn <- rlang::new_function(alist(x = ), body)
  if (!is.na(default)) {
    return(model_field(fn, default))
  }

  return(fn)
}

# --- Experimental

# ---
type_integer <- function(n = NULL, default = NA) {
  base_type("integer", n, default)
}

# ---
type_double <- function(n = NULL, default = NA) {
  base_type("double", n, default)
}

# ---
type_character <- function(n = NULL, default = NA) {
  base_type("character", n, default)
}

# ---
type_logical <- function(n = NULL, default = NA) {
  base_type("logical", n, default)
}

# ---
# DEPRECATED
dtype_integer <- function(n = NULL) {
  fn <- function(x) {
    typeof(x) == "integer" & length(x) == n
  }
  structure(fn, dtype = "integer", n = n)
}
