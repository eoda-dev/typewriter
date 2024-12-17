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
optional_obs <- function(type_check_fn) {
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
