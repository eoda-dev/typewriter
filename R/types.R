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

#' Type predicate `typewriter model`
#' @param model_fn A model factory function created with [base_model()].
#' @export
is_typewriter_model <- function(model_fn) {
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

# ---
# Examples:
#   str = "integer"
#   str = "integer:1"
type_check_fn_from_str <- function(str) {
  optional <- FALSE
  if (startsWith(str, "optional:")) {
    optional <- TRUE
    str <- sub("optional:", "", str)
  }

  values <- unlist(strsplit(str, ":"))
  dtype <- values[1]
  # fn_str <- glue::glue('function(x) typeof(x) == "{dtype}"')
  # if (length(values) == 2) {
  #   fn_str = paste(fn_str, "& length(x) ==", values[2])
  # }

  # eval(parse(text = fn_str))
  fn_args <- alist(x = )
  body <- substitute(
    {
      typeof(x) == dtype
    },
    list(dtype = dtype)
  )
  if (length(values) == 2) {
    n <- as.integer(values[2])
    body <- substitute(
      {
        typeof(x) == dtype & length(x) == n
      },
      list(dtype = dtype, n = n)
    )
  }

  fn <- rlang::new_function(fn_args, body)
  if (optional) {
    return(Optional(fn))
  }

  fn
}

# ---
#' Create a type check function (validator)
#' @param type_check description
#' @param default A default value.
#' @returns A type check function (validator)
#' @export
dtype <- function(type_check, default = NA) {
  model_field(as_type_check_func(type_check), default)
}

# ---
as_type_check_func <- function(type_check_fn) {
  if (is.character(type_check_fn)) {
    type_check_fn <- type_check_fn_from_str(type_check_fn)
  }

  rlang::as_function(type_check_fn)
}

# ---
#' Mark a parameter as optional
#' @param type_check_fn Type check function or type as
#'  character like `integer` or `integer:1`.
#' @example examples/api/type-is-optional.R
#' @returns type check function
#' @export
Optional <- function(type_check_fn) {
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
#' @param ... Type check functions
#' @returns A type check function
#' @examples {
#'   m <- base_model(
#'     a = Union(is.integer, is.null)
#'   )
#'   m(10L)
#'   m(NULL)
#' }
#' @export
Union <- function(...) {
  fns <- list(...)
  structure(
    function(x) {
      any(unlist(lapply(fns, function(fn) fn(x))))
    },
    base_func = fns
  )
}

# ---
BaseType <- function(type_str, n = NULL, default = NA) {
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

# ---
Integer <- function(n = NULL, default = NA) {
  BaseType("integer", n, default)
}

# ---
Double <- function(n = NULL, default = NA) {
  BaseType("double", n, default)
}

# ---
Character <- function(n = NULL, default = NA) {
  BaseType("character", n, default)
}

# ---
Logical <- function(n = NULL, default = NA) {
  BaseType("logical", n, default)
}
