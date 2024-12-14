# ---
model_fields <- function(model_fn) {
  attr(model_fn, "fields")
}

# ---
#' Create a model field
#' @param fn A type check function.
#' @param default A default value for the field.
#' @param alias alias that can be used in [model_dump()]
#' @param error_msg,... **not used** at the moment
#' @returns A model field.
#' @export
model_field <- function(fn, default = NA, alias = NULL, error_msg = NULL, ...) {
  obj <- c(as.list(environment()), list(...))
  base_class <- class(obj)
  structure(obj, class = c(base_class, CLASS_MODEL_FIELD))
}

model_field_new <- function(type, default = NA, alias = NULL) {
  base_class <- class(type)
  structure(type, class = c(base_class, CLASS_MODEL_FIELD), default = default, alias = alias)
}

# ---
#' Create a model config object
#' @param extra Whether to allow extra fields without type check.
#' @param str_to_lower Convert all strings to lower case.
#' @param ... **not used** at the moment
#' @returns A model config object that can be used in [base_model()].
#' @example examples/api/model-config.R
#' @export
model_config <- function(extra = c("ignore", "allow", "forbid"),
                         str_to_lower = FALSE, ...) {
  obj <- c(as.list(environment()), list(...))
  obj$extra <- match.arg(extra)
  base_class <- class(obj)
  return(structure(obj, class = c(base_class, CLASS_MODEL_CONFIG)))
}

# ---
#' Create a model factory function
#' @param fields A named list of field definitions.
#' @param ... Named arguments of field definitions.
#'  Normally either `fields` or `...` is supplied.
#' @param .model_config See [model_config()].
#' @param .model_pre_init A callback function that is executed before the type checks.
#' @param .model_post_init A callback function that is executed after the type checks.
#' @param .validators_before A named list of field validators
#'  that are executed before the type checks.
#' @param .validators_after A named list of field validators
#'  that are executed after the type checks.
#' @param .strict_args_order If set to `TRUE`, the `.x` parameter
#'  of the returned model factory function will be the last function argument.
#'  This is useful if you want to pass the arguments unnamed.
#' @returns A model factory function.
#' @example examples/api/base-model.R
#' @importFrom utils modifyList
#' @export
base_model <- function(fields = list(), ...,
                       .model_config = model_config(),
                       .model_pre_init = NULL,
                       .model_post_init = NULL,
                       .validators_before = list(),
                       .validators_after = list(),
                       .strict_args_order = FALSE) {
  fields <- modifyList(fields, list(...), keep.null = TRUE)
  fields <- Map(function(.x) {
    if (inherits(.x, CLASS_MODEL_FUNCTION)) {
      model_fn <- .x
      fn <- function(x) {
        is.list(model_validate(x, model_fn))
      }
      return(model_field(fn = fn))
    }

    if (!inherits(.x, CLASS_MODEL_FIELD)) {
      .x <- model_field(fn = .x)
    }

    if (is.character(.x$fn)) {
      .x$fn <- type_check_fn_from_str(.x$fn)
    }

    return(.x)
  }, fields)

  # model_args <- purrr::map(fields, ~ .x$default)
  model_args <- Map(function(x) x$default, fields)
  fn_args <- c(alist(.x = NULL), model_args, alist(... = ))
  if (.strict_args_order) {
    fn_args <- c(model_args, alist(... = , .x = NULL))
  }

  # ---
  # Create model factory function
  model_fn <- rlang::new_function(fn_args, quote({
    if (is_not_null(.x)) {
      obj <- .x
    } else {
      obj <- c(as.list(environment()), list(...))
    }

    caller_fn_name <- as.character(match.call()[[1]])
    errors <- list()

    obj <- validate_fields(obj, .validators_before)

    if (is_not_null(.model_pre_init)) {
      obj <- rlang::as_function(.model_pre_init)(obj)
    }

    for (name in names(fields)) {
      type_check_fn <- rlang::as_function(fields[[name]]$fn)
      obj_value <- obj[[name]]

      if (!all(type_check_fn(obj_value))) {
        errors[[name]] <- list(
          name = name,
          value = obj_value,
          type_check_fn = type_check_fn
        )
      }
    }

    obj <- validate_fields(obj, .validators_after)

    if (.model_config$str_to_lower) {
      # obj <- purrr::map_depth(obj, -1, str_to_lower)
      obj <- map_depth_base(obj, -1, str_to_lower)
    }

    if (length(errors) > 0) {
      msg <- paste0(map_type_check_errors(errors), collapse = "\n")
      stop("Type check(s) failed\n", msg, domain = NA)
    }

    if (is.environment(obj)) {
      return(invisible(obj))
    }

    if (.model_config$extra == "ignore") {
      obj <- obj[names(fields)]
    }

    if (.model_config$extra == "forbid") {
      extra_fields <- !names(obj) %in% names(fields)
      if (any(extra_fields)) {
        stop("Forbidden field(s): ", paste(names(obj)[extra_fields], collapse = ", "))
      }
    }

    if (is_not_null(.model_post_init)) {
      obj <- rlang::as_function(.model_post_init)(obj)
    }

    if (is.data.frame(obj)) {
      return(obj)
    }

    return(structure(obj, fields = fields, class = c(class(obj), CLASS_MODEL, caller_fn_name)))
  }))

  return(
    structure(
      model_fn,
      fields = fields,
      class = CLASS_MODEL_FUNCTION
    )
  )
}

# ---
#' Check function arguments
#' @param ... Arg definitions.
#' @returns The caller environment.
#' @examples {
#'   f <- function(a, b) {
#'     check_args(a = is.integer, b = is.integer)
#'     a + b
#'   }
#'
#'   # Succeeds
#'   f(10L, 20L)
#'
#'   # Fails
#'   try(f(10L, 4.6))
#' }
#' @export
check_args <- function(...) {
  fields <- list(...)
  if (length(fields) == 0) {
    fn <- rlang::caller_fn()
    fmls <- rlang::fn_fmls(fn)
    fields <- Map(eval, as.list(fmls))
  }

  func_env <- rlang::caller_env()
  for (name in names(func_env)) {
    value <- func_env[[name]]
    if (inherits(value, CLASS_MODEL_FIELD)) {
      func_env[[name]] <- value$default
    }
  }

  base_model(fields)(.x = func_env)
}

# ---
#' Validate a list or a data frame
#' @param obj A list or a data.frame.
#' @param model_fn A model factory function created with [base_model()].
#' @export
model_validate <- function(obj, model_fn) {
  model_fn(.x = obj)
}

# ---
#' @export
print.typewriter <- function(x, ...) {
  print(x[seq_along(x)])
  return(invisible(x))
}

# ---
check_assignment <- function(x, name, value) {
  fields <- model_fields(x)
  type_check_fn <- rlang::as_function(fields[[name]]$fn)
  fn_text <- get_fn_text(type_check_fn)
  if (isFALSE(type_check_fn(value))) {
    stop(paste0("Type check failed.\n", fn_text))
  }
}

# ---
#' @export
`$<-.typewriter` <- function(x, name, value) {
  if (isFALSE(name %in% names(x))) {
    return(x)
  }

  check_assignment(x, name, value)
  NextMethod()
}

# ---
#' @export
`[[<-.typewriter` <- function(x, name, value) {
  if (isFALSE(name %in% names(x))) {
    return(x)
  }

  check_assignment(x, name, value)
  NextMethod()
}

# ---
# model_validate_from_json <- function(path, model_fn, simplify_vec = TRUE, ...) {
#  obj <- jsonlite::read_json(path, simplifyVector = simplify_vec, ...)
#  model_validate(obj, model_fn)
# }

# ---
#' Convert model to base list
#' @param obj A typewriter model object
#' @param by_alias Use aliases for names.
#' @param exclude_na Whether to exclude `NA` values.
#' @param exclude_null Whether to exclude `NULL` values.
#' @param ... **not used** at the moment.
#' @returns base list object
#' @export
model_dump <- function(obj,
                       by_alias = FALSE,
                       exclude_null = FALSE,
                       exclude_na = FALSE,
                       ...) {
  if (isTRUE(by_alias)) {
    # return(dump_by_alias(obj))
    obj <- dump_by_alias(obj)
  }

  if (exclude_na) {
    obj <- discard_this(obj, rlang::is_na)
  }

  if (exclude_null) {
    obj <- discard_this(obj, is.null)
  }

  return(unclass(obj))
  # return(model_to_list(obj)) # dumps NULLs!
}

# ---
model_exclude_na <- function(obj) {
  discard_this(obj, rlang::is_na)
}
