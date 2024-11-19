# ---
model_fields <- function(model_fn) {
  attr(model_fn, "fields")
}

# ---
#' Create a model field
#' @param fn A type check function.
#' @param default A default value for the field.
#' @param alias,... **not used** at the moment
#' @export
model_field <- function(fn, default = NA, alias = NULL, ...) {
  l <- as.list(environment())
  return(structure(c(l, list(...)), class = CLASS_RDANTIC_MODEL_FIELD))
}

# ---
#' Create a model config object
#' @param allow_extra Whether to allow extra fields without type check.
#' @param str_to_lower Convert all strings to lower case.
#' @param ... **not used** at the moment
#' @returns A model config object that can be used in [base_model()].
#' @example examples/api/model-config.R
#' @export
model_config <- function(allow_extra = FALSE,
                         str_to_lower = FALSE, ...) {
  return(structure(c(as.list(environment()), list(...)), class = CLASS_MODEL_CONFIG))
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
#' @returns A model factory function.
#' @example examples/api/base-model.R
#' @export
base_model <- function(fields = list(), ...,
                       .model_config = model_config(),
                       .model_pre_init = NULL,
                       .model_post_init = NULL,
                       .validators_before = list(),
                       .validators_after = list()) {
  fields <- utils::modifyList(fields, list(...), keep.null = TRUE)
  # fields1 <- purrr::map(fields, ~ {
  #   if (inherits(.x, c("function", "formula"))) {
  #     return(model_field(fn = .x))
  #   }
  #
  #   if (inherits(.x, CLASS_RDANTIC_MODEL)) {
  #     model_fn <- .x
  #     fn <- function(x) {
  #       is.list(model_validate(x, model_fn))
  #     }
  #     return(model_field(fn = fn))
  #   }
  #
  #   return(.x)
  # })

  fields <- Map(function(.x){
    if (inherits(.x, c("function", "formula"))) {
      return(model_field(fn = .x))
    }

    if (inherits(.x, CLASS_RDANTIC_MODEL)) {
      model_fn <- .x
      fn <- function(x) {
        is.list(model_validate(x, model_fn))
      }
      return(model_field(fn = fn))
    }

    return(.x)
  }, fields)

  # model_args <- purrr::map(fields, ~ .x$default)
  model_args <- Map(function(x){
    x$default
  }, fields)
  # Create model factory function
  model_fn <- rlang::new_function(c(model_args, alist(... = , .x = NULL)), quote({
    if (is_not_null(.x)) {
      obj <- .x
    } else {
      obj <- c(as.list(environment()), list(...))
    }

    errors <- list()

    obj <- validate_fields(obj, .validators_before)

    if (is_not_null(.model_pre_init)) {
      obj <- rlang::as_function(.model_pre_init)(obj)
    }

    for (name in names(fields)) {
      check_type_fn <- rlang::as_function(fields[[name]]$fn)
      obj_value <- obj[[name]]
      check_results <- check_type_fn(obj_value)

      if(length(check_results) > 1) {
        stop("Check returned more than one TRUE or FALSE")
      }
      if (isFALSE(check_results)) {
        errors[[name]] <- list(
          name = name,
          value = obj_value,
          type = typeof(obj_value),
          len = length(obj_value),
          type_check_failed = check_type_fn
        )
      }
    }

    obj <- validate_fields(obj, .validators_after)

    if (isTRUE(.model_config$str_to_lower)) {
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

    if (isFALSE(.model_config$allow_extra)) {
      # obj <- purrr::keep_at(obj, names(fields))
      obj <- obj[names(fields)]
    }

    if (is_not_null(.model_post_init)) {
      obj <- rlang::as_function(.model_post_init)(obj)
    }

    if (is.data.frame(obj)) {
      return(obj)
    }

    return(structure(obj, fields = fields, class = c(class(obj), CLASS_RDANTIC)))
  }))

  return(
    structure(
      model_fn,
      fields = fields,
      class = CLASS_RDANTIC_MODEL
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

  e <- rlang::caller_env()
  for (name in names(e)) {
    value <- e[[name]]
    if (inherits(value, CLASS_RDANTIC_MODEL_FIELD)) {
      e[[name]] <- value$default
    }
  }

  base_model(fields)(.x = e)
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
print.rdantic <- function(x, ...) {
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
`$<-.rdantic` <- function(x, name, value) {
  if (isFALSE(name %in% names(x))) {
    return(x)
  }

  check_assignment(x, name, value)
  NextMethod()
}

# ---
#' @export
`[[<-.rdantic` <- function(x, name, value) {
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
#' @param obj An rdantic model object
#' @param by_alias Use aliases for names.
#' @param ... **not used** at the moment.
#' @returns base list object
#' @export
model_dump <- function(obj, by_alias = FALSE, ...) {
  if (isTRUE(by_alias)) {
    return(dump_by_alias(obj))
  }

  return(model_to_list(obj))
}

# ---
model_exclude_na <- function(obj) {
  discard_this(obj, rlang::is_na)
}
