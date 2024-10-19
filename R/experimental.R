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
#' @param allow_extra description
#' @param str_to_lower description
#' @returns A model config object that can be used in [base_model()].
#' @export
model_config <- function(allow_extra = FALSE,
                         str_to_lower = FALSE, ...) {
  return(structure(c(as.list(environment()), list(...)), class = CLASS_MODEL_CONFIG))
}

# ---
#' Create a model factory function
#' @param fields description
#' @param ... description
#' @param .model_config description
#' @param .model_post_init description
#' @param .validators_before description
#' @param .validators_after description
#' @returns A model factory function.
#' @example examples/api/base-model.R
#' @export
base_model <- function(fields = list(), ...,
                       .model_config = model_config(),
                       .model_post_init = NULL,
                       .validators_before = list(),
                       .validators_after = list()) {
  fields <- utils::modifyList(fields, list(...), keep.null = TRUE)
  fields <- purrr::map(fields, ~ {
    if (inherits(.x, "function")) {
      return(model_field(fn = .x))
    }

    return(.x)
  })

  model_args <- purrr::map(fields, ~ .x$default)

  # Create model factory function
  model_fn <- rlang::new_function(c(model_args, alist(... = , .x = NULL)), quote({
    if (is_not_null(.x)) {
      obj <- .x
    } else {
      obj <- as.list(environment())
    }

    obj <- validate_fields(obj, .validators_before)

    for (name in names(fields)) {
      check_type_fn <- rlang::as_function(fields[[name]]$fn)
      obj_value <- obj[[name]]
      if (isFALSE(check_type_fn(obj_value))) {
        cli::cli_abort(
          c(
            "Type check failed.",
            "!" = "field: {name}, type: {typeof(obj_value)}, length: {length(obj_value)}",
            x = "{name} = {rlang::quo_text(obj_value)}",
            x = "{rlang::quo_text(check_type_fn)}"
          ),
          .frame = rlang::current_env()
        )
      }
    }

    obj <- validate_fields(obj, .validators_after)

    if (isTRUE(.model_config$str_to_lower)) {
      obj <- purrr::map_depth(obj, -1, str_to_lower)
    }

    if (is.environment(obj)) {
      return(invisible(obj))
    }

    if (isFALSE(.model_config$allow_extra)) {
      obj <- purrr::keep_at(obj, names(fields))
    }

    if (is_not_null(.model_post_init)) {
      obj <- rlang::as_function(.model_post_init)(obj)
    }

    if (is.data.frame(obj)) {
      return(obj)
    }

    return(structure(obj, fields = fields, class = CLASS_RDANTIC))
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
#' @param ... description
#' @returns The caller environment.
#' @export
check_args <- function(...) {
  fields <- list(...)
  if (length(fields) == 0) {
    fn <- rlang::caller_fn()
    fmls <- rlang::fn_fmls(fn)
    fields <- purrr::map(as.list(fmls), eval)
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
#' Validate an object
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
# model_validate_from_json <- function(path, model_fn, simplify_vec = TRUE, ...) {
#  obj <- jsonlite::read_json(path, simplifyVector = simplify_vec, ...)
#  model_validate(obj, model_fn)
# }

# ---
# TODO: Deprecated, use single functions as 'exclude_na'
model_dump <- function(obj,
                       exclude = NULL,
                       include = NULL,
                       exclude_na = FALSE,
                       exclude_null = FALSE,
                       by_alias = FALSE,
                       keys_to_camel_case = FALSE) {
  fields <- model_fields(obj)

  if (is_not_null(exclude)) obj <- purrr::discard_at(obj, exclude)
  if (is_not_null(include)) obj <- purrr::keep_at(obj, include)
  if (isTRUE(exclude_na)) obj <- discard_this(obj, rlang::is_na)
  if (isTRUE(exclude_null)) obj <- discard_this(obj, rlang::is_null)
  if (isTRUE(keys_to_camel_case)) obj <- keys_to_camel_case(obj)

  return(obj)
}
