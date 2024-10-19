# ---
#' Create a model
#'
#' @param fields A named list where values are functions used for the type checks.
#' @param ... Model parameters and their functions used for the type checks.
#'  Usually only `fields` or `...` are supplied.
#' @param .validators_before A list of validators that run before types are checked.
#' @param .validators_after A list of validators that run after types are checked.
#' @param .model_config **not used at the moment**
#' @returns base model function/
#' @example examples/api/base-model.R
#' @export
base_model <- function(fields = list(), ...,
                       .validators_before = NULL,
                       .validators_after = NULL,
                       .model_config = list()) {
  fields <- utils::modifyList(fields, list(...), keep.null = TRUE)
  model_fn <- function(obj = list(), ...) {
    if (is.list(obj)) {
      obj <- utils::modifyList(obj, list(...), keep.null = TRUE)
    }

    if (length(obj) == 0) {
      obj <- rlang::caller_env()
    }

    if (!is.null(.validators_before)) {
      obj <- validate_fields(obj, .validators_before)
    }

    for (name in names(fields)) {
      check_type <- rlang::as_function(fields[[name]])
      value <- obj[[name]]
      if (isFALSE(check_type(value))) {
        raise_type_check_error(name, value, check_type)
      }
    }

    if (!is.null(.validators_after)) {
      obj <- validate_fields(obj, .validators_after)
    }

    if (is.environment(obj)) {
      return(invisible(obj))
    }

    obj <- purrr::keep_at(obj, names(fields))
    # return(structure(obj, fields = fields, class = c("list")))
    return(obj)
  }

  return(set_attributes(model_fn, fields = fields))
}

#' Modify a [base_model()] object
#' @param obj [base_model()] object
#' @param exclude A set of fields to exclude from the output.
#' @param include A set of fields to include in the output.
#' @param exclude_null Whether to drop items with the value `NULL`.
#' @param exclude_na Whether to drop items with the value `NA`.
#' @param camels Whether to convert all keys to camel case.
#' @returns list
#' @example examples/api/model-dump.R
#' @export
model_dump <- function(obj,
                       exclude = NULL,
                       include = NULL,
                       exclude_null = FALSE,
                       exclude_na = FALSE,
                       camels = FALSE) {
  if (!is.null(exclude)) obj <- purrr::discard_at(obj, exclude)
  if (!is.null(include)) obj <- purrr::keep_at(obj, include)
  if (exclude_null) obj <- purrr::discard(obj, is.null)
  if (exclude_na) obj <- purrr::discard(obj, is.na)
  if (camels) obj <- keys_to_camel_case(obj)
  return(obj)
}

model_fields <- function(model_obj) {
  attr(model_obj, "fields")
}

# ---
#' Validate function arguments
#'
#' @inherit base_model params return
#' @example examples/api/validate-args.R
#' @export
validate_args <- function(..., .validators_before = NULL, .validators_after = NULL) {
  base_model(
    ...,
    .validators_before = .validators_before,
    .validators_after = .validators_after
  )(rlang::caller_env())
}

# ---
#' Validate function arguments
#'
#' @param fn A function containing the arguments to be checked.
#' @returns The function's `environment`.
#' @example examples/api/validate-fn.R
#' @export
validate_fn <- function(fn = rlang::caller_fn()) {
  fmls <- rlang::fn_fmls(fn)
  fields <- purrr::map(as.list(fmls), eval)
  base_model(fields)(rlang::caller_env())
}

# ---
#' Derive a [base_model()] object from a template list.
#'
#' Checks for type, mode and class of the object.
#'
#' @param template A list of key-value pairs where the value is used for type derivation.
#' @returns Returns a [base_model()] object.
#' @example examples/derive-model.R
#' @export
derive_model <- function(template) {
  fields <- purrr::map(template, function(v) {
    body <- substitute(
      {
        typeof(x) == type_ & class(x) == class_ & mode(x) == mode_
      },
      list(type_ = typeof(v), class_ = class(v), mode_ = mode(v))
    )
    rlang::new_function(alist(x = ), body = body)
  })
  return(base_model(fields))
}
