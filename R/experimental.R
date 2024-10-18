# ---
model_field <- function(fn, default = NA, ...) {
  l <- as.list(environment())
  return(c(l, list(...)))
}


# ---
model_config <- function(allow_extra = FALSE,
                         str_to_lower = FALSE, ...) {
  return(c(as.list(environment()), list(...)))
}

# ---
base_model2 <- function(fields = list(), ...,
                        .model_config = model_config(),
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

  # Create model function
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
            x = "Type check failed.",
            i = "{name} = {rlang::quo_text(obj_value)}",
            i = "type: {typeof(obj_value)}",
            i = "length: {length(obj_value)}",
            x = rlang::quo_text(check_type_fn)
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

    return(structure(obj, fields = fields, class = c("rdantic_model")))
  }))

  return(
    structure(
      model_fn,
      fields = fields,
      class = c(class(model_fn), "rdantic_base_model")
    )
  )
}

# ---
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
    if (is.list(value)) {
      e[[name]] <- value$default
    }
  }

  base_model2(fields)(.x = e)
}

# ---
model_validate <- function(obj, model_fn) {
  model_fn(.x = obj)
}

# ---
#' @export
print.rdantic_model <- function(x, ...) {
  print(x[seq_along(x)])
  return(invisible(x))
}

# ---
discard_all <- function(x, fn = rlang::is_na) {
  for (name in names(x)) {
    value <- x[[name]]
    if (is.list(value)) {
      x[[name]] <- discard_all(value, fn)
    }
  }

  return(purrr::discard(x, fn))
}

# ---
map_items <- function(x, fn) {
  purrr::map2(names(x), x, fn) |>
    rlang::set_names(names(x))
}
