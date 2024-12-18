# ---
validate_fields <- function(obj, validators) {
  for (name in names(validators)) {
    obj[[name]] <- rlang::as_function(validators[[name]])(obj[[name]])
  }

  return(obj)
}

# ---
str_to_lower <- function(x) {
  ifelse(is.character(x), tolower(x), x)
}

# ---
numeric_to_integer <- function(x) {
  ifelse(is.numeric(x), as.integer(x), x)
}

# ---
is_not_null <- function(x) {
  isFALSE(is.null(x))
}

# ---
to_raw_list <- function(x) {
  # purrr::keep_at(x, seq_along(x))
  x[seq_along(x)]
}

# ---
map_items <- function(x, fn) {
  rlang::set_names(Map(fn, names(x), x), names(x))
  # purrr::map2(names(x), x, fn) |>
  #   rlang::set_names(names(x))
}

# ---
discard_this <- function(x, fn = rlang::is_na) {
  for (name in names(x)) {
    value <- x[[name]]
    if (is.list(value)) {
      x[[name]] <- discard_this(value, fn)
    }
  }

  return(x[!unlist(Map(fn, x))])
}

# ---
str_to_camel_case <- function(strings) {
  gsub("[_\\.](\\w?)", "\\U\\1", strings, perl = TRUE)
}

# ---
str_to_snake_case <- function(strings) {
  strings <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2", strings)))
  gsub("\\.", "", strings)
}

# ---
mutate_names <- function(x, fn, .recursive = TRUE) {
  for (name in names(x)) {
    value <- x[[name]]
    new_name <- fn(name)
    if (is.list(value) & isTRUE(.recursive)) {
      value <- mutate_names(value, fn)
    }
    x[[name]] <- NULL
    x[[new_name]] <- value
  }

  return(x)
}

# ---
#' Convert names of an object to camel case
#' @param obj A list or a data frame.
#' @param .recursive Whether to convert names on all levels.
#' @returns The input object with camel case names.
#' @export
names_to_camel_case <- function(obj, .recursive = TRUE) {
  mutate_names(obj, str_to_camel_case, .recursive)
}

# ---
#' Convert names of an object to snake case
#' @inheritParams names_to_camel_case
#' @returns The input object with snake case names.
#' @export
names_to_snake_case <- function(obj, .recursive = TRUE) {
  mutate_names(obj, str_to_snake_case, .recursive)
}

# ---
dump_by_alias <- function(obj, fields = NULL) {
  if (is.null(fields)) {
    fields <- model_fields(obj)
  }

  l <- list()
  for (name in names(obj)) {
    alias <- fields[[name]]$alias
    value <- obj[[name]]
    new_name <- ifelse(is.null(alias), name, alias)
    if (inherits(value, CLASS_MODEL)) {
      l[[new_name]] <- dump_by_alias(value)
    } else {
      # l[[new_name]] <- value
      l[[new_name]] <- ifelse(is.null(value), list(NULL), value)
    }
  }

  return(l)
}

# ---
model_to_list <- function(obj) {
  l <- list()
  for (name in names(obj)) {
    value <- obj[[name]]
    if (is.list(value)) {
      l[[name]] <- model_to_list(value)
    } else {
      l[[name]] <- unclass(value)
    }
  }
  return(l)
}


# ---
map_depth_base <- function(.x, .depth, .f) {
  if (!is.list(.x)) {
    stop(".x must be a list.")
  }

  if (.depth == 0) {
    # At depth 0, apply the function to the entire list
    return(.f(.x))
  } else if (.depth == 1) {
    return(lapply(.x, .f))
  } else {
    # Recurse deeper into the list
    return(lapply(.x, function(element) {
      if (is.list(element)) {
        map_depth_base(element, .depth - 1, .f)
      } else {
        .f(element) # Keep non-list elements unchanged
      }
    }))
  }
}

# ---
assign_values <- function(x, ...) {
  l <- list(...)
  for (name in names(l)) {
    value <- l[[name]]
    x[[name]] <- ifelse(is.null(value), list(NULL), value)
  }

  x
}

# ---
assign_value <- function(x, name, value) {
  x[[name]] <- ifelse(is.null(value), list(NULL), value)
  x
}
