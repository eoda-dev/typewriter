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
is_not_null <- function(x) {
  isFALSE(is.null(x))
}

# ---
to_raw_list <- function(x) {
  purrr::keep_at(x, seq_along(x))
}

# ---
map_items <- function(x, fn) {
  purrr::map2(names(x), x, fn) |>
    rlang::set_names(names(x))
}

# ---
discard_this <- function(x, fn = rlang::is_na) {
  for (name in names(x)) {
    value <- x[[name]]
    if (is.list(value)) {
      x[[name]] <- discard_this(value, fn)
    }
  }

  return(purrr::discard(x, fn))
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
#' @param obj A list or a data frame.
#' @returns The input object with snake case names.
#' @export
names_to_snake_case <- function(obj, .recursive = TRUE) {
  mutate_names(obj, str_to_snake_case, .recursive)
}
