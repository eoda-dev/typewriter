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
str_to_camel_case <- function(strings) {
  gsub("_(\\w?)", "\\U\\1", strings, perl = TRUE)
}

# ---
str_to_snake_case <- function(strings) {
  gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2", strings)))
}
