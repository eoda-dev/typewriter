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
# TODO: Use 'structure' instead
#set_attributes <- function(x, ...) {
#  attributes(x) <- c(attributes(x), list(...))
#  return(x)
#}

# ---
#set_class <- function(x, cls = "base_model") {
#  structure(x, class = c(class(x), cls))
#}

# ---
is_not_null <- function(x) {
  isFALSE(is.null(x))
}

# ---
to_raw_list <- function(x) {
  purrr::keep_at(x, seq_along(x))
}
