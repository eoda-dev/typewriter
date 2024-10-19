to_camel_case <- function(x) {
  gsub("_(\\w?)", "\\U\\1", x, perl = TRUE)
}

keys_to_camel_case_DEPRECATED <- function(x) {
  if (is.null(x)) {
    return(x)
  }

  stats::setNames(x, to_camel_case(names(x)))
}

#' Convert keys to camel case
#' @param x list
#' @param .recursive Whether to convert keys on all levels.
#' @export
keys_to_camel_case <- function(x, .recursive = TRUE) {
  if (is.null(x)) {
    return(x)
  }

  l <- list()
  for (k in names(x)) {
    value <- x[[k]]
    k <- to_camel_case(k)
    if (is.list(value) & isTRUE(.recursive)) {
      l[[k]] <- keys_to_camel_case(value)
    } else {
      l[[k]] <- value
    }
  }

  return(l)
}

# ---
camels_to_snakes <- function(strings) {
  gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2", strings)))
}

keys_to_snake_case_DEPRECATED <- function(x) {
  stats::setNames(x, camels_to_snakes(names(x)))
}

# TODO: Rename to 'names_to_snakes'
#' Convert keys to snake case
#' @param x list
#' @param .recursive Whether to convert keys on all levels.
#' @export
keys_to_snake_case <- function(x, .recursive = TRUE) {
  l <- list()
  for (k in names(x)) {
    value <- x[[k]]
    k <- camels_to_snakes(k)
    if (is.list(value) & isTRUE(.recursive)) {
      l[[k]] <- keys_to_snake_case(value)
    } else {
      l[[k]] <- value
    }
  }

  return(l)
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
discard_this2 <- function(x, fn = rlang::is_na) {
  l <- list()
  for (name in names(x)) {
    value <- x[[name]]
    if (is.list(value)) {
      l[[name]] <- discard_this(value, fn)
    } else {
      l[[name]] <- value
    }
  }

  return(purrr::discard(l, fn))
}
