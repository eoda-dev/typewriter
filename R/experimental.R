# ---
# TODO: Deprecated?, use single functions as 'model_exclude_na'
model_dump_ <- function(obj,
                       exclude = NULL,
                       include = NULL,
                       exclude_na = FALSE,
                       exclude_null = FALSE,
                       by_alias = FALSE) {
  fields <- model_fields(obj)

  if (is_not_null(exclude)) {
    obj <- purrr::discard_at(obj, exclude)
  }

  if (is_not_null(include)) {
    obj <- purrr::keep_at(obj, include)
  }

  if (isTRUE(exclude_na)) {
    obj <- discard_this(obj, rlang::is_na)
  }

  if (isTRUE(exclude_null)) {
    obj <- discard_this(obj, rlang::is_null)
  }

  if (isTRUE(by_alias)) {
    obj <- dump_by_alias(obj, fields)
  } else {
    obj <- model_to_list(obj)
  }

  return(obj)
}
