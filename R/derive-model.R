# ---
# TODO: deprecated
derive_model <- function(template) {
  fields <- purrr::map(template, function(v) {
    body <- substitute(
      {
        typeof(x) == vtype & class(x) == vclass & mode(x) == vmode
      },
      list(vtype = typeof(v), vclass = class(v), vmode = mode(v))
    )
    rlang::new_function(alist(x = ), body = body)
  })
  return(base_model(fields))
}

# ---
model_from_template <- function(template,
                                use_defaults = FALSE,
                                use_length = FALSE) {
  fields <- purrr::map(template, function(v) {
    body <- substitute(
      {
        typeof(x) == vtype & class(x) == vclass & mode(x) == vmode
      },
      list(vtype = typeof(v), vclass = class(v), vmode = mode(v))
    )
    type_check_fn <- rlang::new_function(alist(x = ), body = body)
    return(model_field(
      fn = type_check_fn,
      default = ifelse(isTRUE(use_defaults), v, NA)
    ))
  })
  return(base_model(fields))
}
