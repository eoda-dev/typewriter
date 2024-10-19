# ---
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
