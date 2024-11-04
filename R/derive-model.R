# ---
#' Create a model factory function from a template object
#' @param template A template list to derive model from.
#' @param use_defaults Whether to use template values as default values.
#' @param use_length **not used** at the moment
#' @inherit base_model return
#' @example examples/api/model-from-template.R
#' @export
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
