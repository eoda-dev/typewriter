# ---
#' Create a typed structure
#' @param ... Type definitions (Type check functions)
#' @export
typed_struct <- function(...) {
  types <- list(...)
  base_model(types, .model_config = model_config(extra = "forbid"))
}
