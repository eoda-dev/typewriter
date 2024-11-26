# ---
#' Create a typed structure
#' @param ... Type definitions (Type check functions)
#' @returns A type factory function.
#' @example examples/api/typed-struct.R
#' @export
typed_struct <- function(...) {
  types <- list(...)
  base_model(types, .model_config = model_config(extra = "forbid"))
}
