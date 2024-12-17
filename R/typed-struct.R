# ---
#' Create a typed structure
#' @param ... Type definitions.
#' @param .allow_na Allow values to be initialized with `NA`.
#' @returns A type factory function.
#' @example examples/api/typed-struct.R
#' @export
typed_struct <- function(..., .allow_na = FALSE) {
  types <- list(...)
  base_model(types, .extra = "forbid", .allow_na = .allow_na)
}
