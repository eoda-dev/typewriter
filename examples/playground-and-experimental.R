# ---
library(rlang)
devtools::load_all()

# ---
# Check if nulls are kept

my_model <- base_model(
  a = is_any,
  b = is_character
)

my_model(a = NULL, b = "Keep NULL values")

# ---
# Optional

my_model2 <- base_model(
  a = is_optional(is.integer),
  b = is_optional(is.integer, allow_null = TRUE),
  txt = is_character
)

my_model2(txt = "allow NA only", b = NULL)
