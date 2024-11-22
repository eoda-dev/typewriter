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
