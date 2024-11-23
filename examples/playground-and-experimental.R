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

# ---
df <- data.frame(
  id = 1:3,
  value = c(11L, NA, 14L)
)

my_model_df <- base_model(
  id = is_integer,
  value = is_integer,
  .strict_args_order = TRUE
)

my_model_df(.x = df)
my_model_df(df)

is_integer(all(df$value))

# ---
my_model_simple <- base_model(
  a = "integer",
  txt = "character:2"
)

my_model_simple(a = 10L, txt = c("Hi", "there"))

f_test <- function(a = "integer:1", b = "integer") {
  check_args()
  a + b
}
