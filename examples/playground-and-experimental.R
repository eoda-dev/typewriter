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

# --- Optional

my_mod_opt <- base_model(
  a = Optional(is.integer),
  b = "integer",
  txt = model_field("character:1", "hello")
)

my_mod_opt(a = 2L, b = 2L, txt = 10)

mtcars_model <- base_model(
  mpg = "double:32",
  cyl = "integer:32",
  gear = "integer:32",
  .validators_before = list(
    cyl = as.integer,
    gear = as.integer
  ),
  .model_config = model_config(extra = "forbid")
)

mtcars_model(mtcars)

# --- dtype
mm <- base_model(
  a = dtype("optional:integer", 10L)
)

mm(a = mtcars)

f_x <- function(a = dtype("integer", 20L)) {
  check_args()
  a + 1
}

f_x()

# ---
if (FALSE) {
  print("Hi")
}

# --- Union
my_model <- base_model(
  a = Union(is.integer, is.na)
)

my_model(a = 10)

# ---
my_struct <- typed_struct(
  a = is.integer,
  b = Optional(is.character)
)

my_struct(a = 10L, txt = "Hi")

library(rlang)

my_type = typed_struct(
  a = Union("integer", "character"),
  b = "character"
)

my_type(r = 1L)
