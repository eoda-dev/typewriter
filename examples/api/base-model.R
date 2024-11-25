# ---
# Models
my_model <- base_model(
  a = is.integer,
  b = is.integer,
  txt = is.character
)

# Succeeds
my_model(a = 1L, b = 2L, txt = "My awesome model")

# Fails
if (FALSE)
  try(my_model(a = 1, b = 2L, txt = "My awesome model"))

# ---
# Functions
f <- function(a, b) {
  check_args(a = is.integer, b = is.integer)
  a + b
}

# Succeeds
f(4L, 5L)

# Fails
if (FALSE)
  try(f(4, 5))

# ---
# Data frames
df <- data.frame(
  id = 1:2,
  name = c("Donald", "Lee"),
  surname = c("Byrd", "Morgan")
)

df_model <- base_model(
  id = is.integer,
  name = is.character,
  surname = is.character,
  full_name = is.character,
  .model_pre_init = function(obj) {
    obj$full_name <- paste(obj$name, obj$surname)
    return(obj)
  }
)

# Succeeds
df_model(.x = df)

# Fails
df$id <- NULL
if (FALSE)
  try(df_model(.x = df))
