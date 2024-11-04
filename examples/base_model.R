library(rlang)

my_model <- base_model(
  a = is.numeric,
  b = is.numeric,
  # txt = is_optional(rlang::is_scalar_character),
  txt = ~ rlang::is_scalar_character(.x) | is.null(.x)
)

my_model(a = 10, b = 20, txt = "Hi")

x <- list(
  a = 10,
  b = "20",
  txt = NULL
)

my_model(x)
my_model(a = 10, b = 20, txt = c("Hi", "guys"))

f <- function(a, b, txt) {
  args <- my_model(environment())
  return(args)
}

e <- f(10, 20, "yes")
e$args

# ---
another_model <- base_model(
  a = function(x) is.null(x) | is.numeric(x),
  b = is.numeric,
  txt = is.character
)

another_model(
  list(a = NULL, b = 10, txt = "Hello"),
  .drop_null = TRUE
)

f2 <- function(a, b, y = NULL) {
  validate_args(
    a = is.numeric,
    b = is.numeric,
    y = is_optional(is.numeric)
  )
  a + b
}

f2(10, 4)
f2(10, 4, "Hi")
