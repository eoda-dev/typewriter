library(rlang)

my_model <- base_model(
  a = is.numeric,
  b = is.numeric,
  txt = ~ rlang::is_scalar_character(.x) | is.null(.x)
)

my_model(a = 10, b = 20, txt = "Hi")

x <- list(
  a = 10,
  b = "20",
  txt = NULL
)

my_model(.x = x)
my_model(a = 10, b = 20, txt = c("Hi", "guys"))

f <- function(a, b, txt) {
  model_validate(environment(), my_model)
  return(a + b)
}

f(10, 20, "yes")
