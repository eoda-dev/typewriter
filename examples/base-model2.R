
f <- function(a, b = 80L) {
  check_args(a = is.integer, b = is.integer)
  a + b
}

f(2L)

f2 <- function(aa = is.numeric, bb = model_field(is.integer, 10L)) {
  check_args()
  aa + bb
}

f2(5)

# ---
my_model <- base_model(
  cyl = is.double,
  mpg = is.integer,
  .validators_before = list(
    mpg = as.integer
  )
)
my_model(.x = tibble::as_tibble(mtcars))

my_api_model <- base_model(
  name = is.character,
  age = is.integer,
  address = is.list,
  .model_config = model_config(str_to_lower = TRUE)
)

external_data <- list(
  name = "PETER",
  age = 10L,
  address = list(
    city = "KASSEL",
    country = "Germany"
  )
)

my_api_model(.x = external_data)

external_data |>
  model_validate(my_api_model) |>
  model_dump(by_alias = TRUE)

