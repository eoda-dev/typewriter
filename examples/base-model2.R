devtools::load_all()

#check_args <- function(...) {
#  fields <- list(...)
#  if (length(fields) == 0) {
#    fn <- rlang::caller_fn()
#    fmls <- rlang::fn_fmls(fn)
#    fields <- purrr::map(as.list(fmls), eval)
#  }

#  e <- rlang::caller_env()
#  for (name in names(e)) {
#    value <- e[[name]]
#    if (is.list(value)) {
#      e[[name]] <- value$default
#    }
#  }

#  base_model2(fields)(.x = e)
#}

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
my_model2 <- base_model2(
  cyl = is.double,
  mpg = is.integer,
  .validators_before = list(
    mpg = as.integer
  )
)
my_model2(.x = tibble::as_tibble(mtcars))

my_api_model <- base_model2(
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
  model_validate(my_api_model) |> model_dump2(exclude = "address", by_alias = TRUE)

# ---


# ---
blocks <- roxygen2::parse_file("R/utils.R")
param_tags <- roxygen2::block_get_tags(blocks[[1]], "param")

purrr::map(param_tags, ~ list(n = .x$val$name, d = .x$val$description))

# roxygen2::block_has_tags(blocks)

# ---
l <- list(
  snake_here = 10,
  x = list(snake_again = 20)
)

purrr::map_depth(names(l), -1, to_camel_case)
