my_model <- base_model(
  a = is.numeric,
  b = is.numeric,
  .validators = list(
    b = function(x) x * 10
  )
)

my_model(a = 10, b = 20)

f <- function(a, b) {
  my_model()
  a + b
}

f("1", 4)


f3 <- function(a # is.numeric
               , b # is.numeric
) {}

k <- attr(f3, "srcref") |>
  as.character() |>
  paste(collapse = "") |>
  stringr::str_remove_all(" ") |>
  stringr::str_extract_all("\\([^()]+\\)")

substring(k[[1]], 2, nchar(k[[1]]) - 1) |> stringr::str_split(",")
