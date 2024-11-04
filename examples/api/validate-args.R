f <- function(a, b) {
  validate_args(
    a = is.integer,
    b = is.integer
  )
  a + b
}

f(2L, 5L)

try(f(2, 5))
