f <- function(a = is.integer, b = is.integer) {
  validate_fn()
  a + b
}

f(2L, 5L)

try(f(2, 5))
