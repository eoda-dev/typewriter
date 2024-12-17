m <- typed_struct(
  a = either(is.integer, is.null),
  b = either("integer", "logical")
)

# Succeeds
m(a = 10L, b = TRUE)

# Also succeeds
m(a = NULL, b = 10L)
