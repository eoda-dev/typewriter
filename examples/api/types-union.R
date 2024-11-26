m <- typed_struct(
  a = Union(is.integer, is.null),
  b = Union("integer", "logical")
)

# Succeeds
m(a = 10L, b = TRUE)

# Also succeeds
m(a = NULL, b = 10L)
