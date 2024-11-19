my_model <- base_model(
  a = is.integer,
  b = is_optional(is.integer)
)

my_model(a = 10L)
