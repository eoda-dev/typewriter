my_model <- base_model(
  a = "integer",
  b = Optional("integer")
)

my_model(a = 10L)
