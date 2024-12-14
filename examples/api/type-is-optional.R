my_model <- base_model(
  a = "integer",
  b = optional("integer")
)

my_model(a = 10L)
