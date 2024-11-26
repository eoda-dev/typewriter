cfg <- model_config(str_to_lower = TRUE)

my_model <- base_model(
  bar = is.character,
  foo = is.character,
  .model_config = cfg
)

my_model(bar = "FOO", foo = "BAR")
