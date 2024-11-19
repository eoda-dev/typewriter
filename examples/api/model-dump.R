my_model <- base_model(
  this_is_a_character = is.character,
  this_is_an_integer = is.integer,
  this_value_can_be_null = ~ is.integer(.x) | is.null(.x)
)

model_dump(
  obj = my_model(
    this_is_a_character = "yes",
    this_is_an_integer = 10L,
    this_value_can_be_null = NULL
  ), exclude_null = TRUE, camels = TRUE)
