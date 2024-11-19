my_model <- base_model(
  a = ~ is.integer(.x) & .x > 2 & .x < 4,
  b = is_optional(is.integer)
)

my_model(
  a = 5L,
  b = 4L
)

df <- data.frame(
  a = 1:5,
  b = 11:15
)

my_model(.x = df)
model_validate(df, my_model)
