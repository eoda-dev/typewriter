json_str <- '{
  "statusCode": 200,
  "df": [
    {"aA": 1, "b": 2.2},
    {"aA": 4, "b": 4.5}
  ]
}'

df_model <- base_model(
  a_a = is.integer,
  b = is.double
)

my_model <- base_model(
  status_code = is.integer,
  df = ~ is.data.frame(df_model(.x = .x))
)

jsonlite::fromJSON(json_str) |>
  names_to_snake_case() |>
  model_validate(my_model)
