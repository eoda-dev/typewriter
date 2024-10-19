library(httr2)
library(lubridate)

api_url <- "https://cat-fact.herokuapp.com/facts"

status_model <- base_model(
  verified = is.logical,
  sent_count = is.integer
)

cat_facts_model <- base_model(
  text = is.character,
  created_at = is.character,
  updated_at = is.character,
  # status = ~ is.list(status_model(.x)),
  status = is_another_model(status_model),
  .validators_after = list(
    created_at = as_datetime,
    updated_at = as_datetime
  )
)

resp <- request(api_url) |>
  req_perform() |>
  resp_body_json()

(cat_facts <- resp[[1]] |>
  keys_to_snake_case() |>
  cat_facts_model())
