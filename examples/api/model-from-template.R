template <- list(
  a = 10L,
  b = 10L,
  txt = "Some Text"
)

my_model <- model_from_template(template, use_defaults = TRUE)

my_model(a = 20L)
