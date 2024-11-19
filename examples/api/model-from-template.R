template <- list(
  a = 10L,
  b = 10L
)


my_model <- model_from_template(template)
# Succeeds
my_model(a = c(1:10), b = 7L)

# Fails
try(my_model(10, 8))


# Set values to optional
my_model <- model_from_template(template, optional = TRUE)
my_model(a = 12L) # returns NA for b

# When optional is set to FALSE the model throws an error
my_model <- model_from_template(template, optional = FALSE)
try(my_model(a = 12L)) # returns NA for b


# Use defaults if none provided
my_model <- model_from_template(template, use_defaults = TRUE, optional = FALSE)
my_model(a = 12L)

