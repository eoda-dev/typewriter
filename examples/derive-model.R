template <- list(
  a_character = "hello",
  a_model = lm(mpg ~ cyl, data = mtcars),
  a_dataframe = mtcars,
  a_number = 12,
  an_integer = 12L
)


my_model <- derive_model(template)

# This works
my_model(
  an_integer = 12L,
  a_character = "test",
  a_dataframe = iris,
  a_model = lm(Sepal.Width ~ Petal.Width, data = iris),
  a_number = 7
)

# This throws an error
try(
  my_model(
    an_integer = 12,
    a_character = "test",
    a_dataframe = iris,
    a_model = lm(Sepal.Width ~ Petal.Width, data = iris),
    a_number = 7
  )
)
