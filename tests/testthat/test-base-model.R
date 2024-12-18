# ---
test_that("validate lists", {
  # Prepare
  my_model <- base_model(
    a = is.numeric,
    b = is.integer,
    txt = is.character
  )

  # Act
  l <- my_model(a = 10.5, b = 20L, txt = "Hi")

  # Assert
  expect_equal(to_raw_list(l), list(a = 10.5, b = 20L, txt = "Hi"))
  expect_error(my_model(
    a = 10,
    b = 10,
    txt = "Hi"
  ))
})

# ---
test_that("validators before", {
  # Prepare
  my_model <- base_model(
    a = is.integer,
    b = is.integer,
    .validators_before = list(
      a = as.integer,
      b = as.integer
    )
  )

  # Act
  res <- my_model(
    a = 1L,
    b = 2
  )

  # Assert
  expect_equal(to_raw_list(res), list(a = 1L, b = 2L))
})

# ---
test_that("validate function arguments", {
  # Prepare
  f <- function(a, b) {
    check_args(a = is.integer, b = is.integer)
    a + b
  }

  # Act
  res <- f(2L, 4L)

  # Assert
  expect_equal(res, 6L)
  expect_error(f(2, 5L))
})

# ---
test_that("validate typed functions arguments", {
  # Prepare
  f_with_typed_args <- function(a = is.integer, b = is.integer) {
    check_args()
    a + b
  }

  # Act
  res <- f_with_typed_args(2L, 5L)

  # Assert
  expect_equal(res, 7L)
  expect_error(f(2L, 5))
})

# ---
test_that("validate data frame", {
  # Prepare
  df_to_fail <- data.frame(
    mpg = as.integer(mtcars$mpg),
    cyl = mtcars$cyl
  )

  my_model <- base_model(
    mpg = is.double,
    cyl = is.integer,
    .validators_before = list(
      cyl = as.integer
    )
  )

  # Act
  df <- my_model(.x = mtcars)
  df2 <- model_validate(mtcars, my_model)

  # Assert
  expect_equal(df, df2)
  expect_equal(names(df), c("mpg", "cyl"))
  expect_s3_class(df, "data.frame")
  expect_type(df$cyl, "integer")
  expect_error(my_model(.x = df_to_fail))
})

# ---
test_that("model field", {
  # Prepare
  my_model <- base_model(
    a = is.integer,
    b = model_field(is.integer, 10L)
  )

  # Act
  res <- my_model(a = 5L)

  # Assert
  expect_equal(res$a, 5L)
  expect_equal(res$b, 10L)
  expect_s3_class(res, CLASS_MODEL)
})

# ---
test_that("model pre init", {
  # Prepare
  df <- data.frame(
    id = 1:2,
    name = c("Donald", "Lee"),
    surname = c("Byrd", "Morgan")
  )

  df_model <- base_model(
    id = is.integer,
    name = is.character,
    surname = is.character,
    full_name = is.character,
    .model_pre_init = function(obj) {
      obj$full_name <- paste(obj$name, obj$surname)
      return(obj)
    }
  )

  # Act
  res_def <- df_model(.x = df)
  expect_equal(names(res_def), c("id", "name", "surname", "full_name"))
})

# ---
test_that("strict args order", {
  # Prepare

  # Act
  my_model <- base_model(
    a = is.integer,
    b = is.integer,
    .strict_args_order = TRUE
  )
  res <- model_to_list(my_model(4L, 2L))

  # Assert
  expect_equal(res, list(a = 4L, b = 2L))
})

# ---
test_that("discard extra fields", {
  # # Prepare
  my_model <- base_model(
    cyl = is.double,
    mpg = is.double
  )

  # Act
  res <- my_model(mtcars)

  # Assert
  expect_equal(ncol(res), 2)
})

# ---
test_that("allow extra fields", {
  # # Prepare
  my_model <- base_model(
    cyl = is.double,
    mpg = is.double,
    # .model_config = model_config(extra = "allow")
    .extra = "allow"
  )

  # Act
  res <- my_model(mtcars)

  # Assert
  expect_equal(ncol(res), 11)
})
