# ---
test_that("check is optional", {
  # Prepare
  my_model <- base_model(
    a = is.integer,
    b = Optional(is.integer)
  )

  # Act
  res <- my_model(a = 10L)

  # Assert
  expect_true(is.na(res$b))
})

# ---
test_that("fn from str", {
  # Prepare
  dtype <- "integer"
  dtype_scalar <- "integer:1"
  value <- 1:5

  # Act
  fn <- type_check_fn_from_str(dtype)
  fn_scalar <- type_check_fn_from_str(dtype_scalar)

  # Assert
  expect_true(fn(value))
  expect_false(fn_scalar(value))
})

# ---
test_that("fn from str in base model", {
  my_model <- base_model(
    a = "integer"
  )

  res <- my_model(a = 10L)

  expect_equal(res$a, 10L)
})
