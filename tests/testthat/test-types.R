test_that("check is optional", {
  # Prepare
  my_model <- base_model(
    a = is.integer,
    b = is_optional(is.integer)
  )

  # Act
  res <- my_model(a = 10L)

  # Assert
  expect_true(is.na(res$b))
})
