# ---
test_that("snakes to camels", {
  # Prepare
  l <- list(
    snake_case = "yes",
    camelCase = "yes",
    isSnakeCase = "no",
    is_camel_case = list(more_snakes = "yeah")
  )

  # Act
  l_camels <- names_to_camel_case(l)

  # Assert
  expect_equal(names(l_camels), c("snakeCase", "camelCase", "isSnakeCase", "isCamelCase"))
  expect_equal(l_camels$isCamelCase$moreSnakes, "yeah")
})

# ---
test_that("camels to snakes", {
  # Prepare
  from_json <- list(
    convertMeToSnakes = "okay",
    this_is_snake_case = "yes"
  )

  # Act
  res <- names_to_snake_case(from_json)

  # Assert
  expect_equal(res, list(convert_me_to_snakes = "okay", this_is_snake_case = "yes"))
})

# ---
test_that("discard recursive NA", {
  # Prepare
  l <- list(
    a = 10,
    b = NA,
    x = list(
      aa = 20,
      bb = NA
    )
  )

  # Act
  res <- discard_this(l, rlang::is_na)

  # Assert
  expect_equal(res, list(a = 10, x = list(aa = 20)))
})

# ---
test_that("discard recursive NULL", {
  # Prepare
  l <- list(
    a = 10,
    b = NULL,
    x = list(
      aa = 20,
      bb = NULL
    )
  )

  # Act
  res <- discard_this(l, is.null)

  # Assert
  expect_equal(res, list(a = 10, x = list(aa = 20)))
})
