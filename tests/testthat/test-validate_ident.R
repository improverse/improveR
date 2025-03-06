test_that("validate_ident returns NULL for empty character string", {
  expect_null(validate_ident(""))
})

test_that("validate_ident returns NULL for empty list", {
  expect_null(validate_ident(list()))
})

test_that("validate_ident returns NULL for empty data frame", {
  expect_null(validate_ident(data.frame()))
})

test_that("validate_ident returns NULL for NULL input", {
  expect_null(validate_ident(NULL))
})

test_that("validate_ident returns NULL for NA input", {
  expect_null(validate_ident(NA))
})

test_that("validate_ident returns input for non-empty character string", {
  expect_equal(validate_ident("non-empty"), "non-empty")
})

test_that("validate_ident returns input for non-empty list", {
  expect_equal(validate_ident(list(1, 2, 3)), list(1, 2, 3))
})

test_that("validate_ident returns input for non-empty data frame", {
  df <- data.frame(a = 1:3)
  expect_equal(validate_ident(df), df)
})




