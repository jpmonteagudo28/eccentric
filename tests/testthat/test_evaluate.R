test_that("evaluate_data returns correct proportion for a numeric vector", {
  z <- c(-2.5, -1.5, 0, 1.5, 2.5)
  result <- evaluate_data(z, ci = 2)
  expect_type(result, "double")
  expect_equal(result, 2/5)
})

test_that("evaluate_data handles data frames and returns named row with correct proportions", {
  df <- data.frame(
    x = c(-3, 0, 3),
    y = c(-1.5, 0, 1.5)
  )
  result <- evaluate_data(df, ci = 2)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_named(result, c("x", "y"))
  expect_equal(result$x, 2/3)
  expect_equal(result$y, 0)
})

test_that("evaluate_data ignores NAs when computing proportions", {
  z <- c(-2.5, NA, 2.5)
  result <- evaluate_data(z, ci = 2)
  expect_equal(result, 2/2)
})

test_that("evaluate_data throws error for non-numeric or invalid inputs", {
  expect_error(evaluate_data(data = "string", ci = 2))
  expect_error(evaluate_data(data = list(1, 2, 3), ci = 2))
  expect_error(evaluate_data(data = c(1, 2, 3), ci = "high"))
  expect_error(evaluate_data(data = c(1, 2, 3), ci = c(1.96, 2.58)))
})

test_that("evaluate_data correctly handles all values within or outside CI", {
  expect_equal(evaluate_data(c(-1, 0, 1), ci = 2), 0)  # all within
  expect_equal(evaluate_data(c(-3, -2.5, 2.5, 3), ci = 2), 1)  # all outside
})

test_that("evaluate_data works on large data and doesn't return NA", {
  set.seed(123)
  z <- rnorm(1e4)
  result <- evaluate_data(z, ci = 1.96)
  expect_type(result, "double")
  expect_true(result > 0 && result < 1)
})

test_that("evaluate_data with all NA vector returns error", {
  z <- c(NA, NA, NA)  # Logical vector
  expect_error(evaluate_data(z, ci = 2),
               regexp = "(is.numeric\\(data\\) || is.data.frame\\(data\\)) is not TRUE")
})

test_that("evaluate_data with all NA vector returns error", {
  z <- c(NA_real_, NA_real_, NA_real_)  # Numeric vector
  result <- evaluate_data(z, ci = 1.96)
  expect_true(is.nan(result) || is.na(result))
})

test_that("evaluate_data with mixed NA and numeric in data frame returns correct proportions", {
  df <- data.frame(a = c(NA, NA, 3), b = c(-3, 0, NA))
  result <- evaluate_data(df, ci = 2)
  expect_equal(result$a, 1)  # only one non-NA, which is > 2
  expect_equal(result$b, 0.5)  # one of two non-NA is > 2
})
