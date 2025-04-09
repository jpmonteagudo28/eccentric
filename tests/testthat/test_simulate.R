# test-simulate_clt.R

test_that("simulate_clt returns a data frame with correct dimensions", {
  result <- simulate_clt(n = c(10, 20), inverse = "rnorm", samples = 100)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 100)
})

test_that("simulate_clt accepts additional arguments for inverse function", {
  result <- simulate_clt(n = c(10), inverse = "rbeta", shape1 = 2, shape2 = 5)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 1)
})

test_that("simulate_clt throws error on non-numeric or invalid `n`", {
  expect_error(simulate_clt(n = c(0, 1), inverse = "rnorm"),
               "n must be an atomic vector of integers greater than 1")
  expect_error(simulate_clt(n = "ten", inverse = "rnorm"))
})

test_that("simulate_clt throws error when optional args are non-numeric", {
  expect_error(simulate_clt(n = c(10), inverse = "rnorm", mean = "zero"))
  expect_error(simulate_clt(n = c(10), inverse = "rnorm", sd = TRUE))
})

test_that("simulate_clt throws error on unsupported inverse function", {
  expect_error(simulate_clt(n = c(10), inverse = "foobar"))
})

test_that("simulate_clt handles missing samples arg with default", {
  result <- simulate_clt(n = c(5), inverse = "rnorm")
  expect_equal(nrow(result), 249)
})

test_that("simulate_clt produces standardized values", {
  result <- simulate_clt(n = c(30), inverse = "rnorm", samples = 50)
  col_mean <- mean(result[[1]])
  col_sd <- sd(result[[1]])
  expect_true(abs(col_mean) < 0.35) # z-scores should be roughly centered at 0
  expect_true(abs(col_sd - 1) < 0.35)
})

test_that("simulate_clt works with inverse functions requiring extra args", {
  result <- simulate_clt(n = c(25), inverse = "rgamma", shape = 2, rate = 1)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 1)
})

test_that("simulate_clt handles multiple sample sizes and outputs correctly named columns", {
  result <- simulate_clt(n = c(10, 50, 100), inverse = "rnorm")
  expect_equal(colnames(result), c("10", "50", "100"))
})
