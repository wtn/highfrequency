library(testthat)
set.seed(1)

data.table::setDTthreads(2)

# colCumsum ---------------------------------------------------------------
test_that("colCumsum works correctly", {
  skip_on_cran()
  x <- matrix(rnorm(100), nrow = 10, ncol = 10)
  expect_equal(colCumsum(x), apply(x, 2, cumsum))
  
})

# tickGrouping_RETURNS ----------------------------------------------------
test_that("tickGrouping_RETURNS works correctly", {
  skip_on_cran()
  expect_equal(as.numeric(tickGrouping_RETURNS(100, 3)), c(rep(0:32, each = 3), 33))
})


# KK(.) -------------------------------------------------------------------
test_that("KK cpp function works correctly", {
  skip_on_cran()
  
  x <- 1.5
  expect_equal(highfrequency:::KK(x, 1), 1-x)
  expect_equal(highfrequency:::KK(x, 2), 1 - 2 * x^3)
  expect_equal(highfrequency:::KK(x, 3), 1 - x^2)
  expect_equal(highfrequency:::KK(x, 4), 1 - 3 * x^2 + 2 * x^3)
  expect_equal(highfrequency:::KK(x, 5), 1 - 10 * x^3 + 15 * x^4 - 6 * x^5)
  expect_equal(highfrequency:::KK(x, 6), 1 - 15 * x^4 + 24 * x^5 - 10 * x^6)
  expect_equal(highfrequency:::KK(x, 7), 1 - 21 * x^5 + 35 * x^6 - 15 * x^7)
  expect_equal(highfrequency:::KK(x, 8), 1 - 28 * x^6 + 48 * x^7 - 21 * x^8)
  expect_equal(highfrequency:::KK(x, 9), 2 * (1 - x)^3)
  expect_equal(highfrequency:::KK(x-1, 9), 1 - 6 * (x-1)^2 + 6 * (x-1)^3)
  expect_equal(highfrequency:::KK(x, 10), (1 + sin(pi/2 - pi * x))/2)
  expect_equal(highfrequency:::KK(x, 11), (1 - sin(pi/2 - pi * (1 - x) * (1 - x)))/2)
  expect_equal(highfrequency:::KK(x, 12), -999)

})


# har_agg -----------------------------------------------------------------
test_that("har_agg matches the trailing-mean reference", {
  skip_on_cran()
  RM <- rnorm(100)^2
  reference <- function(RM, p) { out <- rep(NA_real_, length(RM)); for (i in p:length(RM)) out[i] <- mean(RM[(i - p + 1):i]); out }
  for (p in c(1, 5, 22, 50)) {
    expect_equal(as.numeric(highfrequency:::har_agg(RM, p, 1L)), reference(RM, p), info = paste("period =", p))
  }
})


test_that("har_agg cost should not scale with window size", {
  skip_on_cran()
  #the inner loop recomputes sum(RM[span(j-p, j-1)])/p every step, making cost O(N*p) per column instead of O(N). a running-sum impl gives O(N) per column.
  RM <- rnorm(50000)^2
  t1 <- system.time(replicate(20, highfrequency:::har_agg(RM, 5, 1L)))[3]
  t2 <- system.time(replicate(20, highfrequency:::har_agg(RM, 500, 1L)))[3]
  expect_lt(t2 / t1, 5)
})