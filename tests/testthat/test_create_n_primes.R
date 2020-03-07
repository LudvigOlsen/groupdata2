library(groupdata2)
context("create_n_primes")


test_that("create_n_primes returns the right primes", {
  xpectr::set_test_seed(1)

  expect_equal(groupdata2:::create_n_primes(10, 5), c(5, 7, 11, 13, 17, 19, 23, 29, 31, 37))
  expect_equal(groupdata2:::create_n_primes(10, 13), c(13, 17, 19, 23, 29, 31, 37, 41, 43, 47))
  expect_equal(groupdata2:::create_n_primes(10, 47), c(47, 53, 59, 61, 67, 71, 73, 79, 83, 89))
  expect_equal(length(groupdata2:::create_n_primes(100, 12347)), 100)

  expect_error(
    xpectr::strip_msg(groupdata2:::create_n_primes(5, 1)),
    xpectr::strip("1 assertions failed:\n * 'start_at' is not a prime number."),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(groupdata2:::create_n_primes(-1, 2)),
    xpectr::strip("1 assertions failed:\n * Variable 'n': Must be >= 0."),
    fixed = TRUE)

})
