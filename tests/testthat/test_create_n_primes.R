context("create_n_primes")


test_that("create_n_primes returns the right primes",{

  expect_equal(groupdata2:::create_n_primes(10,5), c(5,7,11,13,17,19,23,29,31,37))
  expect_equal(groupdata2:::create_n_primes(10,13), c(13,17,19,23,29,31,37,41,43,47))
  expect_equal(groupdata2:::create_n_primes(10,47), c(47,53,59,61,67,71,73,79,83,89))
  expect_equal(length(groupdata2:::create_n_primes(100,12347)), 100)

  expect_error(groupdata2:::create_n_primes(5,1), "start_at is not a prime number", fixed = TRUE)
  expect_error(groupdata2:::create_n_primes(-1,2), "n > 1 is not TRUE", fixed = TRUE)


})

