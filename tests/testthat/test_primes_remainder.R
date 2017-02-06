library(groupdata2)
context("%primes%")

test_that("%primes% returns the right remainders",{

  expect_equal(57 %primes% 5, 4)
  expect_equal(57 %primes% 7, 9)
  expect_equal(63 %primes% 5, 10)

  expect_error(1 %primes% 1, "start_at is not a prime number", fixed = TRUE)
  expect_error(1 %primes% 0, "start_at >= 1 is not TRUE", fixed = TRUE)

})
