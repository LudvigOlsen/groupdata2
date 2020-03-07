library(groupdata2)
context("%primes%")

test_that("%primes% returns the right remainders", {
  expect_equal(57 %primes% 5, 4)
  expect_equal(57 %primes% 7, 9)
  expect_equal(63 %primes% 5, 10)
  expect_equal(77 %primes% 2, 0)

  expect_error(
    xpectr::strip_msg(1 %primes% 1),
    xpectr::strip("1 assertions failed:\n * 'start_at' must be smaller than 'size'."),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(1 %primes% 0),
    xpectr::strip("1 assertions failed:\n * Variable 'start_at': Must be >= 1."),
    fixed = TRUE)
})
