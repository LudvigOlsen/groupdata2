library(groupdata2)
context("%staircase%")


test_that("%staircase% returns the right remainders",{

  expect_equal(57 %staircase% 5, 7)
  expect_equal(57 %staircase% 8, 9)
  expect_equal(63 %staircase% 5, 13)
  expect_equal(1 %staircase% 1, 0)

  expect_error(1 %staircase% 0, "step_size > 0 is not TRUE", fixed = TRUE)

})
