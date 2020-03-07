library(groupdata2)
context("%staircase%")


test_that("%staircase% returns the right remainders", {
  expect_equal(57 %staircase% 5, 7)
  expect_equal(57 %staircase% 8, 9)
  expect_equal(63 %staircase% 5, 13)
  expect_equal(1 %staircase% 1, 0)

  expect_error(
    xpectr::strip_msg(1 %staircase% 0),
    xpectr::strip("1 assertions failed:\n * Variable 'step_size': Must be >= 1."),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(0 %staircase% 1),
    xpectr::strip("missing value where TRUE/FALSE needed"),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(-1 %staircase% 20),
    xpectr::strip("1 assertions failed:\n * Variable 'size': Must be >= 0."),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg("A" %staircase% 20),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'size': Must be of type '",
                         "count', not 'character'.")),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(3 %staircase% "A"),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'step_size': Must be of t",
                         "ype 'count', not 'character'.")),
    fixed = TRUE)


})
