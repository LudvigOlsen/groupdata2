library(groupdata2)
context("utilities")

test_that("seed 1 returns same numbers with runif()", {
  xpectr::set_test_seed(1)

  # In R 3.6.0 the random sampling generator was changed
  # So a lot of tests were ... wrong ...
  # Here we make a test for the future, so we will understand
  # if it happens again!
  # https://blog.revolutionanalytics.com/2019/05/whats-new-in-r-360.html
  expect_equal(
    runif(30),
    c(
      0.26550866, 0.37212390, 0.57285336, 0.90820779, 0.20168193,
      0.89838968, 0.94467527, 0.66079779, 0.62911404, 0.06178627,
      0.20597457, 0.17655675, 0.68702285, 0.38410372, 0.76984142,
      0.49769924, 0.71761851, 0.99190609, 0.38003518, 0.77744522,
      0.93470523, 0.21214252, 0.65167377, 0.12555510, 0.26722067,
      0.38611409, 0.01339033, 0.38238796, 0.86969085, 0.34034900
    )
  )
})

# TODO test assign_starts_col() helper
