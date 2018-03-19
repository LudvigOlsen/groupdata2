library(groupdata2)
context("balance()")

test_that("all size settings work in balance()",{

  # Create dataframe
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1,2,1,1,2,3,4),
    "score" = sample(c(1:100), 7))

  # Using balance() with number
  set.seed(1)
  df_3 <- balance(df, 3, "participant")

  expect_equal(nrow(df_3), 3*3)
  expect_equal(df_3$participant, factor(c(1,1,1,2,2,2,3,3,3)))
  expect_equal(df_3$trial, c(1,2,1,1,1,1,4,1,2))
  expect_equal(ncol(df_3), 3)


  # Using balance() with min
  set.seed(1)
  df_min <- balance(df, "min", "participant")

  expect_equal(nrow(df_min), 3)
  expect_equal(df_min$participant, factor(c(1,2,3)))
  expect_equal(df_min$trial, c(1,1,2))
  expect_equal(ncol(df_min), 3)

  # Using balance() with max
  set.seed(1)
  df_max <-balance(df, "max", "participant")

  expect_equal(nrow(df_max), 4*3)
  expect_equal(df_max$participant, factor(c(1,1,1,1,2,2,2,2,3,3,3,3)))
  expect_equal(df_max$trial, c(1,2,1,1,1,1,1,1,1,2,3,4))
  expect_equal(ncol(df_max), 3)

  # Using balance() with mean
  set.seed(1)
  df_mean <-balance(df, "mean", "participant")

  expect_equal(nrow(df_mean), 2*3)
  expect_equal(df_mean$participant, factor(c(1,1,2,2,3,3)))
  expect_equal(df_mean$trial, c(1,2,1,1,2,4))
  expect_equal(ncol(df_mean), 3)

  # Using balance() with median
  set.seed(1)
  df_median <-balance(df, "median", "participant")

  expect_equal(nrow(df_median), 2*3)
  expect_equal(df_median$participant, factor(c(1,1,2,2,3,3)))
  expect_equal(df_median$trial, c(1,2,1,1,2,4))
  expect_equal(ncol(df_median), 3)

})

test_that("both wrapper functions, upsample() and downsample() work",{

  # Create dataframe
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1,2,1,1,2,3,4),
    "score" = sample(c(1:100), 7))

  set.seed(1)
  df_min_balance <- balance(df, "min", "participant")
  set.seed(1)
  df_min_downsample <- downsample(df, "participant")

  expect_equal(df_min_balance, df_min_downsample)

  set.seed(1)
  df_max_balance <- balance(df, "max", "participant")
  set.seed(1)
  df_max_upsample <- upsample(df, "participant")

  expect_equal(df_max_balance, df_max_upsample)

})
