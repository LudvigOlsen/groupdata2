library(groupdata2)
context("balance()")

test_that("all size settings work in balance()", {
  xpectr::set_test_seed(1)

  # Create data frame
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4),
    "score" = sample(c(1:100), 7)
  )

  # Using balance() with number
  xpectr::set_test_seed(1)
  df_3 <- balance(df, 3, "participant")

  expect_equal(nrow(df_3), 3 * 3)
  expect_equal(df_3$participant, factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3)))
  expect_equal(df_3$trial, c(1, 2, 1, 1, 1, 1, 4, 1, 2))
  expect_equal(ncol(df_3), 3)


  # Using balance() with min
  xpectr::set_test_seed(2)
  df_min <- balance(df, "min", "participant")

  expect_equal(nrow(df_min), 3)
  expect_equal(df_min$participant, factor(c(1, 2, 3)))
  expect_equal(df_min$trial, c(1, 1, 3))
  expect_equal(ncol(df_min), 3)

  # Using balance() with max
  xpectr::set_test_seed(2)
  df_max <- balance(df, "max", "participant")

  expect_equal(nrow(df_max), 4 * 3)
  expect_equal(df_max$participant, factor(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3)))
  expect_equal(df_max$trial, c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 3, 4))
  expect_equal(ncol(df_max), 3)

  # Using balance() with mean
  xpectr::set_test_seed(19)
  df_mean <- balance(df, "mean", "participant")

  expect_equal(nrow(df_mean), 2 * 3)
  expect_equal(df_mean$participant, factor(c(1, 1, 2, 2, 3, 3)))
  expect_equal(df_mean$trial, c(1, 2, 1, 1, 2, 4))
  expect_equal(ncol(df_mean), 3)

  # Using balance() with median
  xpectr::set_test_seed(19)
  df_median <- balance(df, "median", "participant")

  expect_equal(nrow(df_median), 2 * 3)
  expect_equal(df_median$participant, factor(c(1, 1, 2, 2, 3, 3)))
  expect_equal(df_median$trial, c(1, 2, 1, 1, 2, 4))
  expect_equal(ncol(df_median), 3)

  # Errors
  xpectr::set_test_seed(19)
  # Size arg
  expect_error(balance(df, "moon", "participant"),
    "'size' must be one of 'min','max','mean','median' or a whole number.",
    fixed = TRUE
  )
  expect_error(balance(df, -3, "participant"),
    "'size' must be positive when specified as a whole number.",
    fixed = TRUE
  )
  expect_error(balance(df, NULL, "participant"),
    "'size' must be one of 'min','max','mean','median' or a whole number.",
    fixed = TRUE
  )
  # id_col arg
  expect_error(balance(df, "max", "participant", id_col = "hej"),
    "'id_col' was not found in data.",
    fixed = TRUE
  )
  expect_error(balance(df %>% dplyr::mutate(participant = as.character(participant)),
    "max", "trial",
    id_col = "participant"
  ),
  "'id_col' must be a factor.",
  fixed = TRUE
  )
  expect_error(balance(df, "max", "participant", id_method = "hej"),
    "'id_method' must be one of 'n_ids', 'n_rows_c', 'distributed', and 'nested'.",
    fixed = TRUE
  )
  expect_error(balance(df, "max", "participant", mark_new_rows = NULL),
    "'mark_new_rows' must be logical (TRUE/FALSE).",
    fixed = TRUE
  )
  expect_error(balance(df, "max", "participant", mark_new_rows = NA),
    "'mark_new_rows' was NA. Must be either TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(balance(df, "max", "participant", mark_new_rows = "TRUE"),
    "'mark_new_rows' must be logical (TRUE/FALSE).",
    fixed = TRUE
  )
  expect_error(balance(df, "max", 3, mark_new_rows = TRUE),
    "'cat_col' must be the name of a column in 'data'.",
    fixed = TRUE
  )
  expect_error(balance(df, "max", NULL, mark_new_rows = TRUE),
    "'cat_col' must be the name of a column in 'data'.",
    fixed = TRUE
  )
  expect_error(balance(df, "max", NA, mark_new_rows = TRUE),
    "'cat_col' must be the name of a column in 'data'.",
    fixed = TRUE
  )
})


test_that("mark_new_rows works in balance()", {
  xpectr::set_test_seed(1)

  # Create data frame
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4),
    "score" = sample(c(1:100), 7)
  )

  xpectr::set_test_seed(1)
  df_3 <- balance(df, 3, "participant", mark_new_rows = TRUE)
  expect_equal(df_3$.new_row, c(0, 0, 1, 0, 1, 1, 0, 0, 0))

  xpectr::set_test_seed(1)
  df_3 <- balance(df, 3, "participant", mark_new_rows = TRUE, new_rows_col_name = "someName")
  expect_equal(df_3$someName, c(0, 0, 1, 0, 1, 1, 0, 0, 0))
})

test_that("both wrapper functions, upsample() and downsample() work", {
  xpectr::set_test_seed(1)

  # Create data frame
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4),
    "score" = sample(c(1:100), 7)
  )

  xpectr::set_test_seed(1)
  df_min_balance <- balance(df, "min", "participant")
  xpectr::set_test_seed(1)
  df_min_downsample <- downsample(df, "participant")

  expect_equal(df_min_balance, df_min_downsample)

  xpectr::set_test_seed(1)
  df_max_balance <- balance(df, "max", "participant")
  xpectr::set_test_seed(1)
  df_max_upsample <- upsample(df, "participant")

  expect_equal(df_max_balance, df_max_upsample)
})

test_that("balance() works in dplyr pipeline", {
  xpectr::set_test_seed(1)

  library(dplyr)
  # Create data frame
  xpectr::set_test_seed(1)
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4),
    "score" = sample(c(1:100), 7)
  ) %>%
    balance("min", "participant")

  expect_equal(nrow(df), 3)
  expect_equal(df$participant, factor(c(1, 2, 3)))
  expect_equal(df$trial, c(2, 1, 3))
  expect_equal(ncol(df), 3)
})
