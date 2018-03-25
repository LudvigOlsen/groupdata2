library(groupdata2)
context("value_balanced_group_factor_()")


test_that("value_balanced_group_factor_() works", {

  # Create dataframe
  set.seed(1)
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1,2,1,1,2,3,4),
    "score" = sample(c(1:100), 7)) %>%
    dplyr::mutate(neg_score = score-200)

  # value_balanced_group_factor_ on unequal number of dataframe rows
  set.seed(1)
  expect_equal(value_balanced_group_factor_(df, 3, val_col="score"),
               factor(c(2,3,1,3,1,1,2)))

  set.seed(1)
  expect_equal(value_balanced_group_factor_(df, 3, val_col="neg_score"),
               value_balanced_group_factor_(df, 3, val_col="score"))

  # add grouping factor to df and get sums of value col
  set.seed(1)
  df_grouped <- df %>%
    dplyr::mutate(.groups = value_balanced_group_factor_(df, 3, val_col="score"))

  group_sums <- df_grouped %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(group_sums$group_sum, c(163,124,126))

  # value_balanced_group_factor_ on equal number of dataframe rows

  df <- df %>% dplyr::filter(row_number() != 7)
  set.seed(1)
  expect_equal(value_balanced_group_factor_(df, 3, val_col="score"),
               factor(c(3,2,2,1,1,3)))

  set.seed(1)
  expect_equal(value_balanced_group_factor_(df, 3, val_col="neg_score"),
               value_balanced_group_factor_(df, 3, val_col="score"))

  # add grouping factor to df and get sums of value col
  set.seed(1)
  df_grouped <- df %>%
    dplyr::mutate(.groups = value_balanced_group_factor_(df, 3, val_col="score"))

  group_sums <- df_grouped %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(group_sums$group_sum, c(109,94,113))
})


test_that("create_rearrange_factor() works", {

  # unequal_method "middle"

  # Equal size
  expect_equal(create_rearrange_factor(40, unequal_method = "middle"), c(c(1:20), rev(c(1:20))))
  expect_equal(create_rearrange_factor(200, unequal_method = "middle"), c(c(1:100), rev(c(1:100))))

  # Unequal size
  expect_equal(create_rearrange_factor(41, unequal_method = "middle"), c(c(1:10,12:21), 11, rev(c(1:10,12:21))))
  expect_equal(create_rearrange_factor(201, unequal_method = "middle"), c(c(1:50,52:101), 51, rev(c(1:50,52:101))))

  # unequal_method "first"

  # Equal size
  expect_equal(create_rearrange_factor(40, unequal_method = "first"), c(c(1:20), rev(c(1:20))))
  expect_equal(create_rearrange_factor(200, unequal_method = "first"), c(c(1:100), rev(c(1:100))))

  # Unequal size
  expect_equal(create_rearrange_factor(41, unequal_method = "first"), c(1,c(c(1:20), rev(c(1:20)))+1))
  expect_equal(create_rearrange_factor(201, unequal_method = "first"), c(1, c(c(1:100), rev(c(1:100)))+1))
})
