library(groupdata2)
context("value_balanced_group_factor_()")

test_that("value_balanced_group_factor_() work with n=2", {

  # Create dataframe
  set.seed(1)
  df <- data.frame(
    "participant" = factor(c(1, 3, 5, 6, 7, 8)),
    "score" = c(79,85,140,69,87,92))
  vbf <- value_balanced_group_factor_(df, 2, num_col="score")
  df_vbf <- df %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf, factor(c(1,2,2,1,2,1)))
  expect_equal(group_sums$group_sum, c(240,312))

})

test_that("value_balanced_group_factor_() works with n=3", {

  # Create dataframe
  set.seed(1)
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1,2,1,1,2,3,4),
    "score" = sample(c(1:100), 7)) %>%
    dplyr::mutate(neg_score = score-200)

  # value_balanced_group_factor_ on unequal number of dataframe rows
  set.seed(1)
  expect_equal(value_balanced_group_factor_(df, 3, num_col="score"),
               factor(c(2,3,1,3,1,1,2)))

  set.seed(1)
  expect_equal(value_balanced_group_factor_(df, 3, num_col="neg_score"),
               value_balanced_group_factor_(df, 3, num_col="score"))

  # add grouping factor to df and get sums of value col
  set.seed(1)
  df_grouped <- df %>%
    dplyr::mutate(.groups = value_balanced_group_factor_(df, 3, num_col="score"))

  group_sums <- df_grouped %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(group_sums$group_sum, c(163,124,126))

  # value_balanced_group_factor_ on equal number of dataframe rows

  df <- df %>% dplyr::filter(row_number() != 7)
  set.seed(1)
  expect_equal(value_balanced_group_factor_(df, 3, num_col="score"),
               factor(c(3,1,1,2,2,3)))

  set.seed(1)
  expect_equal(value_balanced_group_factor_(df, 3, num_col="neg_score"),
               value_balanced_group_factor_(df, 3, num_col="score"))

  # add grouping factor to df and get sums of value col
  set.seed(1)
  df_grouped <- df %>%
    dplyr::mutate(.groups = value_balanced_group_factor_(df, 3, num_col="score"))

  group_sums <- df_grouped %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(group_sums$group_sum, c(94,109,113))

})


