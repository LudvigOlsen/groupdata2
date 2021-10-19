library(groupdata2)
context("auto_tune_collapsings")


test_that("testing find_best_group_cols_()", {

  # Set seed
  xpectr::set_test_seed(42)

  # Create data frame
  df <- data.frame(
    "participant" = factor(rep(1:20, 3)),
    "age" = rep(sample(c(1:100), 20), 3),
    "diagnosis" = factor(rep(sample(c(1:3), 20, replace = TRUE), 3)),
    "score" = sample(c(1:100), 20 * 3)
  )
  df <- df %>% dplyr::arrange(participant)

  # Sample rows to get unequal sizes per participant
  df <- dplyr::sample_n(df, size = 27)

  # Create the initial groups (to be collapsed)
  df <- fold(
    data = df,
    k = 8,
    num_fold_cols = 5,
    method = "n_dist",
    id_col = "participant"
  )

  # Ungroup the data frame
  df <- dplyr::ungroup(df)

  best_cols <- find_best_group_cols_(
    data = df,
    num_new_group_cols = 3,
    group_cols_names = paste0(".folds_", 1:5),
    cat_cols = "diagnosis",
    num_cols = "age",
    id_cols = "participant",
    weights = c(
      "diagnosis" = 3,
      "age" = 5,
      "participant" = 2
    ),
    balance_size = TRUE
  )

  expect_equal(
    best_cols,
    c(".folds_4", ".folds_5", ".folds_1"),
    fixed = TRUE
  )

  # zero-variance
  df$.man_folds = 1

  best_cols <- find_best_group_cols_(
    data = df,
    num_new_group_cols = 2,
    group_cols_names = c(".folds_1", ".man_folds"),
    cat_cols = "diagnosis",
    num_cols = "age",
    id_cols = "participant",
    weights = c(
      "diagnosis" = 3,
      "age" = 5,
      "participant" = 2
    ),
    balance_size = TRUE
  )

  # .man_folds give NAs in ranked_balances
  # so it should rank last
  expect_equal(
    best_cols,
    c(".folds_1", ".man_folds")
  )

})
