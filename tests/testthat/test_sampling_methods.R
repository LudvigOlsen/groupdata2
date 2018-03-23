library(groupdata2)
context("sampling_methods()")


test_that("balance() works with method n_ids_()", {

  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5)),
    "diagnosis" = factor(c(0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4, 1, 2, 1, 2, 3, 4),
    "score" = sample(c(1:100), 13)
  )

  set.seed(1)
  balanced_by_n_ids <- balance(df, size="max", cat_col = "diagnosis",
                              id_col = "participant",
                              id_method = "n_ids", mark_new_rows = FALSE)
  counts <- balanced_by_n_ids %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(2, 4, 4, 2, 2))
  expect_equal(counts$diagnosis, factor(c(0, 0, 0, 1, 1)))

  # Test mark_new_rows = TRUE
  set.seed(1)
  balanced_by_n_ids <- balance(df, size="max", cat_col = "diagnosis",
                               id_col = "participant",
                               id_method = "n_ids", mark_new_rows = TRUE)
  expect_equal(balanced_by_n_ids$.new_row, c(0,0,0,1,0,0,0,0,0,0,0,0,0,0))
  counts <- balanced_by_n_ids %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(2, 4, 4, 2, 2))
  expect_equal(counts$diagnosis, factor(c(0, 0, 0, 1, 1)))



})
