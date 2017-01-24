library(groupdata2)
context("fold()")

# Create more tests for fold() down the road.
# I need more examples of datasets to test it on to find
# quirky behavior

test_that("dimensions of dataframe with fold()",{

  df <- data.frame("participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
                   "age" = rep(c(25,65,34), 3),
                   "diagnosis" = rep(c('a', 'b', 'a', 'a', 'b', 'b'), 3),
                   "score" = c(34,23,54,23,56,76,43,56,76,42,54,1,5,76,34,76,23,65))

  df <- df[order(df$participant),]

  # Add session info
  df$session <- rep(c('1','2', '3'), 6)

  # The added grouping factor means we should get and extra column
  expect_equal(ncol(fold(df, 5)), 6)

  # We expect the same amount of rows
  expect_equal(nrow(fold(df, 5)), 18)


})

test_that(".folds is correct in fold()",{

  df <- data.frame("participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
                   "age" = rep(c(25,65,34), 3),
                   "diagnosis" = rep(c('a', 'b', 'a', 'a', 'b', 'b'), 3),
                   "score" = c(34,23,54,23,56,76,43,56,76,42,54,1,5,76,34,76,23,65))

  df <- df[order(df$participant),]

  # Add session info
  df$session <- rep(c('1','2', '3'), 6)

  col_is_factor <- function(df, n, cat_col = NULL, id_col = NULL, col){

    folded_df <- fold(df, n, cat_col=cat_col, id_col=id_col)

    return(is.factor(folded_df[[col]]))

  }



  group_counts <- function(df, n, cat_col = NULL, id_col = NULL, method){

    folded_df <- fold(df, n, cat_col=cat_col, id_col=id_col, method = method)
    counts <- plyr::count(folded_df$.folds)
    return(counts$freq)

  }

  # Check if .folds is a factor
  expect_equal(col_is_factor(df, 5, col='.folds'), TRUE)
  expect_equal(col_is_factor(df, 5, cat_col = 'diagnosis', col='.folds'), TRUE)
  expect_equal(col_is_factor(df, 5, id_col = 'participant', col='.folds'), TRUE)
  expect_equal(col_is_factor(df, 3, cat_col = 'diagnosis',
                             id_col = 'participant',col='.folds'), TRUE)

  expect_equal(group_counts(df, 5, method = 'greedy'), c(5,5,5,3))
  #expect_equal(group_counts(df, 5, cat_col = 'diagnosis', method = 'greedy'), c(6,6,6))


  expect_equal(group_counts(df, 5, method = 'n_dist'), c(3,4,3,4,4))

  expect_equal(group_counts(df, 0.2, method = 'n_dist'), c(6,6,6))

  expect_equal(group_counts(df, 5, cat_col = 'diagnosis',
                            method = 'n_dist'), c(2,4,4,4,4))

  expect_equal(group_counts(df, 3, cat_col = 'diagnosis',
                            id_col = 'participant',
                            method = 'n_dist'), c(6,6,6))

  expect_equal(group_counts(df, 2, cat_col = 'diagnosis',
                            id_col = 'participant',
                            method = 'n_dist'), c(6,12))

  expect_equal(group_counts(df, 3, id_col = 'participant',
                            method = 'n_dist'), c(6,6,6))

  expect_equal(group_counts(df, 2, id_col = 'participant',
                            method = 'n_dist'), c(9,9))

  # Staircase
  expect_equal(group_counts(df, 2,
                            method = 'staircase'), c(2,4,6,6))

  expect_equal(group_counts(df, 2, id_col = 'participant',
                            method = 'staircase'), c(6,12))

  expect_equal(group_counts(df, 2, id_col = 'participant',
                            cat_col = 'diagnosis',
                            method = 'staircase'), c(6,12))

  expect_equal(group_counts(df, 2, cat_col = 'diagnosis',
                            method = 'staircase'), c(2,4,6,6))

})

