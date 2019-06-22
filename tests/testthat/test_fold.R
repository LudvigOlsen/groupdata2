library(groupdata2)
context("fold()")

# Create more tests for fold() down the road.
# I need more examples of datasets to test it on to find
# quirky behavior
# Add tests for other methods

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

  df <- df %>% dplyr::arrange(participant)

  # Add session info
  df$session <- rep(c('1','2', '3'), 6)

  df_unequal <- df %>%
    dplyr::filter(dplyr::row_number() != 18)

  col_is_factor <- function(df, n, cat_col = NULL, num_col = NULL,
                            id_col = NULL, col, num_fold_cols=1){

    set.seed(1)
    folded_df <- fold(df, n, cat_col=cat_col, num_col=num_col,
                      id_col=id_col, num_fold_cols = num_fold_cols)
    # print(folded_df)
    return(is.factor(folded_df[[col]]))

  }

  group_counts <- function(df, n, cat_col = NULL, num_col = NULL,
                           id_col = NULL, method, num_fold_cols=1,
                           folds_col=".folds", seed=1){

    set.seed(seed)
    folded_df <- fold(df, n, cat_col=cat_col, num_col = num_col, id_col=id_col,
                      method = method, num_fold_cols=num_fold_cols)
    #print(folded_df)
    counts <- plyr::count(folded_df[[folds_col]])
    return(counts$freq)

  }

  # Check if .folds is a factor
  expect_true(col_is_factor(df, 5, col='.folds'))
  expect_true(col_is_factor(df, 5, cat_col = 'diagnosis', col='.folds'))
  expect_true(col_is_factor(df, 5, id_col = 'participant', col='.folds'))
  expect_true(col_is_factor(df, 3, cat_col = 'diagnosis',
                             id_col = 'participant',col='.folds'))
  expect_true(col_is_factor(df, 5, num_col = 'score', col='.folds'))
  expect_true(col_is_factor(df, 5, cat_col = 'diagnosis', num_col = 'score', col='.folds'))
  expect_true(col_is_factor(df, 5, id_col = 'participant', num_col = 'score', col='.folds'))
  expect_true(col_is_factor(df, 3, cat_col = 'diagnosis', num_col = 'score',
                             id_col = 'participant', col='.folds'))
  expect_true(col_is_factor(df, 3, cat_col = 'diagnosis', num_col = 'score',
                            id_col = 'participant', col='.folds_1', num_fold_cols=2))
  # Here they were identical and the .folds_2 was removed
  expect_true(!col_is_factor(df, 3, cat_col = 'diagnosis', num_col = 'score',
                            id_col = 'participant', col='.folds_2', num_fold_cols=2))


  expect_equal(group_counts(df, 5, method = 'greedy'), c(5,5,5,3))
  #expect_equal(group_counts(df, 5, cat_col = 'diagnosis', method = 'greedy'), c(6,6,6))

  expect_equal(group_counts(df, 5, method = 'n_dist'), c(3,4,3,4,4))

  expect_equal(group_counts(df, 0.2, method = 'n_dist'), c(3,3,3,3,3,3))

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

  expect_equal(group_counts(df, 2, num_col = 'score',
                            method = 'n_dist'), c(9,9))

  expect_equal(group_counts(df, 2, num_col = 'score', id_col = 'participant',
                            method = 'n_dist'), c(9,9))

  expect_equal(group_counts(df, 2, cat_col = 'diagnosis', num_col = 'score', id_col = 'participant',
                            method = 'n_dist'), c(9,9))

  expect_equal(group_counts(df, 2, num_col=NULL, id_col = 'participant',
                            method = 'n_dist', num_fold_cols = 5, folds_col = ".folds_2"), c(9,9))
  expect_equal(group_counts(df, 2, cat_col = 'diagnosis', num_col = 'score', id_col = 'participant',
                            method = 'n_dist', num_fold_cols = 2, folds_col = ".folds_1"), c(9,9))


  # Unequal number of rows in dataframe

  expect_equal(group_counts(df_unequal, 5, method = 'greedy'), c(5,5,5,2))

  expect_equal(group_counts(df_unequal, 5, method = 'n_dist'), c(3,3,4,3,4))

  expect_equal(group_counts(df_unequal, 0.2, method = 'n_dist'), c(2,3,3,3,3,3))

  expect_equal(group_counts(df_unequal, 5, cat_col = 'diagnosis',
                            method = 'n_dist'), c(2,4,3,4,4))

  expect_equal(group_counts(df_unequal, 3, cat_col = 'diagnosis',
                            id_col = 'participant',
                            method = 'n_dist'), c(6,6,5))

  expect_equal(group_counts(df_unequal, 2, cat_col = 'diagnosis',
                            id_col = 'participant',
                            method = 'n_dist'), c(6,11))

  expect_equal(group_counts(df_unequal, 3, id_col = 'participant',
                            method = 'n_dist'), c(6,6,5))

  expect_equal(group_counts(df_unequal, 2, id_col = 'participant',
                            method = 'n_dist'), c(9,8))

  expect_equal(group_counts(df_unequal, 2, num_col = 'score',
                            method = 'n_dist'), c(9,8))

  expect_equal(group_counts(df_unequal, 2, num_col = 'score', id_col = 'participant',
                            method = 'n_dist'), c(8,9))

  expect_equal(group_counts(df_unequal, 2, cat_col = 'diagnosis', num_col = 'score', id_col = 'participant',
                            method = 'n_dist'), c(9,8))


  # warning
  expect_warning(group_counts(df, 2, num_col = 'score',
                            method = 'n_rand'),
                 "'method' is ignored when 'num_col' is not NULL. This message occurs, because 'method' is not the default value.")

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


test_that("values are decently balanced in num_col in fold()",{

  set.seed(1)
  df <- data.frame("participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
                   "age" = rep(c(25,65,34), 3),
                   "diagnosis" = rep(c('a', 'b', 'a', 'a', 'b', 'b'), 3),
                   "score" = c(34,23,54,23,56,76,43,56,76,42,54,1,5,76,34,76,23,65))

  df <- df %>% dplyr::arrange(participant, score)

  # With num_col
  df_folded <- fold(df, 3, num_col = 'score')
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(267, 273, 277))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  df_folded <- fold(df, 4, num_col = 'score')
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(234,207,189, 187))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  df_folded <- fold(df, 1, num_col = 'score')
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, sum(df_folded$score))

  df_folded <- fold(df, 18, num_col = 'score')
  aggregated_scores <- df_folded %>%
    dplyr::group_by(participant, .folds) %>%
    dplyr::summarize(group_sums = sum(score)) %>%
    dplyr::arrange(participant, group_sums)

  expect_equal(aggregated_scores$group_sums, df_folded$score)

  # With num_col and id_col
  df_folded <- fold(df, 3, num_col = 'score', id_col="participant")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(283, 246, 288))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  df_folded <- fold(df, 4, num_col = 'score', id_col="participant")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(246,288,141,142))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  set.seed(1)

  # With num_col and cat_col
  df_folded <- fold(df, 3, num_col = 'score', cat_col="diagnosis")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(321, 306, 190))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  df_folded <- fold(df, 4, num_col = 'score', cat_col="diagnosis")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(222, 189, 188, 218))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  # With num_col, cat_col and id_col
  df_folded <- fold(df, 3, num_col = 'score', cat_col="diagnosis", id_col="participant")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(283, 297, 237))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  set.seed(1)
  df_folded <- fold(df, 2, num_col = 'score', cat_col="diagnosis", id_col="participant")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(378, 439))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))


})



test_that("repeated folding works in fold()",{

  set.seed(1)
  df <- data.frame("participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
                   "age" = rep(c(25,65,34), 3),
                   "diagnosis" = rep(c('a', 'b', 'a', 'a', 'b', 'b'), 3),
                   "score" = c(34,23,54,23,56,76,43,56,76,42,54,1,5,76,34,76,23,65))

  df <- df %>% dplyr::arrange(participant, score)

  # With num_col
  df_folded <- fold(df, 3, num_col = 'score', num_fold_cols=5)
  folds_colnames <- extract_fold_colnames(df_folded)
  df_folded_long <- df_folded %>%
    tidyr::gather(key="folds_col", value=".folds", folds_colnames)
  aggregated_scores <- df_folded_long %>%
    dplyr::group_by(folds_col, .folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expected_folds_col <- rep(c(".folds_1",".folds_2",".folds_3",".folds_4",".folds_5"), 18)
  expected_folds_col <- expected_folds_col[order(expected_folds_col)]
  expect_equal(df_folded_long$folds_col, expected_folds_col)

  expected_aggregated_folds_col <- rep(c(".folds_1",".folds_2",".folds_3",".folds_4",".folds_5"), 3)
  expected_aggregated_folds_col <- expected_aggregated_folds_col[order(expected_aggregated_folds_col)]
  expect_equal(aggregated_scores$folds_col, expected_aggregated_folds_col)

  set.seed(1)
  # We set num_fold_cols to a larger number than is possible to create unique .folds columns
  # Hence it will only create a smaller number of columns!
  df_folded_5reps <- fold(head(df,7), 2, num_col = 'score', num_fold_cols=10,
                          handle_existing_fold_cols = "remove")
  expect_equal(length(extract_fold_colnames(df_folded_5reps)), 5)

  # Test 10 cols
  # Also test whether all fold cols are unique (only value-wise, not group-wise (ADD group-wise test))
  df_folded_10 <- fold(df, 3, num_col = 'score', num_fold_cols=10,
                       handle_existing_fold_cols = "remove")
  folds_colnames <- extract_fold_colnames(df_folded_10)
  expect_equal(folds_colnames, paste0(".folds_",1:10))
  expect_equal(colnames(unique(as.matrix(df_folded_10), MARGIN=2)),
               c("participant","age","diagnosis","score", paste0(".folds_",1:10)))


  # system.time({
  #
  #   df_folded_100reps <- fold(df, 3, num_col = 'score', num_fold_cols=100,
  #                             max_iters = 100)
  #
  # })

})
