library(groupdata2)
context("fold()")

# Create more tests for fold() down the road.
# I need more examples of datasets to test it on to find
# quirky behavior
# Add tests for other methods

test_that("dimensions of data frame with fold()",{

  set_seed_for_R_compatibility(1)

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


test_that("errors and warnings are correct with fold()",{

  set_seed_for_R_compatibility(1)

  df <- data.frame("participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
                   "age" = rep(c(25,65,34), 3),
                   "diagnosis" = rep(c('a', 'b', 'a', 'a', 'b', 'b'), 3),
                   "score" = c(34,23,54,23,56,76,43,56,76,42,54,1,5,76,34,76,23,65))

  df <- df[order(df$participant),]

  # Add session info
  df$session <- rep(c('1','2', '3'), 6)

  # methods
  expect_error(fold(df, 5, method="l_sizes"), "method 'l_sizes' is not supported by fold().", fixed=TRUE)
  expect_error(fold(df, 5, method="l_starts"), "method 'l_starts' is not supported by fold().", fixed=TRUE)
  expect_error(fold(df, 5, method="primes"), "method 'primes' is not supported by fold().", fixed=TRUE)

  # k
  expect_error(fold(df,k = c(5,4)), "'k' must be numeric scalar.", fixed=TRUE)
  expect_error(fold(df,k = c(-3,4)), "'k' must be numeric scalar.", fixed=TRUE)
  expect_error(fold(df,k = -3), "'k' must be positive.", fixed=TRUE)

  # handle_existing_fold_cols
  expect_error(fold(df,k = 5, handle_existing_fold_cols = "naa"),
               "Please specify handle_existing_fold_cols as either 'keep_warn','keep', or 'remove'.", fixed=TRUE)
  expect_error(fold(df,k = 5, handle_existing_fold_cols = NULL),
               "Please specify handle_existing_fold_cols as either 'keep_warn','keep', or 'remove'.", fixed=TRUE)
  expect_error(fold(df,k = 5, handle_existing_fold_cols = NA),
               "Please specify handle_existing_fold_cols as either 'keep_warn','keep', or 'remove'.", fixed=TRUE)
  expect_error(fold(df,k = 5, handle_existing_fold_cols = character()),
               "Please specify handle_existing_fold_cols as either 'keep_warn','keep', or 'remove'.", fixed=TRUE)
  df$.folds_1 <- 1
  df$.folds_2 <- 1
  expect_warning(fold(df,k = 5, handle_existing_fold_cols = "keep_warn"),
               "Found 2 existing fold columns. These will NOT be replaced. Change 'handle_existing_fold_cols' to 'remove' if you want to replace them.", fixed=TRUE)

})

test_that(".folds is correct in fold()",{

  set_seed_for_R_compatibility(1)

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

    set_seed_for_R_compatibility(1)
    folded_df <- fold(df, n, cat_col=cat_col, num_col=num_col,
                      id_col=id_col, num_fold_cols = num_fold_cols)
    # print(folded_df)
    return(is.factor(folded_df[[col]]))

  }

  group_counts <- function(df, n, cat_col = NULL, num_col = NULL,
                           id_col = NULL, method, num_fold_cols=1,
                           folds_col=".folds", seed=1){

    set_seed_for_R_compatibility(seed)
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


  # Unequal number of rows in data frame

  expect_equal(group_counts(df_unequal, 5, method = 'greedy'), c(5,5,5,2))

  expect_equal(group_counts(df_unequal, 5, method = 'n_dist'), c(3,3,4,3,4))

  expect_equal(group_counts(df_unequal, 0.2, method = 'n_dist'), c(2,3,3,3,3,3))

  expect_equal(group_counts(df_unequal, 5, cat_col = 'diagnosis',
                            method = 'n_dist'), c(2,4,3,4,4))

  expect_equal(group_counts(df_unequal, 3, cat_col = 'diagnosis',
                            id_col = 'participant',
                            method = 'n_dist'), c(6,5,6))

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
                 "'method' is ignored when 'num_col' is not NULL. This warning occurs, because 'method' is not the default value.")

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

  set_seed_for_R_compatibility(1)

  set_seed_for_R_compatibility(1)
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

  expect_equal(aggregated_scores$group_sums, c(285, 264, 268))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  df_folded <- fold(df, 4, num_col = 'score')
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(207,225,196, 189))
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

  expect_equal(aggregated_scores$group_sums, c(288, 246, 283))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  df_folded <- fold(df, 4, num_col = 'score', id_col="participant")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(283,246,133,155))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  set_seed_for_R_compatibility(1)

  # With num_col and cat_col
  df_folded <- fold(df, 3, num_col = 'score', cat_col="diagnosis")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(268, 285, 264))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  df_folded <- fold(df, 4, num_col = 'score', cat_col="diagnosis")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(207, 202, 199, 209))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  # With num_col, cat_col and id_col
  df_folded <- fold(df, 3, num_col = 'score', cat_col="diagnosis", id_col="participant")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(237, 283, 297))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  set_seed_for_R_compatibility(1)
  df_folded <- fold(df, 2, num_col = 'score', cat_col="diagnosis", id_col="participant")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(378, 439))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))


})



test_that("repeated folding works in fold()",{

  set_seed_for_R_compatibility(1)
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

  set_seed_for_R_compatibility(1)
  # We set num_fold_cols to a larger number than is possible to create unique .folds columns
  # Hence it will only create a smaller number of columns!
  df_folded_5reps <- fold(head(df,7), 2, num_col = 'score', num_fold_cols=10,
                          handle_existing_fold_cols = "remove")
  expect_equal(length(extract_fold_colnames(df_folded_5reps)), 5)

  # Test 10 cols
  # Also test whether all fold cols are unique
  df_folded_10 <- fold(df, 3, num_col = 'score', num_fold_cols=10,
                       handle_existing_fold_cols = "remove")
  folds_colnames <- extract_fold_colnames(df_folded_10)
  expect_equal(folds_colnames, paste0(".folds_",1:10))
  expect_equal(colnames(unique(as.matrix(df_folded_10), MARGIN=2)),
               c("participant","age","diagnosis","score", paste0(".folds_",1:10)))
  expect_equal(colnames(unique(as.matrix(df_folded_10), MARGIN=2)),
               c("participant","age","diagnosis","score", paste0(".folds_",1:10)))
  # Test group-wise uniqueness
  column_combinations <- as.data.frame(t(combn(paste0(".folds_",1:10), 2)), stringsAsFactors=FALSE)
  column_combinations[["identical"]] <- plyr::llply(1:nrow(column_combinations), function(r){
    col_1 <- df_folded_10[[column_combinations[r, 1]]]
    col_2 <- df_folded_10[[column_combinations[r, 2]]]
    return(all_groups_identical(col_1, col_2))
  }) %>% unlist()
  expect_true(all(!column_combinations$identical))


  # system.time({
  #
  #   df_folded_100reps <- fold(df, 3, num_col = 'score', num_fold_cols=100,
  #                             max_iters = 100)
  #
  # })

})


test_that("bootstrap test of num_col works",{

  # Takes 4 seconds, so we disable it for now.
  testthat::skip(message = "Skipping bootstrapped numerical balancing test in fold()")

  df <- data.frame("participant"=factor(rep(1:100, 100)),
                   "diagnosis"=factor(rep(c("a","b","c","d","e"), 2000)),
                   "age"=rep(sample(100),100))

  # Single
  set_seed_for_R_compatibility(1)
  df_folded <- fold(df, 3, num_col="age")

  for (i in 1:10){

    set_seed_for_R_compatibility(i)
    df_folded <- fold(df, 0.5, cat_col="diagnosis", num_col="age",
                      id_col="participant")

    age_distribution <- df_folded %>% group_by(.folds) %>%
      dplyr::summarise(mean_age = mean(age),
                       sd_age = sd(age))

    expect_true(is_between_(age_distribution$mean_age[1], 50, 52))
    expect_true(is_between_(age_distribution$mean_age[2], 49, 51))

  }

  for (i in 1:10){

    set_seed_for_R_compatibility(i)
    df_folded <- fold(df, 5, cat_col="diagnosis", num_col="age",
                      id_col="participant")

    age_distribution <- df_folded %>% group_by(.folds) %>%
      dplyr::summarise(mean_age = mean(age),
                       sd_age = sd(age))

    expect_true(is_between_(age_distribution$mean_age[1], 47.5, 53.5))
    expect_true(is_between_(age_distribution$mean_age[2], 47.5, 53.5))
    expect_true(is_between_(age_distribution$mean_age[3], 47.5, 53.5))
    expect_true(is_between_(age_distribution$mean_age[4], 47.5, 53.5))
    expect_true(is_between_(age_distribution$mean_age[5], 47.5, 53.5))

  }

  # With two levels of extreme pairing

  for (i in 1:10){

    set_seed_for_R_compatibility(i)
    df_folded <- fold(df, 5,
                      cat_col="diagnosis", num_col="age",
                      id_col="participant", extreme_pairing_levels = 2)

    age_distribution <- df_folded %>% group_by(.folds) %>%
      dplyr::summarise(mean_age = mean(age),
                       sd_age = sd(age))

    expect_true(is_between_(age_distribution$mean_age[1], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[2], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[3], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[4], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[5], 49, 51.5))

  }

  # With three levels of extreme pairing

  for (i in 1:10){

    set_seed_for_R_compatibility(i)
    expect_error(fold(df, 5,
                      cat_col="diagnosis", num_col="age",
                      id_col="participant", extreme_pairing_levels = 3),
                 "data is too small to perform 3 levels of extreme pairing")

    df_folded <- fold(df, 5,
                      cat_col="diagnosis", num_col="age", extreme_pairing_levels = 3)

    age_distribution <- df_folded %>% group_by(.folds) %>%
      dplyr::summarise(mean_age = mean(age),
                       sd_age = sd(age))

    expect_true(is_between_(age_distribution$mean_age[1], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[2], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[3], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[4], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[5], 49, 51.5))

  }

  for (i in 1:10){

    set_seed_for_R_compatibility(i)
    df_folded <- fold(df, i,
                      cat_col="diagnosis", num_col="age", extreme_pairing_levels = 1)

    age_distribution <- df_folded %>% group_by(.folds) %>%
      dplyr::summarise(mean_age = mean(age),
                       sd_age = sd(age))

    expect_true(is_between_(min(age_distribution$mean_age), 49, 51.5))
    expect_true(is_between_(max(age_distribution$mean_age), 49, 51.5))

    set_seed_for_R_compatibility(i)
    df_folded <- fold(df, i,
                      cat_col="diagnosis", num_col="age", extreme_pairing_levels = 2)

    age_distribution <- df_folded %>% group_by(.folds) %>%
      dplyr::summarise(mean_age = mean(age),
                       sd_age = sd(age))

    expect_true(is_between_(min(age_distribution$mean_age), 49, 51.5))
    expect_true(is_between_(max(age_distribution$mean_age), 49, 51.5))

    set_seed_for_R_compatibility(i)
    df_folded <- fold(df, i,
                      cat_col="diagnosis", num_col="age", extreme_pairing_levels = 3)

    age_distribution <- df_folded %>% group_by(.folds) %>%
      dplyr::summarise(mean_age = mean(age),
                       sd_age = sd(age))

    expect_true(is_between_(min(age_distribution$mean_age), 49, 51.5))
    expect_true(is_between_(max(age_distribution$mean_age), 49, 51.5))

  }



  # set_seed_for_R_compatibility(47)
  # # With four levels of extreme pairing
  # df_folded <- fold(df, 5,
  #                   cat_col="diagnosis", num_col="age",
  #                   extreme_pairing_levels = 4)
  #
  # age_distribution <- df_folded %>% group_by(.folds) %>%
  #   dplyr::summarise(mean_age = mean(age),
  #                    sd_age = sd(age))

})


