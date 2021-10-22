library(groupdata2)
context("fold()")

# Create more tests for fold() down the road.
# I need more examples of datasets to test it on to find
# quirky behavior
# Add tests for other methods

test_that("dimensions of data frame with fold()", {
  xpectr::set_test_seed(1)

  df <- data.frame(
    "participant" = factor(rep(c("1", "2", "3", "4", "5", "6"), 3)),
    "age" = rep(c(25, 65, 34), 3),
    "diagnosis" = factor(rep(c("a", "b", "a", "a", "b", "b"), 3)),
    "score" = c(34, 23, 54, 23, 56, 76, 43, 56, 76, 42, 54, 1, 5, 76, 34, 76, 23, 65)
  )

  df <- df[order(df$participant), ]

  # Add session info
  df$session <- rep(c("1", "2", "3"), 6)

  # The added grouping factor means we should get and extra column
  expect_equal(ncol(fold(df, 5)), 6)

  # We expect the same amount of rows
  expect_equal(nrow(fold(df, 5)), 18)
})

test_that("errors and warnings are correct with fold()", {
  xpectr::set_test_seed(1)

  df <- data.frame(
    "participant" = factor(rep(c("1", "2", "3", "4", "5", "6"), 3)),
    "age" = rep(c(25, 65, 34), 3),
    "diagnosis" = factor(rep(c("a", "b", "a", "a", "b", "b"), 3)),
    "score" = c(34, 23, 54, 23, 56, 76, 43, 56, 76, 42, 54, 1, 5, 76, 34, 76, 23, 65)
  )

  df <- df[order(df$participant), ]

  # Add session info
  df$session <- rep(c("1", "2", "3"), 6)

  # methods

  expect_error(
    xpectr::strip_msg(fold(df, 5, method = "l_sizes")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'method': Must be a subset",
                         " of set {n_dist,n_fill,n_last,n_rand,greedy,staircase}.")),
    fixed = TRUE)
  expect_error(
    xpectr::strip_msg(fold(df, 5, method = "l_starts")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'method': Must be a subset",
                         " of set {n_dist,n_fill,n_last,n_rand,greedy,staircase}.")),
    fixed = TRUE)
  expect_error(
    xpectr::strip_msg(fold(df, 5, method = "primes")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'method': Must be a subset",
                         " of set {n_dist,n_fill,n_last,n_rand,greedy,staircase}.")),
    fixed = TRUE)

  # k
  expect_error(
    xpectr::strip_msg(fold(df, k = c(5, 4))),
    xpectr::strip("1 assertions failed:\n when length(k) > 1 it must have precisely num_fold_cols elements"),
    fixed = TRUE)
  expect_error(
    xpectr::strip_msg(fold(df, k = c(-3, 4))),
    xpectr::strip("1 assertions failed:\n * Variable 'k': Element 1 is not > 0."),
    fixed = TRUE)
  expect_error(
    xpectr::strip_msg(fold(df, k = -3)),
    xpectr::strip("1 assertions failed:\n * Variable 'k': Element 1 is not >= 0."),
    fixed = TRUE)


  # handle_existing_fold_cols

  expect_error(
    xpectr::strip_msg(fold(df, k = 5, handle_existing_fold_cols = "naa")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'handle_existing_fold_cols",
                         "': Must be a subset of set {keep_warn,keep,remove}.")),
    fixed = TRUE)
  expect_error(
    xpectr::strip_msg(fold(df, k = 5, handle_existing_fold_cols = NULL)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'handle_existing_fold_cols",
                         "': Must be of type 'string', not 'NULL'.")),
    fixed = TRUE)
  expect_error(
    xpectr::strip_msg(fold(df, k = 5, handle_existing_fold_cols = NA)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'handle_existing_fold_cols",
                         "': May not be NA.")),
    fixed = TRUE)
  expect_error(
    xpectr::strip_msg(fold(df, k = 5, handle_existing_fold_cols = character())),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'handle_existing_fold_cols",
                         "': Must have length 1.")),
    fixed = TRUE)

  df$.folds_1 <- 1
  df$.folds_2 <- 1
  expect_warning(fold(df, k = 5, handle_existing_fold_cols = "keep_warn"),
    "Found 2 existing fold columns. These will NOT be replaced. Change 'handle_existing_fold_cols' to 'remove' if you want to replace them or 'keep' to remove the warning.",
    fixed = TRUE
  )
})

test_that(".folds is correct in fold()", {
  xpectr::set_test_seed(1)

  df <- data.frame(
    "participant" = factor(rep(c("1", "2", "3", "4", "5", "6"), 3)),
    "age" = rep(c(25, 65, 34), 3),
    "diagnosis" = factor(rep(c("a", "b", "a", "a", "b", "b"), 3)),
    "score" = c(34, 23, 54, 23, 56, 76, 43, 56, 76, 42, 54, 1, 5, 76, 34, 76, 23, 65)
  )

  df <- df %>% dplyr::arrange(participant)

  # Add session info
  df$session <- rep(c("1", "2", "3"), 6)

  df_unequal <- df %>%
    dplyr::filter(dplyr::row_number() != 18)

  col_is_factor <- function(df, n, cat_col = NULL, num_col = NULL,
                            id_col = NULL, col, num_fold_cols = 1) {
    xpectr::set_test_seed(1)
    folded_df <- fold(df, n,
      cat_col = cat_col, num_col = num_col,
      id_col = id_col, num_fold_cols = num_fold_cols
    )
    # print(folded_df)
    return(is.factor(folded_df[[col]]))
  }

  group_counts <- function(df, n, cat_col = NULL, num_col = NULL,
                           id_col = NULL, method, num_fold_cols = 1,
                           folds_col = ".folds", seed = 1) {
    xpectr::set_test_seed(seed)
    folded_df <- fold(df, n,
      cat_col = cat_col, num_col = num_col, id_col = id_col,
      method = method, num_fold_cols = num_fold_cols
    )
    # print(folded_df)
    counts <- plyr::count(folded_df[[folds_col]])
    return(counts$freq)
  }

  # Check if .folds is a factor
  expect_true(col_is_factor(df, 5, col = ".folds"))
  expect_true(col_is_factor(df, 5, cat_col = "diagnosis", col = ".folds"))
  expect_true(col_is_factor(df, 5, id_col = "participant", col = ".folds"))
  expect_true(col_is_factor(df, 3,
    cat_col = "diagnosis",
    id_col = "participant", col = ".folds"
  ))
  expect_true(col_is_factor(df, 5, num_col = "score", col = ".folds"))
  expect_true(col_is_factor(df, 5, cat_col = "diagnosis", num_col = "score", col = ".folds"))
  expect_true(col_is_factor(df, 5, id_col = "participant", num_col = "score", col = ".folds"))
  expect_true(col_is_factor(df, 3,
    cat_col = "diagnosis", num_col = "score",
    id_col = "participant", col = ".folds"
  ))
  expect_true(col_is_factor(df, 3,
    cat_col = "diagnosis", num_col = "score",
    id_col = "participant", col = ".folds_1", num_fold_cols = 2
  ))
  expect_true(col_is_factor(df, 3,
    cat_col = "diagnosis", num_col = "score",
    id_col = "participant", col = ".folds_2", num_fold_cols = 2
  ))


  expect_equal(group_counts(df, 5, method = "greedy"), c(5, 5, 5, 3))
  # expect_equal(group_counts(df, 5, cat_col = 'diagnosis', method = 'greedy'), c(6,6,6))

  expect_equal(group_counts(df, 5, method = "n_dist"), c(3, 4, 3, 4, 4))

  expect_equal(group_counts(df, 0.2, method = "n_dist"), c(3, 3, 3, 3, 3, 3))

  expect_equal(group_counts(df, 5,
    cat_col = "diagnosis",
    method = "n_dist"
  ), c(2, 4, 4, 4, 4))

  expect_equal(group_counts(df, 3,
    cat_col = "diagnosis",
    id_col = "participant",
    method = "n_dist"
  ), c(6, 6, 6))

  expect_equal(group_counts(df, 2,
    cat_col = "diagnosis",
    id_col = "participant",
    method = "n_dist"
  ), c(6, 12))

  expect_equal(group_counts(df, 3,
    id_col = "participant",
    method = "n_dist"
  ), c(6, 6, 6))

  expect_equal(group_counts(df, 2,
    id_col = "participant",
    method = "n_dist"
  ), c(9, 9))

  expect_equal(group_counts(df, 2,
    num_col = "score",
    method = "n_dist"
  ), c(9, 9))

  expect_equal(group_counts(df, 2,
    num_col = "score", id_col = "participant",
    method = "n_dist"
  ), c(9, 9))

  expect_equal(group_counts(df, 2,
    cat_col = "diagnosis", num_col = "score", id_col = "participant",
    method = "n_dist"
  ), c(9, 9))

  expect_equal(group_counts(df, 2,
    num_col = NULL, id_col = "participant",
    method = "n_dist", num_fold_cols = 5, folds_col = ".folds_2"
  ), c(9, 9))
  expect_equal(group_counts(df, 2,
    cat_col = "diagnosis", num_col = "score", id_col = "participant",
    method = "n_dist", num_fold_cols = 2, folds_col = ".folds_1"
  ), c(9, 9))


  # Unequal number of rows in data frame

  expect_equal(group_counts(df_unequal, 5, method = "greedy"), c(5, 5, 5, 2))

  expect_equal(group_counts(df_unequal, 5, method = "n_dist"), c(3, 3, 4, 3, 4))

  expect_equal(group_counts(df_unequal, 0.2, method = "n_dist"), c(2, 3, 3, 3, 3, 3))

  expect_equal(group_counts(df_unequal, 5,
    cat_col = "diagnosis",
    method = "n_dist"
  ), c(2, 4, 3, 4, 4))

  expect_equal(group_counts(df_unequal, 3,
    cat_col = "diagnosis",
    id_col = "participant",
    method = "n_dist"
  ), c(6, 5, 6))

  expect_equal(group_counts(df_unequal, 2,
    cat_col = "diagnosis",
    id_col = "participant",
    method = "n_dist"
  ), c(6, 11))

  expect_equal(group_counts(df_unequal, 3,
    id_col = "participant",
    method = "n_dist"
  ), c(6, 6, 5))

  expect_equal(group_counts(df_unequal, 2,
    id_col = "participant",
    method = "n_dist"
  ), c(9, 8))

  expect_equal(group_counts(df_unequal, 2,
    num_col = "score",
    method = "n_dist"
  ), c(9, 8))

  expect_equal(group_counts(df_unequal, 2,
    num_col = "score", id_col = "participant",
    method = "n_dist"
  ), c(8, 9))

  expect_equal(group_counts(df_unequal, 2,
    cat_col = "diagnosis", num_col = "score", id_col = "participant",
    method = "n_dist"
  ), c(9, 8))


  # warning
  expect_warning(
    group_counts(df, 2,
      num_col = "score",
      method = "n_rand"
    ),
    "'method' is ignored when 'num_col' is not 'NULL'. This warning occurs, because 'method' is not the default value."
  )

  # Staircase
  expect_equal(group_counts(df, 2,
    method = "staircase"
  ), c(2, 4, 6, 6))

  expect_equal(group_counts(df, 2,
    id_col = "participant",
    method = "staircase"
  ), c(6, 12))

  expect_equal(group_counts(df, 2,
    id_col = "participant",
    cat_col = "diagnosis",
    method = "staircase"
  ), c(6, 12))

  expect_equal(group_counts(df, 2,
    cat_col = "diagnosis",
    method = "staircase"
  ), c(2, 4, 6, 6))
})

test_that("values are decently balanced in num_col in fold()", {

  xpectr::set_test_seed(1)
  df <- data.frame(
    "participant" = factor(rep(c("1", "2", "3", "4", "5", "6"), 3)),
    "age" = rep(c(25, 65, 34), 3),
    "diagnosis" = factor(rep(c("a", "b", "a", "a", "b", "b"), 3)),
    "score" = c(34, 23, 54, 23, 56, 76, 43, 56, 76, 42, 54, 1, 5, 76, 34, 76, 23, 65)
  )

  df <- df %>% dplyr::arrange(participant, score)

  # With num_col
  df_folded <- fold(df, 3, num_col = "score")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(285, 264, 268))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  df_folded <- fold(df, 4, num_col = "score")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(207, 225, 196, 189))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  df_folded <- fold(df, 1, num_col = "score")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, sum(df_folded$score))

  df_folded <- fold(df, 18, num_col = "score")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(participant, .folds) %>%
    dplyr::summarize(group_sums = sum(score)) %>%
    dplyr::arrange(participant, group_sums)

  expect_equal(aggregated_scores$group_sums, df_folded$score)

  # With num_col and id_col
  df_folded <- fold(df, 3, num_col = "score", id_col = "participant")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(288, 283, 246))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  df_folded <- fold(df, 4, num_col = "score", id_col = "participant")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(283, 288, 82, 164))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  xpectr::set_test_seed(1)

  # With num_col and cat_col
  df_folded <- fold(df, 3, num_col = "score", cat_col = "diagnosis")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(268, 285, 264))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  df_folded <- fold(df, 4, num_col = "score", cat_col = "diagnosis")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(207, 202, 199, 209))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  # With num_col, cat_col and id_col
  df_folded <- fold(df, 3, num_col = "score", cat_col = "diagnosis", id_col = "participant")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(237, 283, 297))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))

  xpectr::set_test_seed(1)
  df_folded <- fold(df, 2, num_col = "score", cat_col = "diagnosis", id_col = "participant")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expect_equal(aggregated_scores$group_sums, c(378, 439))
  expect_equal(sum(aggregated_scores$group_sums), sum(df_folded$score))
})

test_that("num_col works with multiple cat_col strings in fold()", {

  xpectr::set_test_seed(1)
  df <- data.frame(
    "cat_a" = factor(rep(c("a", "b", "c"), each=10)),
    "cat_b" = factor(rep(rep(c("a", "b"), each=5), 3)),
    "num" = runif(30)
  )

  # With num_col
  df_folded <- fold(df, 3, cat_col=c("cat_a", "cat_b"), num_col = "num")
  aggregated_scores <- df_folded %>%
    dplyr::group_by(.folds) %>%
    dplyr::summarize(num = sum(num))

  # Testing column values
  expect_equal(
    aggregated_scores[[".folds"]],
    structure(1:3, .Label = c("1", "2", "3"), class = "factor"))
  expect_equal(
    aggregated_scores[["num"]],
    c(5.1269665339496, 5.57825285824947, 4.58135238196701),
    tolerance = 1e-4)

})

test_that("repeated folding works in fold()", {
  xpectr::set_test_seed(1)
  df <- data.frame(
    "participant" = factor(rep(c("1", "2", "3", "4", "5", "6"), 3)),
    "age" = rep(c(25, 65, 34), 3),
    "diagnosis" = factor(rep(c("a", "b", "a", "a", "b", "b"), 3)),
    "score" = c(34, 23, 54, 23, 56, 76, 43, 56, 76, 42, 54, 1, 5, 76, 34, 76, 23, 65)
  )

  df <- df %>% dplyr::arrange(participant, score)

  # With num_col
  df_folded <- fold(df, 3, num_col = "score", num_fold_cols = 5)
  folds_colnames <- extract_fold_colnames(df_folded)
  df_folded_long <- df_folded %>%
    tidyr::gather(key = "folds_col", value = ".folds", folds_colnames)
  aggregated_scores <- df_folded_long %>%
    dplyr::group_by(folds_col, .folds) %>%
    dplyr::summarize(group_sums = sum(score))

  expected_folds_col <- rep(c(".folds_1", ".folds_2", ".folds_3", ".folds_4", ".folds_5"), 18)
  expected_folds_col <- expected_folds_col[order(expected_folds_col)]
  expect_equal(df_folded_long$folds_col, expected_folds_col)

  expected_aggregated_folds_col <- rep(c(".folds_1", ".folds_2", ".folds_3", ".folds_4", ".folds_5"), 3)
  expected_aggregated_folds_col <- expected_aggregated_folds_col[order(expected_aggregated_folds_col)]
  expect_equal(aggregated_scores$folds_col, expected_aggregated_folds_col)

  xpectr::set_test_seed(1)
  # We set num_fold_cols to a larger number than is possible to create unique .folds columns
  # Hence it will only create a smaller number of columns!
  df_folded_5reps <- fold(head(df, 7), 2,
    num_col = "score", num_fold_cols = 10,
    handle_existing_fold_cols = "remove"
  )
  expect_equal(length(extract_fold_colnames(df_folded_5reps)), 5)

  # Test 10 cols
  # Also test whether all fold cols are unique
  df_folded_10 <- fold(df, 3,
    num_col = "score", num_fold_cols = 10,
    handle_existing_fold_cols = "remove"
  )
  folds_colnames <- extract_fold_colnames(df_folded_10)
  expect_equal(folds_colnames, paste0(".folds_", 1:10))
  expect_equal(
    colnames(unique(as.matrix(df_folded_10), MARGIN = 2)),
    c("participant", "age", "diagnosis", "score", paste0(".folds_", 1:10))
  )
  expect_equal(
    colnames(unique(as.matrix(df_folded_10), MARGIN = 2)),
    c("participant", "age", "diagnosis", "score", paste0(".folds_", 1:10))
  )
  # Test group-wise uniqueness
  column_combinations <- as.data.frame(t(combn(paste0(".folds_", 1:10), 2)),
    stringsAsFactors = FALSE
  )
  column_combinations[["identical"]] <- plyr::llply(
    seq_len(nrow(column_combinations)),
    function(r) {
      col_1 <- df_folded_10[[column_combinations[r, 1]]]
      col_2 <- df_folded_10[[column_combinations[r, 2]]]
      return(all_groups_identical(col_1, col_2))
    }
  ) %>% unlist()
  expect_true(all(!column_combinations$identical))


  # system.time({
  #
  #   df_folded_100reps <- fold(df, 3, num_col = 'score', num_fold_cols=100,
  #                             max_iters = 100)
  #
  # })
})

test_that("bootstrap test of num_col works", {

  # Takes 4 seconds, so we disable it for now.
  # testthat::skip(message = "Skipping bootstrapped numerical balancing test in fold()")

  xpectr::set_test_seed(1)
  df <- data.frame(
    "participant" = factor(rep(1:100, 100)),
    "diagnosis" = factor(rep(c("a", "b", "c", "d", "e"), 2000)),
    "age" = rep(sample(100), 100)
  )

  # Single ####

  xpectr::set_test_seed(1)
  df_folded <- fold(df, 3, num_col = "age")

  mean_ages <- plyr::ldply(1:10, function(i){
    xpectr::set_test_seed(i)
    df_folded <- fold(df, 0.5,
      cat_col = "diagnosis", num_col = "age",
      id_col = "participant"
    )

    age_distribution <- df_folded %>%
      dplyr::group_by(.folds) %>%
      dplyr::summarise(
        mean_age = mean(age),
        sd_age = sd(age)
      )

    age_distribution$mean_age
  })


  ## Testing 'mean_ages'                                                    ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(mean_ages),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    mean_ages[["V1"]],
    c(50.6, 50.42, 50.52, 50.62, 50.6, 50.54, 50.56, 50.68, 50.2, 50.48),
    tolerance = 1e-4)
  expect_equal(
    mean_ages[["V2"]],
    c(50.4, 50.58, 50.48, 50.38, 50.4, 50.46, 50.44, 50.32, 50.8, 50.52),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(mean_ages),
    c("V1", "V2"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(mean_ages),
    c("numeric", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(mean_ages),
    c("double", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(mean_ages),
    c(10L, 2L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(mean_ages)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'mean_ages'                                           ####

  mean_ages <- plyr::ldply(1:10, function(i){
    xpectr::set_test_seed(i)
    df_folded <- fold(df, 5,
      cat_col = "diagnosis",
      num_col = "age",
      id_col = "participant"
    )

    age_distribution <- df_folded %>%
      dplyr::group_by(.folds) %>%
      dplyr::summarise(
        mean_age = mean(age),
        sd_age = sd(age)
      )

    age_distribution$mean_age
  })


  ## Testing 'mean_ages'                                                    ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(mean_ages),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    mean_ages[["V1"]],
    c(49.4, 51.2, 50.9, 51.5, 50, 49.2, 50.15, 50.2, 49.5, 51.8),
    tolerance = 1e-4)
  expect_equal(
    mean_ages[["V2"]],
    c(49.7, 49.5, 50.45, 51.4, 49.75, 51.15, 51.05, 49.9, 49, 49.15),
    tolerance = 1e-4)
  expect_equal(
    mean_ages[["V3"]],
    c(52.6, 50.4, 49.7, 50, 51, 50.05, 50.35, 52.65, 51.45, 50.05),
    tolerance = 1e-4)
  expect_equal(
    mean_ages[["V4"]],
    c(50.55, 50.35, 50.9, 50.05, 50.55, 50.8, 50.75, 50.45, 50, 50.85),
    tolerance = 1e-4)
  expect_equal(
    mean_ages[["V5"]],
    c(50.25, 51.05, 50.55, 49.55, 51.2, 51.3, 50.2, 49.3, 52.55, 50.65),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(mean_ages),
    c("V1", "V2", "V3", "V4", "V5"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(mean_ages),
    c("numeric", "numeric", "numeric", "numeric", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(mean_ages),
    c("double", "double", "double", "double", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(mean_ages),
    c(10L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(mean_ages)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'mean_ages'                                           ####


  # With two levels of extreme pairing ####

  mean_ages <- plyr::ldply(1:10, function(i){
    xpectr::set_test_seed(i)
    df_folded <- fold(df, 5,
      cat_col = "diagnosis",
      num_col = "age",
      id_col = "participant",
      extreme_pairing_levels = 2
    )

    age_distribution <- df_folded %>%
      dplyr::group_by(.folds) %>%
      dplyr::summarise(
        mean_age = mean(age),
        sd_age = sd(age)
      )

    age_distribution$mean_age
  })


  ## Testing 'mean_ages'                                                    ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(mean_ages),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    mean_ages[["V1"]],
    c(50.5, 51.15, 50.05, 50.15, 50.9, 50, 50.05, 50.6, 50.5, 50.4),
    tolerance = 1e-4)
  expect_equal(
    mean_ages[["V2"]],
    c(50.4, 50.2, 50.65, 50.2, 50.05, 49.9, 50.4, 50.9, 50.25, 50.8),
    tolerance = 1e-4)
  expect_equal(
    mean_ages[["V3"]],
    c(50.4, 50.35, 50.35, 50.45, 50.3, 51.3, 50.3, 50.6, 50.25, 50.05),
    tolerance = 1e-4)
  expect_equal(
    mean_ages[["V4"]],
    c(50.55, 50.35, 50.95, 50.8, 50.9, 50.5, 50.4, 50.05, 50.95, 50.35),
    tolerance = 1e-4)
  expect_equal(
    mean_ages[["V5"]],
    c(50.65, 50.45, 50.5, 50.9, 50.35, 50.8, 51.35, 50.35, 50.55, 50.9),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(mean_ages),
    c("V1", "V2", "V3", "V4", "V5"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(mean_ages),
    c("numeric", "numeric", "numeric", "numeric", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(mean_ages),
    c("double", "double", "double", "double", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(mean_ages),
    c(10L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(mean_ages)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'mean_ages'                                           ####


  # With three levels of extreme pairing

  mean_ages <- plyr::ldply(1:10, function(i){
    xpectr::set_test_seed(i)
    expect_error(
      fold(df, 5,
        cat_col = "diagnosis", num_col = "age",
        id_col = "participant", extreme_pairing_levels = 3
      ),
      "data is too small to perform 3 levels of extreme pairing"
    )

    df_folded <- fold(df, 5,
      cat_col = "diagnosis", num_col = "age", extreme_pairing_levels = 3
    )

    age_distribution <- df_folded %>%
      dplyr::group_by(.folds) %>%
      dplyr::summarise(
        mean_age = mean(age),
        sd_age = sd(age)
      )

    age_distribution$mean_age
  })


  ## Testing 'mean_ages'                                                    ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(mean_ages),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    mean_ages[["V1"]],
    c(50.5175, 50.4745, 50.502, 50.504, 50.506, 50.5095, 50.5285, 50.485,
      50.5025, 50.5035),
    tolerance = 1e-4)
  expect_equal(
    mean_ages[["V2"]],
    c(50.506, 50.515, 50.488, 50.5035, 50.527, 50.547, 50.4985, 50.51,
      50.49, 50.5215),
    tolerance = 1e-4)
  expect_equal(
    mean_ages[["V3"]],
    c(50.4975, 50.464, 50.508, 50.499, 50.5045, 50.4515, 50.504, 50.4835,
      50.5165, 50.498),
    tolerance = 1e-4)
  expect_equal(
    mean_ages[["V4"]],
    c(50.497, 50.5315, 50.5055, 50.4885, 50.481, 50.5035, 50.4895, 50.5025,
      50.521, 50.4775),
    tolerance = 1e-4)
  expect_equal(
    mean_ages[["V5"]],
    c(50.482, 50.515, 50.4965, 50.505, 50.4815, 50.4885, 50.4795, 50.519,
      50.47, 50.4995),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(mean_ages),
    c("V1", "V2", "V3", "V4", "V5"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(mean_ages),
    c("numeric", "numeric", "numeric", "numeric", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(mean_ages),
    c("double", "double", "double", "double", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(mean_ages),
    c(10L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(mean_ages)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'mean_ages'                                           ####

  age_distributions <- plyr::ldply(1:4, function(i){
    xpectr::set_test_seed(i)
    df_folded <- fold(df, k = i,
      cat_col = "diagnosis",
      num_col = "age",
      extreme_pairing_levels = 1
    )

    age_distribution <- df_folded %>%
      dplyr::group_by(.folds) %>%
      dplyr::summarise(
        mean_age = mean(age),
        sd_age = sd(age)
      )

    current_rows <- list(age_distribution)

    xpectr::set_test_seed(i)
    df_folded <- fold(df, i,
      cat_col = "diagnosis", num_col = "age", extreme_pairing_levels = 2
    )

    age_distribution <- df_folded %>%
      dplyr::group_by(.folds) %>%
      dplyr::summarise(
        mean_age = mean(age),
        sd_age = sd(age)
      )

    current_rows <- c(current_rows, list(age_distribution))

    xpectr::set_test_seed(i)
    df_folded <- fold(df, i,
      cat_col = "diagnosis", num_col = "age", extreme_pairing_levels = 3
    )

    age_distribution <- df_folded %>%
      dplyr::group_by(.folds) %>%
      dplyr::summarise(
        mean_age = mean(age),
        sd_age = sd(age)
      )

    current_rows <- dplyr::bind_rows(c(current_rows, list(age_distribution)))
    current_rows
  })



  ## Testing 'age_distributions'                                            ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(age_distributions),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    age_distributions[[".folds"]],
    structure(c(1L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 3L, 1L,
      2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L,
      3L, 4L), .Label = c("1", "2", "3", "4"), class = "factor"))
  expect_equal(
    age_distributions[["mean_age"]],
    c(50.5, 50.5, 50.5, 50.488, 50.512, 50.5016, 50.4984, 50.5078, 50.4922,
      50.40912, 50.52024, 50.5707, 50.49295, 50.4997, 50.50736, 50.52116,
      50.5093, 50.46957, 50.5368, 50.5944, 50.3496, 50.5192, 50.4924,
      50.5444, 50.472, 50.4912, 50.504, 50.5056, 50.5152, 50.4752),
    tolerance = 1e-4)
  expect_equal(
    age_distributions[["sd_age"]],
    c(28.86751, 28.86751, 28.86751, 28.78545, 28.95221, 28.8557, 28.88221,
      28.89085, 28.84704, 28.95847, 29.08568, 28.56392, 28.9281, 28.63966,
      29.04209, 28.76932, 28.75625, 29.08426, 28.89381, 28.46876,
      28.95188, 29.16791, 28.59038, 29.28341, 28.66115, 28.94721,
      28.93272, 28.86576, 28.63692, 29.05039),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(age_distributions),
    c(".folds", "mean_age", "sd_age"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(age_distributions),
    c("factor", "numeric", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(age_distributions),
    c("integer", "double", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(age_distributions),
    c(30L, 3L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(age_distributions)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'age_distributions'                                   ####



  # xpectr::set_test_seed(47)
  # # With four levels of extreme pairing
  # df_folded <- fold(df, 5,
  #                   cat_col="diagnosis", num_col="age",
  #                   extreme_pairing_levels = 4)
  #
  # age_distribution <- df_folded %>% dplyr::group_by(.folds) %>%
  #   dplyr::summarise(mean_age = mean(age),
  #                    sd_age = sd(age))
})

test_that("arg check fuzz tests for fold()", {
  xpectr::set_test_seed(1)

  df <- data.frame(
    "participant" = factor(rep(c("1", "2", "3", "4", "5", "6"), 3)),
    "shuffled_participant" = factor(sample(rep(c("1", "2", "3", "4", "5", "6"), 3))),
    "age" = rep(c(25, 65, 34), 3),
    "diagnosis" = factor(rep(c("a", "b", "a", "a", "b", "b"), 3)),
    "subdiagnosis" = factor(rep(c("x", "x", "x", "y", "y", "y"), 3)),
    "score" = c(34, 23, 54, 23, 56, 76, 43, 56, 76, 42, 54, 1, 5, 76, 34, 76, 23, 65)
  )

  df <- df[order(df$participant), ]

  # Add session info
  df$session <- rep(c("1", "2", "3"), 6)


  fold_2 <- function(...){
    d <- fold(...)
    nms <- colnames(d)
    base_select(d, cols = nms[grepl("folds", nms)])
  }

  # fold_2(df, k=3, num_fold_cols = 2)

  xpectr::set_test_seed(42)
  # xpectr::gxs_function(fold_2,
  #                      args_values = list(
  #                        "data" = list(df, c(1,1,1,2,2,2,1,1,1,2,2,2),
  #                                      df[df$diagnosis == "a",], NA, matrix(1, 3, 3)),
  #                        "k" = list(3, 0, -1, NA, "hej", 40),
  #                        "cat_col" = list("diagnosis", "score", "participant", 2, NA,
  #                                         c("diagnosis", "diagnosis"), c("diagnosis", "hej"),
  #                                         c("diagnosis", "subdiagnosis")),
  #                        "num_col" = list(NULL, "score", "participant", "hej", c("participant", "diagnosis"), NA, 1),
  #                        "id_col" = list("participant", "shuffled_participant", "diagnosis",
  #                                        "score", "hej", c("participant", "diagnosis"), NA, 1),
  #                        "method" = list("n_dist", "n_fill", "n_last", "n_rand", "greedy", "staircase", "hej", 1, NA),
  #                        "id_aggregation_fn" = list(sum, 1, NA), # test mean and identity with num_col specified
  #                        "extreme_pairing_levels" = list(1, 0, NA), # Only makes sense to test >1 with num_col specified
  #                        "num_fold_cols" = list(1, 2, NA, "hej"),
  #                        "unique_fold_cols_only" = list(TRUE, "TRUE", NA), # Test FALSE with num_fold_cols > 1
  #                        "max_iters" = list(5, 0, NA),
  #                        "handle_existing_fold_cols" = list("keep_warn", "hej", NA), # test other valid values elsewhere
  #                        "parallel" = list(FALSE) # Test TRUE with num_fold_cols > 1
  #                      ), indentation = 2)



  ## Testing 'fold_2'                                                         ####
  ## Initially generated by xpectr
  # Testing different combinations of argument values

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)
  # Testing class
  expect_equal(
    class(output_19148),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19148[[".folds"]],
    structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L, 3L, 3L, 3L, 2L,
      2L, 2L, 1L, 1L, 1L), .Label = c("1", "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_19148),
    ".folds",
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_19148),
    "factor",
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_19148),
    "integer",
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_19148),
    c(18L, 1L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_19148)),
    ".folds",
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "score", nu...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "score", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * The value in 'data[[cat_col]]' mus",
                         "t be constant within each ID.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "participan...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "participant", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * 'id_col' and 'cat_col' cannot cont",
                         "ain the same column name.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = 2, num_col ...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = 2, num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'cat_col': Must be of typ",
                         "e 'character' (or 'NULL'), not 'double'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = NA, num_col...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = NA, num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'cat_col': Contains missi",
                         "ng values (element 1).")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = c("diagnosi...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = c("diagnosis", "diagnosis"), num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'cat_col': Contains dupli",
                         "cated values, position 2.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = c("diagnosi...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = c("diagnosis", "hej"), num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * 'cat_col' column(s), 'hej', not fo",
                         "und in 'data'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = c("diagnosi...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Assigning output
  output_11346 <- fold_2(data = df, k = 3, cat_col = c("diagnosis", "subdiagnosis"), num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)
  # Testing class
  expect_equal(
    class(output_11346),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_11346[[".folds"]],
    structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L, 3L, 3L, 3L, 2L,
      2L, 2L, 1L, 1L, 1L), .Label = c("1", "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_11346),
    ".folds",
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_11346),
    "factor",
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_11346),
    "integer",
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_11346),
    c(18L, 1L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_11346)),
    ".folds",
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = NULL, num_c...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Assigning output
  output_16569 <- fold_2(data = df, k = 3, cat_col = NULL, num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)
  # Testing class
  expect_equal(
    class(output_16569),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_16569[[".folds"]],
    structure(c(3L, 3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
      2L, 2L, 1L, 1L, 1L), .Label = c("1", "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_16569),
    ".folds",
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_16569),
    "factor",
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_16569),
    "integer",
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_16569),
    c(18L, 1L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_16569)),
    ".folds",
    fixed = TRUE)

  # Testing fold_2(data = c(1, 1, 1, 2, 2, 2, 1, 1, 1, 2, ...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = c(1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2), k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data': Must be of type '",
                         "data.frame', not 'double'.")),
    fixed = TRUE)

  # Testing fold_2(data = df[df$diagnosis == "a", ], k = 3...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Assigning output
  output_14577 <- fold_2(data = df[df$diagnosis == "a", ], k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)
  # Testing class
  expect_equal(
    class(output_14577),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_14577[[".folds"]],
    structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L), .Label = c("1",
      "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_14577),
    ".folds",
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_14577),
    "factor",
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_14577),
    "integer",
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_14577),
    c(9L, 1L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_14577)),
    ".folds",
    fixed = TRUE)

  # Testing fold_2(data = NA, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = NA, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data': Must be of type '",
                         "data.frame', not 'logical'.")),
    fixed = TRUE)

  # Testing fold_2(data = matrix(1, 3, 3), k = 3, cat_col ...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = matrix(1, 3, 3), k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data': Must be of type '",
                         "data.frame', not 'matrix'.")),
    fixed = TRUE)

  # Testing fold_2(data = NULL, k = 3, cat_col = "diagnosi...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = NULL, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data': Must be of type '",
                         "data.frame', not 'NULL'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: extreme_pairing_levels
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 0, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'extreme_pairing_levels':",
                         " Must be >= 1.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: extreme_pairing_levels
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = NA, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'extreme_pairing_levels':",
                         " May not be NA.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: extreme_pairing_levels
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = NULL, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'extreme_pairing_levels':",
                         " Must be of type 'count', not 'NULL'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: handle_existing_fold_cols
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "hej", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'handle_existing_fold_col",
                         "s': Must be a subset of set {keep_warn,keep,remove}.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: handle_existing_fold_cols
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = NA, parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'handle_existing_fold_col",
                         "s': May not be NA.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: handle_existing_fold_cols
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = NULL, parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'handle_existing_fold_col",
                         "s': Must be of type 'string', not 'NULL'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: id_aggregation_fn
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = 1, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'id_aggregation_fn': Must",
                         " be a function, not 'double'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: id_aggregation_fn
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = NA, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'id_aggregation_fn': Must",
                         " be a function, not 'logical'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: id_aggregation_fn
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = NULL, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'id_aggregation_fn': Must",
                         " be a function, not 'NULL'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "shuffled_participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * The value in 'data[[cat_col]]' mus",
                         "t be constant within each ID.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "diagnosis", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * 'id_col' and 'cat_col' cannot cont",
                         "ain the same column name.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "score", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data[[id_col]]': Must be",
                         " of type 'factor', not 'double'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "hej", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * 'id_col' column, 'hej', not found ",
                         "in 'data'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = c("participant", "diagnosis"), method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'id_col': Must have length 1."),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = NA, method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'id_col': May not be NA."),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = 1, method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'id_col': Must be of type",
                         " 'string' (or 'NULL'), not 'double'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Assigning output
  output_17375 <- fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = NULL, method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)
  # Testing class
  expect_equal(
    class(output_17375),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_17375[[".folds"]],
    structure(c(3L, 3L, 1L, 2L, 2L, 3L, 2L, 1L, 1L, 3L, 2L, 2L, 3L,
      1L, 2L, 1L, 3L, 1L), .Label = c("1", "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_17375),
    ".folds",
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_17375),
    "factor",
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_17375),
    "integer",
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_17375),
    c(18L, 1L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_17375)),
    ".folds",
    fixed = TRUE)

  # Testing fold_2(data = df, k = 0, cat_col = "diagnosis"...
  # Changed from baseline: k
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 0, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'k': Must be >= 1."),
    fixed = TRUE)

  # Testing fold_2(data = df, k = -1, cat_col = "diagnosis...
  # Changed from baseline: k
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = -1, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'k': Element 1 is not >= 0."),
    fixed = TRUE)

  # Testing fold_2(data = df, k = NA, cat_col = "diagnosis...
  # Changed from baseline: k
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = NA, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'k': Contains missing values element 1."),
    fixed = TRUE)

  # Testing fold_2(data = df, k = "hej", cat_col = "diagno...
  # Changed from baseline: k
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = "hej", cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'k': Must be of type 'num",
                         "eric', not 'character'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 40, cat_col = "diagnosis...
  # Changed from baseline: k
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 40, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'k': Element 1 is not <= 18."),
    fixed = TRUE)

  # Testing fold_2(data = df, k = NULL, cat_col = "diagnos...
  # Changed from baseline: k
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = NULL, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'k': Must be of type 'num",
                         "eric', not 'NULL'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: max_iters
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 0, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'max_iters': Must be >= 1."),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: max_iters
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = NA, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'max_iters': May not be NA."),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: max_iters
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = NULL, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'max_iters': Must be of t",
                         "ype 'count', not 'NULL'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Assigning output
  output_13795 <- fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_fill", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)
  # Testing class
  expect_equal(
    class(output_13795),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_13795[[".folds"]],
    structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L, 3L, 3L, 3L, 2L,
      2L, 2L, 1L, 1L, 1L), .Label = c("1", "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_13795),
    ".folds",
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_13795),
    "factor",
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_13795),
    "integer",
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_13795),
    c(18L, 1L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_13795)),
    ".folds",
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Assigning output
  output_14357 <- fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_last", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)
  # Testing class
  expect_equal(
    class(output_14357),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_14357[[".folds"]],
    structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L, 3L, 3L, 3L, 2L,
      2L, 2L, 1L, 1L, 1L), .Label = c("1", "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_14357),
    ".folds",
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_14357),
    "factor",
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_14357),
    "integer",
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_14357),
    c(18L, 1L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_14357)),
    ".folds",
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Assigning output
  output_10374 <- fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_rand", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)
  # Testing class
  expect_equal(
    class(output_10374),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_10374[[".folds"]],
    structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L, 3L, 3L, 3L, 2L,
      2L, 2L, 1L, 1L, 1L), .Label = c("1", "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_10374),
    ".folds",
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_10374),
    "factor",
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_10374),
    "integer",
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_10374),
    c(18L, 1L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_10374)),
    ".folds",
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Assigning output
  output_19735 <- fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "greedy", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)
  # Testing class
  expect_equal(
    class(output_19735),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19735[[".folds"]],
    structure(c(2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L,
      1L, 1L, 1L, 1L, 1L), .Label = c("1", "2"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_19735),
    ".folds",
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_19735),
    "factor",
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_19735),
    "integer",
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_19735),
    c(18L, 1L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_19735)),
    ".folds",
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Assigning output
  output_14317 <- fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "staircase", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)
  # Testing class
  expect_equal(
    class(output_14317),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_14317[[".folds"]],
    structure(c(2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L,
      1L, 1L, 1L, 1L, 1L), .Label = c("1", "2"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_14317),
    ".folds",
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_14317),
    "factor",
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_14317),
    "integer",
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_14317),
    c(18L, 1L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_14317)),
    ".folds",
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "hej", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'method': Must be a subse",
                         "t of set {n_dist,n_fill,n_last,n_rand,greedy,staircase}.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = 1, id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'method': Must be of type",
                         " 'string', not 'double'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = NA, id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'method': May not be NA."),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = NULL, id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'method': Must be of type",
                         " 'string', not 'NULL'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: num_col
  xpectr::set_test_seed(42)
  # Assigning output
  output_16188 <- fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = "score", id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)
  # Testing class
  expect_equal(
    class(output_16188),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_16188[[".folds"]],
    structure(c(1L, 1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 2L, 2L, 2L, 3L,
      3L, 3L, 2L, 2L, 2L), .Label = c("1", "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_16188),
    ".folds",
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_16188),
    "factor",
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_16188),
    "integer",
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_16188),
    c(18L, 1L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_16188)),
    ".folds",
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: num_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = "participant", id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip("1 assertions failed:\n * 'num_col' column must be numeric."),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: num_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = "hej", id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * 'num_col' column, 'hej', not found",
                         " in 'data'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: num_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = c("participant", "diagnosis"), id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'num_col': Must have length 1."),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: num_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NA, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'num_col': May not be NA."),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: num_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = 1, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'num_col': Must be of typ",
                         "e 'string' (or 'NULL'), not 'double'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: num_fold_cols
  xpectr::set_test_seed(42)
  # Assigning output
  output_17487 <- fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 2, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)
  # Testing class
  expect_equal(
    class(output_17487),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_17487[[".folds_1"]],
    structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L, 3L, 3L, 3L, 2L,
      2L, 2L, 1L, 1L, 1L), .Label = c("1", "2", "3"), class = "factor"))
  expect_equal(
    output_17487[[".folds_2"]],
    structure(c(3L, 3L, 3L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 2L,
      2L, 2L, 1L, 1L, 1L), .Label = c("1", "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_17487),
    c(".folds_1", ".folds_2"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_17487),
    c("factor", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_17487),
    c("integer", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_17487),
    c(18L, 2L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_17487)),
    character(0),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: num_fold_cols
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = NA, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'num_fold_cols': May not be NA."),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: num_fold_cols
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = "hej", unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'num_fold_cols': Must be ",
                         "of type 'count', not 'character'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: num_fold_cols
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = NULL, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'num_fold_cols': Must be ",
                         "of type 'count', not 'NULL'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: parallel
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = TRUE, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = NULL)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'parallel': Must be of ty",
                         "pe 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: unique_fold_cols_only
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = "TRUE", max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'unique_fold_cols_only': ",
                         "Must be of type 'logical flag', not 'character'.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: unique_fold_cols_only
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = NA, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'unique_fold_cols_only': ",
                         "May not be NA.")),
    fixed = TRUE)

  # Testing fold_2(data = df, k = 3, cat_col = "diagnosis"...
  # Changed from baseline: unique_fold_cols_only
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(fold_2(data = df, k = 3, cat_col = "diagnosis", num_col = NULL, id_col = "participant", method = "n_dist", id_aggregation_fn = sum, extreme_pairing_levels = 1, num_fold_cols = 1, unique_fold_cols_only = NULL, max_iters = 5, handle_existing_fold_cols = "keep_warn", parallel = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'unique_fold_cols_only': ",
                         "Must be of type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  ## Finished testing 'fold_2'                                                ####
  #


})

test_that("fold() works with group_by()", {
  xpectr::set_test_seed(42)

  df <- data.frame(
    "n" = c(1, 2, 3, 4, 2, 1, 5, 2, 1, 9),
    "s" = c(4, 4, 4, 4, 7, 7, 7, 7, 1, 1),
    "c" = as.character(c(4, 4, 6, 6, 7, 7, 7, 8, 8, 1)),
    "f" = as.factor(as.character(c(4, 4, 6, 6, 7, 7, 7, 8, 8, 1))),
    stringsAsFactors = FALSE
  )


  ## Testing 'xpectr::suppress_mw( df %>% dplyr::group_by(...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- xpectr::suppress_mw(
      df %>%
        dplyr::group_by(s) %>%
        fold(k=2)
    )
  # Testing class
  expect_equal(
    class(output_19148),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19148[["n"]],
    c(1, 9, 1, 2, 3, 4, 2, 1, 5, 2),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["s"]],
    c(1, 1, 4, 4, 4, 4, 7, 7, 7, 7),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["c"]],
    c("8", "1", "4", "4", "6", "6", "7", "7", "7", "8"),
    fixed = TRUE)
  expect_equal(
    output_19148[["f"]],
    structure(c(5L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 4L, 5L), .Label = c("1",
      "4", "6", "7", "8"), class = "factor"))
  expect_equal(
    output_19148[[".folds"]],
    structure(c(2L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L), .Label = c("1",
      "2"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_19148),
    c("n", "s", "c", "f", ".folds"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_19148),
    c("numeric", "numeric", "character", "factor", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_19148),
    c("double", "double", "character", "integer", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_19148),
    c(10L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_19148)),
    ".folds",
    fixed = TRUE)
  ## Finished testing 'xpectr::suppress_mw( df %>% dplyr::group_by(...'     ####


})

test_that("multiple k values in repeated folding()", {
  xpectr::set_test_seed(1)

  #
  # We can have a different number of folds per fold column.
  #

  df <- data.frame(
    "participant" = factor(rep(c("1", "2", "3", "4", "5", "6"), 3)),
    "age" = rep(c(25, 65, 34), 3),
    "diagnosis" = factor(rep(c("a", "b", "a", "a", "b", "b"), 3)),
    "score" = c(34, 23, 54, 23, 56, 76, 43, 56, 76, 42, 54, 1, 5, 76, 34, 76, 23, 65)
  )
  df <- df[order(df$participant), ]

  # Add session info
  df$session <- rep(c("1", "2", "3"), 6)

  # Four fold columns with three different number of folds settings (k)

  xpectr::set_test_seed(7)
  df_folded <- fold(
    data = df,
    k = c(1, 2, 3, 3),
    cat_col = "diagnosis",
    id_col = "participant",
    num_fold_cols = 4,
    unique_fold_cols_only = TRUE,
    max_iters = 4
  )

  expect_equal(
    colnames(df_folded),
    c("participant", "age", "diagnosis", "score", "session", ".folds_1",
      ".folds_2", ".folds_3", ".folds_4"),
    fixed = TRUE)

  expect_equal(as.character(unique(df_folded$.folds_1)), "1")
  expect_equal(sort(as.character(unique(df_folded$.folds_2))),
               as.character(c(1, 2)))
  expect_equal(sort(as.character(unique(df_folded$.folds_3))),
               as.character(c(1, 2, 3)))
  expect_equal(sort(as.character(unique(df_folded$.folds_4))),
               as.character(c(1, 2, 3)))

  expect_equal(
    as.character(df_folded$.folds_2),
    c("2", "2", "2", "2", "2", "2", "1", "1", "1", "2", "2", "2", "1",
      "1", "1", "2", "2", "2"),
    fixed = TRUE)

  expect_equal(
    as.character(df_folded$.folds_3),
    c("3", "3", "3", "1", "1", "1", "2", "2", "2", "1", "1", "1", "2",
      "2", "2", "3", "3", "3"),
    fixed = TRUE)

  expect_equal(
    as.character(df_folded$.folds_4),
    c("3", "3", "3", "1", "1", "1", "2", "2", "2", "2", "2", "2",
    "3", "3", "3", "1", "1", "1"),
    fixed = TRUE)


  # Where k is mix of counts and percentages
  # and no id_col

  xpectr::set_test_seed(7)
  df_folded <- fold(
    data = df,
    k = c(2, 0.4, 0.2),
    cat_col = "diagnosis",
    num_fold_cols = 3,
    unique_fold_cols_only = TRUE,
    max_iters = 4
  )

  # 0.4 should equal 3 groups
  # i.e. ceiling(nrow(df_folded) / floor((nrow(df_folded) * 0.4)))

  # 0.2 should equal 6 groups
  # i.e. ceiling(nrow(df_folded) / floor((nrow(df_folded) * 0.2)))

  expect_equal(
    colnames(df_folded),
    c("participant", "age", "diagnosis", "score", "session", ".folds_1",
      ".folds_2", ".folds_3"),
    fixed = TRUE)

  expect_equal(sort(as.character(unique(df_folded$.folds_1))),
               as.character(c(1, 2)))
  expect_equal(sort(as.character(unique(df_folded$.folds_2))),
               as.character(c(1, 2, 3)))
  expect_equal(sort(as.character(unique(df_folded$.folds_3))),
               as.character(c(1, 2, 3, 4, 5, 6)))


  ## Testing 'df_folded'                                                    ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(df_folded),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    df_folded[["participant"]],
    structure(c(1L, 1L, 1L, 3L, 3L, 3L, 4L, 4L, 4L, 2L, 2L, 2L, 5L,
      5L, 5L, 6L, 6L, 6L), .Label = c("1", "2", "3", "4", "5", "6"),
      class = "factor"))
  expect_equal(
    df_folded[["age"]],
    c(25, 25, 25, 34, 34, 34, 25, 25, 25, 65, 65, 65, 65, 65, 65, 34,
      34, 34),
    tolerance = 1e-4)
  expect_equal(
    df_folded[["diagnosis"]],
    structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
      2L, 2L, 2L, 2L, 2L), .Label = c("a", "b"), class = "factor"))
  expect_equal(
    df_folded[["score"]],
    c(34, 43, 5, 54, 76, 34, 23, 42, 76, 23, 56, 76, 56, 54, 23, 76,
      1, 65),
    tolerance = 1e-4)
  expect_equal(
    df_folded[["session"]],
    c("1", "2", "3", "1", "2", "3", "1", "2", "3", "1", "2", "3", "1",
      "2", "3", "1", "2", "3"),
    fixed = TRUE)
  expect_equal(
    df_folded[[".folds_1"]],
    structure(c(2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 2L,
      1L, 2L, 2L, 1L, 1L), .Label = c("1", "2"), class = "factor"))
  expect_equal(
    df_folded[[".folds_2"]],
    structure(c(3L, 1L, 2L, 1L, 3L, 2L, 3L, 1L, 2L, 2L, 3L, 1L, 3L,
      1L, 1L, 2L, 3L, 2L), .Label = c("1", "2", "3"), class = "factor"))
  expect_equal(
    df_folded[[".folds_3"]],
    structure(c(4L, 5L, 4L, 2L, 3L, 2L, 6L, 1L, 6L, 2L, 1L, 4L, 5L,
      4L, 6L, 3L, 2L, 6L), .Label = c("1", "2", "3", "4", "5", "6"),
      class = "factor"))
  # Testing column names
  expect_equal(
    names(df_folded),
    c("participant", "age", "diagnosis", "score", "session", ".folds_1",
      ".folds_2", ".folds_3"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(df_folded),
    c("factor", "numeric", "factor", "numeric", "character", "factor",
      "factor", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(df_folded),
    c("integer", "double", "integer", "double", "character", "integer",
      "integer", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(df_folded),
    c(18L, 8L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(df_folded)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'df_folded'                                           ####


  # Without cat_col

  xpectr::set_test_seed(7)
  df_folded <- fold(
    data = df,
    k = c(2, 3),
    num_fold_cols = 2,
    unique_fold_cols_only = TRUE,
    max_iters = 4
  )

  ## Testing 'df_folded'                                                    ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(df_folded),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    df_folded[[".folds_1"]],
    structure(c(2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 1L,
      1L, 2L, 1L, 2L, 1L), .Label = c("1", "2"), class = "factor"))
  expect_equal(
    df_folded[[".folds_2"]],
    structure(c(3L, 1L, 2L, 1L, 3L, 2L, 3L, 1L, 2L, 3L, 2L, 1L, 2L,
      1L, 3L, 1L, 2L, 3L), .Label = c("1", "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(df_folded),
    c("participant", "age", "diagnosis", "score", "session", ".folds_1",
      ".folds_2"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(df_folded),
    c("factor", "numeric", "factor", "numeric", "character", "factor",
      "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(df_folded),
    c("integer", "double", "integer", "double", "character", "integer",
      "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(df_folded),
    c(18L, 7L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(df_folded)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'df_folded'                                           ####


  # With num_col
  # Without cat_col

  xpectr::set_test_seed(7)
  df_folded <- fold(
    data = df,
    k = c(3, 2),
    num_col = "score",
    num_fold_cols = 2,
    unique_fold_cols_only = TRUE,
    max_iters = 4
  )

  balanced_avg_scores <- df_folded %>%
    dplyr::group_by(.folds_1) %>%
    dplyr::summarise(mean_score = mean(score))

  expect_equal(
    balanced_avg_scores[[".folds_1"]],
    structure(1:3, .Label = c("1", "2", "3"), class = "factor"))
  expect_equal(
    balanced_avg_scores[["mean_score"]],
    c(46, 47.66667, 42.5),
    tolerance = 1e-4)


  ## Testing 'df_folded'                                                    ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(df_folded),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    df_folded[["participant"]],
    structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
      5L, 5L, 6L, 6L, 6L), .Label = c("1", "2", "3", "4", "5", "6"),
      class = "factor"))
  expect_equal(
    df_folded[["age"]],
    c(25, 25, 25, 65, 65, 65, 34, 34, 34, 25, 25, 25, 65, 65, 65, 34,
      34, 34),
    tolerance = 1e-4)
  expect_equal(
    df_folded[["diagnosis"]],
    structure(c(1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L,
      2L, 2L, 2L, 2L, 2L), .Label = c("a", "b"), class = "factor"))
  expect_equal(
    df_folded[["score"]],
    c(34, 43, 5, 23, 56, 76, 54, 76, 34, 23, 42, 76, 56, 54, 23, 76,
      1, 65),
    tolerance = 1e-4)
  expect_equal(
    df_folded[["session"]],
    c("1", "2", "3", "1", "2", "3", "1", "2", "3", "1", "2", "3", "1",
      "2", "3", "1", "2", "3"),
    fixed = TRUE)
  expect_equal(
    df_folded[[".folds_1"]],
    structure(c(3L, 2L, 1L, 2L, 3L, 3L, 2L, 1L, 2L, 3L, 1L, 2L, 2L,
      1L, 1L, 1L, 3L, 3L), .Label = c("1", "2", "3"), class = "factor"))
  expect_equal(
    df_folded[[".folds_2"]],
    structure(c(1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L,
      1L, 2L, 2L, 1L, 2L), .Label = c("1", "2"), class = "factor"))
  # Testing column names
  expect_equal(
    names(df_folded),
    c("participant", "age", "diagnosis", "score", "session", ".folds_1",
      ".folds_2"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(df_folded),
    c("factor", "numeric", "factor", "numeric", "character", "factor",
      "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(df_folded),
    c("integer", "double", "integer", "double", "character", "integer",
      "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(df_folded),
    c(18L, 7L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(df_folded)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'df_folded'                                           ####


  # Error when wrong number of k's


  ## Testing 'fold( data = df, k = c(3, 2, 3), num_col = "...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19889 <- xpectr::capture_side_effects(fold(
      data = df,
      k = c(3, 2, 3),
      num_col = "score",
      num_fold_cols = 2
    ), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19889[['error']]),
    xpectr::strip("1 assertions failed:\n * when `length(k) > 1`, it must have precisely `num_fold_cols` elements."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19889[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'fold( data = df, k = c(3, 2, 3), num_col = "...'     ####


})
