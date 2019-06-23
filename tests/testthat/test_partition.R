library(groupdata2)
context("partition()")

test_that("dimensions of data frame with partition()",{

  df <- data.frame("participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
                   "age" = rep(c(25,65,34), 3),
                   "diagnosis" = rep(c('a', 'b', 'a', 'a', 'b', 'b'), 3),
                   "score" = c(34,23,54,23,56,76,43,56,76,42,54,1,5,76,34,76,23,65))

  df <- df %>% dplyr::arrange(participant)

  # Add session info
  df$session <- rep(c('1','2', '3'), 6)

  # The added grouping factor means we should get and extra column
  expect_equal(ncol(partition(df, 0.2, list_out = FALSE)), 6)
  expect_equal(ncol(partition(df, c(0.2,0.3), list_out = FALSE)), 6)

  # We expect the same amount of rows
  expect_equal(nrow(partition(df, 0.2, list_out = FALSE)), 18)
  expect_equal(nrow(partition(df, c(0.2,0.3), list_out = FALSE)), 18)


})

test_that(".partitions is correct in partition() with list_out == FALSE",{

  df <- data.frame("participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
                   "age" = rep(c(25,65,34), 3),
                   "diagnosis" = rep(c('a', 'b', 'a', 'a', 'b', 'b'), 3),
                   "score" = c(34,23,54,23,56,76,43,56,76,42,54,1,5,76,34,76,23,65))

  df <- df %>% dplyr::arrange(participant)

  # Add session info
  df$session <- rep(c('1','2', '3'), 6)

  df_unequal <- df %>%
    dplyr::filter(dplyr::row_number() != 18)

  col_is_factor <- function(df, n, cat_col = NULL, num_col = NULL, id_col = NULL, col){

    set.seed(1)
    partitioned_df <- partition(df, n, cat_col=cat_col, num_col=num_col,
                                id_col=id_col, list_out = FALSE)

    return(is.factor(partitioned_df[[col]]))

  }



  group_counts <- function(df, n, cat_col = NULL, num_col = NULL, id_col = NULL, force_equal = FALSE){

    set.seed(1)
    partitioned_df <- partition(df, n, cat_col=cat_col, num_col=num_col,
                                id_col=id_col, force_equal = force_equal,
                                list_out = FALSE)
    counts <- plyr::count(partitioned_df$.partitions)
    return(counts$freq)

  }

  # Check if .folds is a factor
  expect_true(col_is_factor(df, 0.2, col='.partitions'))
  expect_true(col_is_factor(df, 0.2, cat_col = 'diagnosis', col='.partitions'))
  expect_true(col_is_factor(df, 0.2, id_col = 'participant', col='.partitions'))
  expect_true(col_is_factor(df, 0.4, cat_col = 'diagnosis',
                            id_col = 'participant',col='.partitions'))
  expect_true(col_is_factor(df, 0.2, num_col = 'score', col='.partitions'))
  expect_true(col_is_factor(df, 0.3, num_col = 'score', cat_col = 'diagnosis', col='.partitions'))
  expect_true(col_is_factor(df, 0.2, num_col = 'score', id_col = 'participant',
                            col='.partitions'))
  expect_true(col_is_factor(df, 0.4, num_col = 'score', cat_col = 'diagnosis',
                            id_col = 'participant', col='.partitions'))

  # equal number of rows in df

  expect_equal(group_counts(df, 0.2), c(3,15))
  expect_equal(group_counts(df, 0.2, cat_col = 'diagnosis'), c(2,16))
  expect_equal(group_counts(df, 0.2, id_col = 'participant'), c(3,15))
  expect_equal(group_counts(df, 0.2, num_col = 'score'), c(3,15))
  expect_equal(group_counts(df, 0.4, cat_col = 'diagnosis',
                            id_col = 'participant'), c(6,12))
  expect_equal(group_counts(df, 0.4, cat_col = 'diagnosis',
                            num_col = 'score'), c(6,12))
  expect_equal(group_counts(df, 0.4, num_col = 'score', id_col = 'participant'), c(6,12))
  expect_equal(group_counts(df, 0.4, cat_col = 'diagnosis', num_col = 'score',
                            id_col = 'participant'), c(6,12))
  expect_equal(group_counts(df, 2, cat_col = 'diagnosis',
                            id_col = 'participant'), c(12,6))
  expect_equal(group_counts(df, 3, cat_col = 'diagnosis', id_col = 'participant'), c(18))
  expect_equal(group_counts(df, 3, id_col = 'participant'), c(9,9))
  expect_equal(group_counts(df, 2, id_col = 'participant'), c(6,12))
  expect_equal(group_counts(df, 2, num_col = 'score'), c(2,16))
  expect_equal(group_counts(df, c(2,4,6), num_col = 'score'), c(2,4,6,6))
  expect_equal(group_counts(df, 2, cat_col = 'diagnosis', num_col = 'score'), c(4,14))
  expect_equal(group_counts(df, 2, cat_col = 'diagnosis', num_col = 'score'), c(4,14))
  expect_equal(group_counts(df, 2, id_col = 'participant', num_col = 'score'), c(6,12))
  expect_equal(group_counts(df, 2, cat_col = 'diagnosis', id_col = 'participant', num_col = 'score'), c(12,6))
  expect_equal(group_counts(df, 1, cat_col = 'diagnosis', id_col = 'participant', num_col = 'score'), c(6,12))

  # unequal number of rows in df
  set.seed(1)
  expect_equal(group_counts(df_unequal, 0.2), c(3,14))
  expect_equal(group_counts(df_unequal, 0.2, cat_col = 'diagnosis'), c(2,15))
  expect_equal(group_counts(df_unequal, 0.2, id_col = 'participant'), c(3,14))
  expect_equal(group_counts(df_unequal, 0.2, num_col = 'score'), c(3,14))
  expect_equal(group_counts(df_unequal, 0.4, cat_col = 'diagnosis',
                            id_col = 'participant'), c(6,11))
  expect_equal(group_counts(df_unequal, 0.4, cat_col = 'diagnosis',
                            num_col = 'score'), c(6,11))
  expect_equal(group_counts(df_unequal, 0.4, num_col = 'score', id_col = 'participant'), c(5,12))
  expect_equal(group_counts(df_unequal, 0.4, cat_col = 'diagnosis', num_col = 'score',
                            id_col = 'participant'), c(5,12))
  expect_equal(group_counts(df_unequal, 2, cat_col = 'diagnosis',
                            id_col = 'participant'), c(12,5))
  expect_equal(group_counts(df_unequal, 3, cat_col = 'diagnosis', id_col = 'participant'), c(17))
  expect_equal(group_counts(df_unequal, 3, id_col = 'participant'), c(9,8))
  expect_equal(group_counts(df_unequal, 2, id_col = 'participant'), c(6,11))
  expect_equal(group_counts(df_unequal, 2, num_col = 'score'), c(2,15))
  expect_equal(group_counts(df_unequal, c(2,4,6), num_col = 'score'), c(2,4,6,5))
  expect_equal(group_counts(df_unequal, 2, cat_col = 'diagnosis', num_col = 'score'), c(4,13))
  expect_equal(group_counts(df_unequal, 2, cat_col = 'diagnosis', num_col = 'score'), c(4,13))
  expect_equal(group_counts(df_unequal, 2, id_col = 'participant', num_col = 'score'), c(5,12))
  expect_equal(group_counts(df_unequal, 2, cat_col = 'diagnosis', id_col = 'participant', num_col = 'score'), c(11,6))
  expect_equal(group_counts(df_unequal, 1, cat_col = 'diagnosis', id_col = 'participant', num_col = 'score'), c(5,12))


  # Test force_equal

  # equal number of rows in df
  set.seed(1)
  expect_equal(group_counts(df, 2, id_col = 'participant', force_equal = TRUE), c(6))
  expect_equal(group_counts(df, 3, id_col = 'participant', force_equal = TRUE), c(9))
  expect_equal(group_counts(df, 2, cat_col = 'diagnosis', force_equal = TRUE), c(4))
  expect_equal(group_counts(df, 2, num_col = 'score', force_equal = TRUE), c(2))
  expect_equal(group_counts(df, 2, cat_col = 'diagnosis', num_col = 'score', force_equal = TRUE), c(4))
  expect_equal(group_counts(df, 2, cat_col = 'diagnosis', num_col = 'score',
                            id_col = 'participant', force_equal = TRUE), c(12))
  expect_equal(group_counts(df, 2, id_col = 'participant',
                            cat_col = 'diagnosis',
                            force_equal = TRUE), c(12))

  # unequal number of rows in df
  set.seed(1)
  expect_equal(group_counts(df_unequal, 2, id_col = 'participant', force_equal = TRUE), c(6))
  expect_equal(group_counts(df_unequal, 3, id_col = 'participant', force_equal = TRUE), c(9))
  expect_equal(group_counts(df_unequal, 2, cat_col = 'diagnosis', force_equal = TRUE), c(4))
  expect_equal(group_counts(df_unequal, 2, num_col = 'score', force_equal = TRUE), c(2))
  expect_equal(group_counts(df_unequal, 2, cat_col = 'diagnosis', num_col = 'score', force_equal = TRUE), c(4))
  expect_equal(group_counts(df_unequal, 2, cat_col = 'diagnosis', num_col = 'score',
                            id_col = 'participant', force_equal = TRUE), c(11))
  expect_equal(group_counts(df_unequal, 2, id_col = 'participant',
                            cat_col = 'diagnosis',
                            force_equal = TRUE), c(12))

})

test_that(".partitions is correct in partition() with list_out == TRUE",{

  df <- data.frame("participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
                   "age" = rep(c(25,65,34), 3),
                   "diagnosis" = rep(c('a', 'b', 'a', 'a', 'b', 'b'), 3),
                   "score" = c(34,23,54,23,56,76,43,56,76,42,54,1,5,76,34,76,23,65))

  df <- df %>% dplyr::arrange(participant)

  # Add session info
  df$session <- rep(c('1','2', '3'), 6)


  partition_count <- function(df, n, cat_col = NULL, num_col = NULL, id_col = NULL, force_equal = FALSE){

    partitioned_df <- partition(df, n, cat_col=cat_col, num_col=num_col,
                                id_col=id_col, force_equal = force_equal,
                                list_out = TRUE)

    counts <- length(partitioned_df)
    return(counts)

  }

  partition_element_counts <- function(df, n, cat_col = NULL, num_col = NULL, id_col = NULL,
                                       force_equal = FALSE){

    partitioned_df <- partition(df, n, cat_col=cat_col, num_col=num_col,
                                id_col=id_col, force_equal = force_equal,
                                list_out = TRUE)

    counts <- sapply(partitioned_df, nrow)
    return(counts)

  }


  expect_equal(partition_count(df, 2),2)
  expect_equal(partition_count(df, 2, force_equal = TRUE),1)
  expect_equal(partition_count(df, c(2,3,5)),4)
  expect_equal(partition_count(df, c(2,3,5),force_equal = TRUE),3)

  expect_equal(partition_element_counts(df, 2), c(2,16))
  expect_equal(partition_element_counts(df, 9), c(9,9))
  expect_equal(partition_element_counts(df, c(0.2,0.2)), c(3,3,12))
  expect_equal(partition_element_counts(df, c(0.2,0.2),
                                        force_equal = TRUE), c(3,3))

})

test_that("partition() outputs correct error messages",{

  df <- data.frame("participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
                   "age" = rep(c(25,65,34), 3),
                   "diagnosis" = rep(c('a', 'b', 'a', 'a', 'b', 'b'), 3),
                   "score" = c(34,23,54,23,56,76,43,56,76,42,54,1,5,76,34,76,23,65))

  df <- df %>% dplyr::arrange(participant)

  # Add session info
  df$session <- rep(c('1','2', '3'), 6)


  expect_error(partition(df, c(19), cat_col = 'diagnosis'),
               "n creates more values than is in v", fixed = TRUE)

  expect_warning(expect_error(partition(c(1:3), 0.2, force_equal = TRUE),
                              "NA in .partitions column.", fixed = TRUE),
                 "No groups. Returned NA.", fixed = TRUE)


})


test_that("bootstrap test of num_col works",{

  # Takes 4 seconds, so we disable it for now.
  testthat::skip(message = "Skipping bootstrapped numerical balancing test in partition()")

  df <- data.frame("participant"=factor(rep(1:100, 100)),
                   "diagnosis"=factor(rep(c("a","b","c","d","e"), 2000)),
                   "age"=rep(sample(100),100))

  # Single
  set.seed(1)
  df_partitioned <- partition(df, c(0.3,0.3),
                              #cat_col="diagnosis",
                              num_col="age",
                              #id_col="participant",
                              list_out = FALSE)

  for (i in 1:10){

    set.seed(i)
    df_partitioned <- partition(df, 0.5, cat_col="diagnosis", num_col="age",
                                id_col="participant", list_out = FALSE)

    age_distribution <- df_partitioned %>% group_by(.partitions) %>%
      dplyr::summarise(mean_age = mean(age),
                       sd_age = sd(age))

    expect_true(is_between_(age_distribution$mean_age[1], 50, 52))
    expect_true(is_between_(age_distribution$mean_age[2], 49, 51))

  }

  for (i in 1:10){

    set.seed(i)
    df_partitioned <- partition(df, c(0.2, 0.2, 0.2, 0.2, 0.2),
                                cat_col="diagnosis", num_col="age",
                                id_col="participant", list_out = FALSE)

    age_distribution <- df_partitioned %>% group_by(.partitions) %>%
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

    set.seed(i)
    df_partitioned <- partition(df, c(0.2, 0.2, 0.2, 0.2, 0.2),
                                cat_col="diagnosis", num_col="age",
                                id_col="participant", extreme_pairing_levels = 2,
                                list_out = FALSE)

    age_distribution <- df_partitioned %>% group_by(.partitions) %>%
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

    set.seed(i)
    df_partitioned <- partition(df, c(0.2, 0.2, 0.2, 0.2, 0.2),
                                cat_col="diagnosis", num_col="age",
                                id_col="participant", extreme_pairing_levels = 3,
                                list_out = FALSE)

    age_distribution <- df_partitioned %>% group_by(.partitions) %>%
      dplyr::summarise(mean_age = mean(age),
                       sd_age = sd(age))

    expect_true(is_between_(age_distribution$mean_age[1], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[2], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[3], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[4], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[5], 49, 51.5))

  }

  # With four levels of extreme pairing
  expect_error(partition(df, c(0.2, 0.2, 0.2, 0.2, 0.2),
                              cat_col="diagnosis", num_col="age",
                              id_col="participant", extreme_pairing_levels = 4,
                              list_out = FALSE),
               " 4 levels of extreme pairing. Decrease 'extreme_pairing_levels'.")

})

