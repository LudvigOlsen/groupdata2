library(groupdata2)
context("partition()")

test_that("dimensions of dataframe with partition()",{

  df <- data.frame("participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
                   "age" = rep(c(25,65,34), 3),
                   "diagnosis" = rep(c('a', 'b', 'a', 'a', 'b', 'b'), 3),
                   "score" = c(34,23,54,23,56,76,43,56,76,42,54,1,5,76,34,76,23,65))

  df <- df %>% arrange(participant)

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

  df <- df %>% arrange(participant)

  # Add session info
  df$session <- rep(c('1','2', '3'), 6)

  col_is_factor <- function(df, n, cat_col = NULL, num_col = NULL, id_col = NULL, col){

    partitioned_df <- partition(df, n, cat_col=cat_col, num_col=num_col,
                                id_col=id_col, list_out = FALSE)

    return(is.factor(partitioned_df[[col]]))

  }



  group_counts <- function(df, n, cat_col = NULL, num_col = NULL, id_col = NULL, force_equal = FALSE){

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
  expect_true(col_is_factor(df, 0.2, num_col = 'score', id_col = 'participant', col='.partitions'))
  expect_true(col_is_factor(df, 0.4, num_col = 'score', cat_col = 'diagnosis',
                            id_col = 'participant', col='.partitions'))

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


  # Test force_equal

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

})

test_that(".partitions is correct in partition() with list_out == TRUE",{

  df <- data.frame("participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
                   "age" = rep(c(25,65,34), 3),
                   "diagnosis" = rep(c('a', 'b', 'a', 'a', 'b', 'b'), 3),
                   "score" = c(34,23,54,23,56,76,43,56,76,42,54,1,5,76,34,76,23,65))

  df <- df %>% arrange(participant)

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

  df <- df %>% arrange(participant)

  # Add session info
  df$session <- rep(c('1','2', '3'), 6)


  expect_error(partition(df, c(19), cat_col = 'diagnosis'),
               "n creates more values than is in v", fixed = TRUE)

  expect_warning(expect_error(partition(c(1:3), 0.2, force_equal = TRUE),
                              "NA in .partitions column.", fixed = TRUE),
                 "No groups. Returned NA.", fixed = TRUE)


})
