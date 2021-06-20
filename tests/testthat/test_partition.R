library(groupdata2)
context("partition()")

test_that("dimensions of data frame with partition()", {
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

  # The added grouping factor means we should get and extra column
  expect_equal(ncol(partition(df, 0.2, list_out = FALSE)), 6)
  expect_equal(ncol(partition(df, c(0.2, 0.3), list_out = FALSE)), 6)

  # We expect the same amount of rows
  expect_equal(nrow(partition(df, 0.2, list_out = FALSE)), 18)
  expect_equal(nrow(partition(df, c(0.2, 0.3), list_out = FALSE)), 18)
})

test_that(".partitions is correct in partition() with list_out == FALSE", {
  xpectr::set_test_seed(6)

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

  col_is_factor <- function(df, n, cat_col = NULL, num_col = NULL, id_col = NULL, col) {
    xpectr::set_test_seed(1)
    partitioned_df <- partition(df, n,
      cat_col = cat_col, num_col = num_col,
      id_col = id_col, list_out = FALSE
    )

    return(is.factor(partitioned_df[[col]]))
  }

  group_counts <- function(df, n, cat_col = NULL, num_col = NULL, id_col = NULL, force_equal = FALSE) {
    xpectr::set_test_seed(1)
    partitioned_df <- partition(df, n,
      cat_col = cat_col, num_col = num_col,
      id_col = id_col, force_equal = force_equal,
      list_out = FALSE
    )
    counts <- plyr::count(partitioned_df$.partitions)
    return(counts$freq)
  }

  # Check if .folds is a factor
  expect_true(col_is_factor(df, 0.2, col = ".partitions"))
  expect_true(col_is_factor(df, 0.2, cat_col = "diagnosis", col = ".partitions"))
  expect_true(col_is_factor(df, 0.2, id_col = "participant", col = ".partitions"))
  expect_true(col_is_factor(df, 0.4,
    cat_col = "diagnosis",
    id_col = "participant", col = ".partitions"
  ))
  expect_true(col_is_factor(df, 0.2, num_col = "score", col = ".partitions"))
  expect_true(col_is_factor(df, 0.3, num_col = "score", cat_col = "diagnosis", col = ".partitions"))
  expect_true(col_is_factor(df, 0.2,
    num_col = "score", id_col = "participant",
    col = ".partitions"
  ))
  expect_true(col_is_factor(df, 0.4,
    num_col = "score", cat_col = "diagnosis",
    id_col = "participant", col = ".partitions"
  ))

  # equal number of rows in df

  expect_equal(group_counts(df, 0.2), c(3, 15))
  expect_equal(group_counts(df, 0.2, cat_col = "diagnosis"), c(2, 16))
  expect_equal(group_counts(df, 0.2, id_col = "participant"), c(3, 15))
  expect_equal(group_counts(df, 0.2, num_col = "score"), c(3, 15))
  expect_equal(group_counts(df, 0.4,
    cat_col = "diagnosis",
    id_col = "participant"
  ), c(6, 12))
  expect_equal(group_counts(df, 0.4,
    cat_col = "diagnosis",
    num_col = "score"
  ), c(6, 12))
  expect_equal(group_counts(df, 0.4, num_col = "score", id_col = "participant"), c(6, 12))
  expect_equal(group_counts(df, 0.4,
    cat_col = "diagnosis", num_col = "score",
    id_col = "participant"
  ), c(6, 12))
  expect_equal(group_counts(df, 2,
    cat_col = "diagnosis",
    id_col = "participant"
  ), c(12, 6))
  expect_equal(group_counts(df, 3, cat_col = "diagnosis", id_col = "participant"), c(18))
  expect_equal(group_counts(df, 3, id_col = "participant"), c(9, 9))
  expect_equal(group_counts(df, 2, id_col = "participant"), c(6, 12))
  expect_equal(group_counts(df, 2, num_col = "score"), c(2, 16))
  expect_equal(group_counts(df, c(2, 4, 6), num_col = "score"), c(2, 4, 6, 6))
  expect_equal(group_counts(df, 2, cat_col = "diagnosis", num_col = "score"), c(4, 14))
  expect_equal(group_counts(df, 2, cat_col = "diagnosis", num_col = "score"), c(4, 14))
  expect_equal(group_counts(df, 2, id_col = "participant", num_col = "score"), c(6, 12))
  expect_equal(group_counts(df, 2, cat_col = "diagnosis", id_col = "participant", num_col = "score"), c(12, 6))
  expect_equal(group_counts(df, 1, cat_col = "diagnosis", id_col = "participant", num_col = "score"), c(6, 12))

  # unequal number of rows in df
  xpectr::set_test_seed(1)
  expect_equal(group_counts(df_unequal, 0.2), c(3, 14))
  expect_equal(group_counts(df_unequal, 0.2, cat_col = "diagnosis"), c(2, 15))
  expect_equal(group_counts(df_unequal, 0.2, id_col = "participant"), c(3, 14))
  expect_equal(group_counts(df_unequal, 0.2, num_col = "score"), c(3, 14))
  expect_equal(group_counts(df_unequal, 0.4,
    cat_col = "diagnosis",
    id_col = "participant"
  ), c(6, 11))
  expect_equal(group_counts(df_unequal, 0.4,
    cat_col = "diagnosis",
    num_col = "score"
  ), c(6, 11))
  expect_equal(group_counts(df_unequal, 0.4, num_col = "score", id_col = "participant"), c(5, 12))
  expect_equal(group_counts(df_unequal, 0.4,
    cat_col = "diagnosis", num_col = "score",
    id_col = "participant"
  ), c(6, 11))
  expect_equal(group_counts(df_unequal, 2,
    cat_col = "diagnosis",
    id_col = "participant"
  ), c(11, 6))
  expect_equal(group_counts(df_unequal, 3, cat_col = "diagnosis", id_col = "participant"), c(17))
  expect_equal(group_counts(df_unequal, 3, id_col = "participant"), c(9, 8))
  expect_equal(group_counts(df_unequal, 2, id_col = "participant"), c(6, 11))
  expect_equal(group_counts(df_unequal, 2, num_col = "score"), c(2, 15))
  expect_equal(group_counts(df_unequal, c(2, 4, 6), num_col = "score"), c(2, 4, 6, 5))
  expect_equal(group_counts(df_unequal, 2, cat_col = "diagnosis", num_col = "score"), c(4, 13))
  expect_equal(group_counts(df_unequal, 2, cat_col = "diagnosis", num_col = "score"), c(4, 13))
  expect_equal(group_counts(df_unequal, 2, id_col = "participant", num_col = "score"), c(5, 12))
  expect_equal(group_counts(df_unequal, 2, cat_col = "diagnosis", id_col = "participant", num_col = "score"), c(11, 6))
  expect_equal(group_counts(df_unequal, 1, cat_col = "diagnosis", id_col = "participant", num_col = "score"), c(6, 11))


  # Test force_equal

  # equal number of rows in df
  xpectr::set_test_seed(1)
  expect_equal(group_counts(df, 2, id_col = "participant", force_equal = TRUE), c(6))
  expect_equal(group_counts(df, 3, id_col = "participant", force_equal = TRUE), c(9))
  expect_equal(group_counts(df, 2, cat_col = "diagnosis", force_equal = TRUE), c(4))
  expect_equal(group_counts(df, 2, num_col = "score", force_equal = TRUE), c(2))
  expect_equal(group_counts(df, 2, cat_col = "diagnosis", num_col = "score", force_equal = TRUE), c(4))
  expect_equal(group_counts(df, 2,
    cat_col = "diagnosis", num_col = "score",
    id_col = "participant", force_equal = TRUE
  ), c(12))
  expect_equal(group_counts(df, 2,
    id_col = "participant",
    cat_col = "diagnosis",
    force_equal = TRUE
  ), c(12))

  # unequal number of rows in df
  xpectr::set_test_seed(1)
  expect_equal(group_counts(df_unequal, 2, id_col = "participant", force_equal = TRUE), c(6))
  expect_equal(group_counts(df_unequal, 3, id_col = "participant", force_equal = TRUE), c(9))
  expect_equal(group_counts(df_unequal, 2, cat_col = "diagnosis", force_equal = TRUE), c(4))
  expect_equal(group_counts(df_unequal, 2, num_col = "score", force_equal = TRUE), c(2))
  expect_equal(group_counts(df_unequal, 2, cat_col = "diagnosis", num_col = "score", force_equal = TRUE), c(4))
  expect_equal(group_counts(df_unequal, 2,
    cat_col = "diagnosis", num_col = "score",
    id_col = "participant", force_equal = TRUE
  ), c(11))
  expect_equal(group_counts(df_unequal, 2,
    id_col = "participant",
    cat_col = "diagnosis",
    force_equal = TRUE
  ), c(12))
})

test_that(".partitions is correct in partition() with list_out == TRUE", {
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


  partition_count <- function(df, n, cat_col = NULL, num_col = NULL, id_col = NULL, force_equal = FALSE) {
    partitioned_df <- partition(df, n,
      cat_col = cat_col, num_col = num_col,
      id_col = id_col, force_equal = force_equal,
      list_out = TRUE
    )

    counts <- length(partitioned_df)
    return(counts)
  }

  partition_element_counts <- function(df, n, cat_col = NULL, num_col = NULL, id_col = NULL,
                                       force_equal = FALSE) {
    partitioned_df <- partition(df, n,
      cat_col = cat_col, num_col = num_col,
      id_col = id_col, force_equal = force_equal,
      list_out = TRUE
    )

    counts <- sapply(partitioned_df, nrow)
    return(counts)
  }


  expect_equal(partition_count(df, 2), 2)
  expect_equal(partition_count(df, 2, force_equal = TRUE), 1)
  expect_equal(partition_count(df, c(2, 3, 5)), 4)
  expect_equal(partition_count(df, c(2, 3, 5), force_equal = TRUE), 3)

  expect_equal(partition_element_counts(df, 2), c(2, 16))
  expect_equal(partition_element_counts(df, 9), c(9, 9))
  expect_equal(partition_element_counts(df, c(0.2, 0.2)), c(3, 3, 12))
  expect_equal(partition_element_counts(df, c(0.2, 0.2),
    force_equal = TRUE
  ), c(3, 3))
})

test_that("partition() outputs correct error messages", {
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

  expect_error(
    xpectr::strip_msg(partition(df, c(19), cat_col = "diagnosis")),
    xpectr::strip("1 assertions failed:\n * Variable 'p': Element 1 is not <= 18."),
    fixed = TRUE)

  expect_error(partition(df, c(17, 2), cat_col = "diagnosis"),
    "'n' creates more values than is in 'v'",
    fixed = TRUE
  )

  expect_error(
    xpectr::strip_msg(partition(c(1:3), 0.2, force_equal = TRUE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data': Must be of type 'd",
                         "ata.frame', not 'integer'.")),
    fixed = TRUE)

})

test_that("bootstrap test of num_col works", {
  xpectr::set_test_seed(1)

  # Takes 4 seconds, so we disable it for now.
  testthat::skip(message = "Skipping bootstrapped numerical balancing test in partition()")

  df <- data.frame(
    "participant" = factor(rep(1:100, 100)),
    "diagnosis" = factor(rep(c("a", "b", "c", "d", "e"), 2000)),
    "age" = rep(sample(100), 100)
  )

  # Single
  xpectr::set_test_seed(1)
  df_partitioned <- partition(df, c(0.3, 0.3),
    # cat_col="diagnosis",
    num_col = "age",
    # id_col="participant",
    list_out = FALSE
  )

  for (i in 1:10) {
    xpectr::set_test_seed(i)
    df_partitioned <- partition(df, 0.5,
      cat_col = "diagnosis", num_col = "age",
      id_col = "participant", list_out = FALSE
    )

    age_distribution <- df_partitioned %>%
      dplyr::group_by(.partitions) %>%
      dplyr::summarise(
        mean_age = mean(age),
        sd_age = sd(age)
      )

    expect_true(is_between_(age_distribution$mean_age[1], 49, 53))
    expect_true(is_between_(age_distribution$mean_age[2], 49, 53))
  }

  for (i in 1:10) {
    xpectr::set_test_seed(i)
    df_partitioned <- partition(df, c(0.2, 0.2, 0.2, 0.2, 0.2),
      cat_col = "diagnosis", num_col = "age",
      id_col = "participant", list_out = FALSE
    )

    age_distribution <- df_partitioned %>%
      dplyr::group_by(.partitions) %>%
      dplyr::summarise(
        mean_age = mean(age),
        sd_age = sd(age)
      )

    expect_true(is_between_(age_distribution$mean_age[1], 47.5, 53.5))
    expect_true(is_between_(age_distribution$mean_age[2], 47.5, 53.5))
    expect_true(is_between_(age_distribution$mean_age[3], 47.5, 53.5))
    expect_true(is_between_(age_distribution$mean_age[4], 47.5, 53.5))
    expect_true(is_between_(age_distribution$mean_age[5], 47.5, 53.5))
  }

  # With two levels of extreme pairing

  for (i in 1:10) {
    xpectr::set_test_seed(i)
    df_partitioned <- partition(df, c(0.2, 0.2, 0.2, 0.2, 0.2),
      cat_col = "diagnosis", num_col = "age",
      id_col = "participant", extreme_pairing_levels = 2,
      list_out = FALSE
    )

    age_distribution <- df_partitioned %>%
      dplyr::group_by(.partitions) %>%
      dplyr::summarise(
        mean_age = mean(age),
        sd_age = sd(age)
      )

    expect_true(is_between_(age_distribution$mean_age[1], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[2], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[3], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[4], 49, 51.5))
    expect_true(is_between_(age_distribution$mean_age[5], 49, 51.5))
  }

  # With three levels of extreme pairing

  for (i in 1:10) {
    xpectr::set_test_seed(i)
    df_partitioned <- partition(df, c(0.2, 0.2, 0.2, 0.2, 0.2),
      cat_col = "diagnosis", num_col = "age",
      id_col = "participant", extreme_pairing_levels = 3,
      list_out = FALSE
    )

    age_distribution <- df_partitioned %>%
      dplyr::group_by(.partitions) %>%
      dplyr::summarise(
        mean_age = mean(age),
        sd_age = sd(age)
      )

    expect_true(is_between_(age_distribution$mean_age[1], 49, 53))
    expect_true(is_between_(age_distribution$mean_age[2], 49, 53))
    expect_true(is_between_(age_distribution$mean_age[3], 49, 53))
    expect_true(is_between_(age_distribution$mean_age[4], 49, 53))
    expect_true(is_between_(age_distribution$mean_age[5], 49, 53))
  }

  # With four levels of extreme pairing
  expect_error(
    partition(df, c(0.2, 0.2, 0.2, 0.2, 0.2),
      cat_col = "diagnosis", num_col = "age",
      id_col = "participant", extreme_pairing_levels = 4,
      list_out = FALSE
    ),
    " 4 levels of extreme pairing. Decrease 'extreme_pairing_levels'."
  )
})

test_that("input checks fuzz testing of partition()", {
  xpectr::set_test_seed(1)

  df <- data.frame(
    "participant" = factor(rep(c("1", "2", "3", "4", "5", "6"), 3)),
    "shuffled_participant" = factor(sample(rep(c("1", "2", "3", "4", "5", "6"), 3))),
    "age" = rep(c(25, 65, 34), 3),
    "diagnosis" = factor(rep(c("a", "b", "a", "a", "b", "b"), 3)),
    "subdiagnosis" = factor(rep(c("x", "x", "x", "y", "y", "y"), 3)),
    "score" = c(34, 23, 54, 23, 56, 76, 43, 56, 76, 42, 54, 1, 5, 76, 34, 76, 23, 65)
  )

  df <- df %>% dplyr::arrange(participant)

  # Add session info
  df$session <- rep(c("1", "2", "3"), 6)

  partition_2 <- function(...){
    d <- partition(...)
    nms <- colnames(d)
    if (is.data.frame(d))
      base_select(d, cols = nms[grepl(".partitions", nms)])
    else
      d
  }

  xpectr::set_test_seed(42)
  # xpectr::gxs_function(partition_2,
  #                      args_values = list(
  #                        "data" = list(df, c(1,1,1,2,2,2,1,1,1,2,2,2),
  #                                      df[df$diagnosis == "a",], NA, matrix(1, 3, 3)),
  #                        "p" = list(0.5, c(0.35, 0.35), 2, 0, -1, NA, "hej", 40),
  #                        "cat_col" = list("diagnosis", "score", "participant", 2, NA,
  #                                         c("diagnosis", "diagnosis"), c("diagnosis", "hej"),
  #                                         c("diagnosis", "subdiagnosis")),
  #                        "num_col" = list(NULL, "score", "participant", "hej", c("participant", "diagnosis"), NA, 1),
  #                        "id_col" = list("participant", "shuffled_participant", "diagnosis",
  #                                        "score", "hej", c("participant", "diagnosis"), NA, 1),
  #                        "id_aggregation_fn" = list(sum, 1, NA), # test mean and identity with num_col specified
  #                        "extreme_pairing_levels" = list(1, 0, NA), # Only makes sense to test >1 with num_col specified
  #                        "force_equal" = list(FALSE, TRUE),
  #                        "list_out" = list(FALSE, TRUE)
  #                      ), indentation = 2)

  ## Testing 'partition_2'                                                    ####
  ## Initially generated by xpectr
  # Testing different combinations of argument values

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)
  # Testing class
  expect_equal(
    class(output_19148),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19148[[".partitions"]],
    structure(c(2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
      2L, 2L, 1L, 1L, 1L), .Label = c("1", "2"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_19148),
    ".partitions",
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
    ".partitions",
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "sco...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "score", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * The value in 'data[[cat_col]]' mus",
                         "t be constant within each ID.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "par...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "participant", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * 'id_col' and 'cat_col' cannot cont",
                         "ain the same column name.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = 2, n...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = 2, num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'cat_col': Must be of typ",
                         "e 'character' (or 'NULL'), not 'double'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = NA, ...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = NA, num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'cat_col': Contains missi",
                         "ng values (element 1).")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = c("d...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = c("diagnosis", "diagnosis"), num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'cat_col': Contains dupli",
                         "cated values, position 2.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = c("d...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = c("diagnosis", "hej"), num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * 'cat_col' column(s), 'hej', not fo",
                         "und in 'data'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = c("d...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Assigning output
  output_11346 <- partition_2(data = df, p = 0.5, cat_col = c("diagnosis", "subdiagnosis"), num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)
  # Testing class
  expect_equal(
    class(output_11346),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_11346[[".partitions"]],
    structure(c(2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
      2L, 2L, 1L, 1L, 1L), .Label = c("1", "2"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_11346),
    ".partitions",
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
    ".partitions",
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = NULL...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Assigning output
  output_16569 <- partition_2(data = df, p = 0.5, cat_col = NULL, num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)
  # Testing class
  expect_equal(
    class(output_16569),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_16569[[".partitions"]],
    structure(c(2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L,
      2L, 2L, 1L, 1L, 1L), .Label = c("1", "2"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_16569),
    ".partitions",
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
    character(0),
    fixed = TRUE)

  # Testing partition_2(data = c(1, 1, 1, 2, 2, 2, 1, 1, 1...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = c(1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2), p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data': Must be of type '",
                         "data.frame', not 'double'.")),
    fixed = TRUE)

  # Testing partition_2(data = df[df$diagnosis == "a", ], ...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Assigning output
  output_14577 <- partition_2(data = df[df$diagnosis == "a", ], p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)
  # Testing class
  expect_equal(
    class(output_14577),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_14577[[".partitions"]],
    structure(c(2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L), .Label = c("1",
      "2"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_14577),
    ".partitions",
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
    ".partitions",
    fixed = TRUE)

  # Testing partition_2(data = NA, p = 0.5, cat_col = "dia...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = NA, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data': Must be of type '",
                         "data.frame', not 'logical'.")),
    fixed = TRUE)

  # Testing partition_2(data = matrix(1, 3, 3), p = 0.5, c...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = matrix(1, 3, 3), p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data': Must be of type '",
                         "data.frame', not 'matrix'.")),
    fixed = TRUE)

  # Testing partition_2(data = NULL, p = 0.5, cat_col = "d...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = NULL, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data': Must be of type '",
                         "data.frame', not 'NULL'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: extreme_pairing_levels
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 0, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'extreme_pairing_levels':",
                         " Must be >= 1.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: extreme_pairing_levels
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = NA, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'extreme_pairing_levels':",
                         " May not be NA.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: extreme_pairing_levels
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = NULL, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'extreme_pairing_levels':",
                         " Must be of type 'count', not 'NULL'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: force_equal
  xpectr::set_test_seed(42)
  # Assigning output
  output_11174 <- partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = TRUE, list_out = FALSE)
  # Testing class
  expect_equal(
    class(output_11174),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_11174[[".partitions"]],
    structure(c(1L, 1L, 1L, 1L, 1L, 1L), .Label = "1", class = "factor"))
  # Testing column names
  expect_equal(
    names(output_11174),
    ".partitions",
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_11174),
    "factor",
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_11174),
    "integer",
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_11174),
    c(6L, 1L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_11174)),
    ".partitions",
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: force_equal
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = NULL, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'force_equal': Must be of",
                         " type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: id_aggregation_fn
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = 1, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'id_aggregation_fn': Must",
                         " be a function, not 'double'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: id_aggregation_fn
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = NA, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'id_aggregation_fn': Must",
                         " be a function, not 'logical'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: id_aggregation_fn
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = NULL, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'id_aggregation_fn': Must",
                         " be a function, not 'NULL'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "shuffled_participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * The value in 'data[[cat_col]]' mus",
                         "t be constant within each ID.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "diagnosis", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * 'id_col' and 'cat_col' cannot cont",
                         "ain the same column name.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "score", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data[[id_col]]': Must be",
                         " of type 'factor', not 'double'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "hej", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * 'id_col' column, 'hej', not found ",
                         "in 'data'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = c("participant", "diagnosis"), id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'id_col': Must have length 1."),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = NA, id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'id_col': May not be NA."),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = 1, id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'id_col': Must be of type",
                         " 'string' (or 'NULL'), not 'double'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Assigning output
  output_18360 <- partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = NULL, id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)
  # Testing class
  expect_equal(
    class(output_18360),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_18360[[".partitions"]],
    structure(c(2L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 2L,
      1L, 2L, 1L, 2L, 1L), .Label = c("1", "2"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_18360),
    ".partitions",
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_18360),
    "factor",
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_18360),
    "integer",
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_18360),
    c(18L, 1L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_18360)),
    character(0),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: list_out
  xpectr::set_test_seed(42)
  # Assigning output
  output_17375 <- partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = TRUE)
  # Testing class
  expect_equal(
    class(output_17375),
    "list",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_17375,
    type = "list")
  # Testing values
  expect_equal(
    output_17375,
    list(structure(list(participant = structure(c(4L, 4L, 4L, 6L, 6L,
      6L), .Label = c("1", "2", "3", "4", "5", "6"), class = "factor"),
      shuffled_participant = structure(c(2L, 1L, 6L, 6L, 5L, 3L),
          .Label = c("1", "2", "3", "4", "5", "6"), class = "factor"),
      age = c(25, 25, 25, 34, 34, 34), diagnosis = structure(c(1L,
          1L, 1L, 2L, 2L, 2L), .Label = c("a", "b"), class = "factor"),
      subdiagnosis = structure(c(2L, 2L, 2L, 2L, 2L, 2L), .Label = c("x",
          "y"), class = "factor"), score = c(23, 42, 76, 76, 1, 65),
      session = c("1", "2", "3", "1", "2", "3")), row.names = c(NA,
      -6L), class = c("tbl_df", "tbl", "data.frame")), structure(list(participant = structure(c(1L,
      1L, 1L, 3L, 3L, 3L, 2L, 2L, 2L, 5L, 5L, 5L), .Label = c("1",
      "2", "3", "4", "5", "6"), class = "factor"), shuffled_participant = structure(c(5L,
      1L, 6L, 4L, 5L, 4L, 1L, 2L, 4L, 3L, 2L, 3L), .Label = c("1",
      "2", "3", "4", "5", "6"), class = "factor"), age = c(25, 25,
      25, 34, 34, 34, 65, 65, 65, 65, 65, 65), diagnosis = structure(c(1L,
      1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("a",
      "b"), class = "factor"), subdiagnosis = structure(c(1L, 1L,
      1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L), .Label = c("x", "y"),
      class = "factor"), score = c(34, 43, 5, 54, 76, 34, 23, 56,
      76, 56, 54, 23), session = c("1", "2", "3", "1", "2", "3", "1",
      "2", "3", "1", "2", "3")), row.names = c(NA, -12L), class = c("tbl_df",
      "tbl", "data.frame"))))
  # Testing names
  expect_equal(
    names(output_17375),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_17375),
    2L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_17375)),
    14L)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: list_out
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = NULL)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'list_out': Must be of ty",
                         "pe 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: num_col
  xpectr::set_test_seed(42)
  # Assigning output
  output_13881 <- partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = "score", id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)
  # Testing class
  expect_equal(
    class(output_13881),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_13881[[".partitions"]],
    structure(c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
    2L, 2L, 1L, 1L, 1L), .Label = c("1", "2"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_13881),
    ".partitions",
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_13881),
    "factor",
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_13881),
    "integer",
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_13881),
    c(18L, 1L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_13881)),
    character(0),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: num_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = "participant", id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip("1 assertions failed:\n * 'num_col' column must be numeric."),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: num_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = "hej", id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * 'num_col' column, 'hej', not found",
                         " in 'data'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: num_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = c("participant", "diagnosis"), id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'num_col': Must have length 1."),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: num_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = NA, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'num_col': May not be NA."),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0.5, cat_col = "dia...
  # Changed from baseline: num_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0.5, cat_col = "diagnosis", num_col = 1, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'num_col': Must be of typ",
                         "e 'string' (or 'NULL'), not 'double'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = "hej", cat_col = "d...
  # Changed from baseline: p
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = "hej", cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'p': Must be of type 'num",
                         "eric', not 'character'.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = 40, cat_col = "diag...
  # Changed from baseline: p
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 40, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip("1 assertions failed:\n * Variable 'p': Element 1 is not <= 18."),
    fixed = TRUE)

  # Testing partition_2(data = df, p = c(0.35, 0.35), cat_...
  # Changed from baseline: p
  xpectr::set_test_seed(42)
  # Assigning output
  output_13795 <- partition_2(data = df, p = c(0.35, 0.35), cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)
  # Testing class
  expect_equal(
    class(output_13795),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_13795[[".partitions"]],
    structure(c(3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L, 3L, 3L, 3L, 2L,
      2L, 2L, 1L, 1L, 1L), .Label = c("1", "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_13795),
    ".partitions",
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
    ".partitions",
    fixed = TRUE)

  # Testing partition_2(data = df, p = 2, cat_col = "diagn...
  # Changed from baseline: p
  xpectr::set_test_seed(42)
  # Assigning output
  output_14357 <- partition_2(data = df, p = 2, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)
  # Testing class
  expect_equal(
    class(output_14357),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_14357[[".partitions"]],
    structure(c(2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L,
      1L, 1L, 1L, 1L, 1L), .Label = c("1", "2"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_14357),
    ".partitions",
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
    ".partitions",
    fixed = TRUE)

  # Testing partition_2(data = df, p = 0, cat_col = "diagn...
  # Changed from baseline: p
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = 0, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'p': Element 1 is not >= ",
                         "0.0555556.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = -1, cat_col = "diag...
  # Changed from baseline: p
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = -1, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'p': Element 1 is not >= ",
                         "0.0555556.")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = NA, cat_col = "diag...
  # Changed from baseline: p
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = NA, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'p': Contains missing val",
                         "ues (element 1).")),
    fixed = TRUE)

  # Testing partition_2(data = df, p = NULL, cat_col = "di...
  # Changed from baseline: p
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(partition_2(data = df, p = NULL, cat_col = "diagnosis", num_col = NULL, id_col = "participant", id_aggregation_fn = sum, extreme_pairing_levels = 1, force_equal = FALSE, list_out = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'p': Must be of type 'num",
                         "eric', not 'NULL'.")),
    fixed = TRUE)

  ## Finished testing 'partition_2'                                           ####
  #
})


test_that("partition() works with group_by()", {
  xpectr::set_test_seed(1)

  df <- data.frame(
    "participant" = factor(rep(c("1", "2", "3", "4", "5", "6"), 3)),
    "age" = rep(c(25, 65, 34), 3),
    "diagnosis" = factor(rep(c("a", "b", "a", "a", "b", "b"), 3)),
    "score" = c(34, 23, 54, 23, 56, 76, 43, 56, 76, 42, 54, 1, 5, 76, 34, 76, 23, 65)
  )

  ## Testing 'xpectr::suppress_mw( df %>% dplyr::group_by(...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_12655 <- xpectr::suppress_mw(
      df %>%
        dplyr::group_by(diagnosis) %>%
        partition(p=0.8, id_col = "participant")
    )
  # Testing class
  expect_equal(
    class(output_12655),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_12655[["participant"]],
    structure(c(1L, 1L, 1L, 3L, 3L, 3L, 4L, 4L, 4L, 2L, 2L, 2L, 5L,
      5L, 5L, 6L, 6L, 6L), .Label = c("1", "2", "3", "4", "5", "6"),
      class = "factor"))
  expect_equal(
    output_12655[["age"]],
    c(25, 25, 25, 34, 34, 34, 25, 25, 25, 65, 65, 65, 65, 65, 65, 34,
      34, 34),
    tolerance = 1e-4)
  expect_equal(
    output_12655[["diagnosis"]],
    structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
      2L, 2L, 2L, 2L, 2L), .Label = c("a", "b"), class = "factor"))
  expect_equal(
    output_12655[["score"]],
    c(34, 5, 43, 54, 76, 34, 42, 23, 76, 23, 76, 56, 56, 54, 23, 1,
      76, 65),
    tolerance = 1e-4)
  expect_equal(
    output_12655[[".partitions"]],
    structure(c(2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L,
      1L, 1L, 1L, 1L, 1L), .Label = c("1", "2"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_12655),
    c("participant", "age", "diagnosis", "score", ".partitions"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_12655),
    c("factor", "numeric", "factor", "numeric", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_12655),
    c("integer", "double", "integer", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_12655),
    c(18L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_12655)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'xpectr::suppress_mw( df %>% dplyr::group_by(...'     ####


})

