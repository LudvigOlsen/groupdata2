library(groupdata2)
context("group()")

# Needs testing of vector and factor as input

test_that("dimensions of data frame with group()", {

  skip_test_if_old_R_version()

  df <- data.frame("x"=c(1:12),
                   "species" = rep(c('cat','pig', 'human'), 4),
                   "age" = c(5,65,34,54,32,54,23,65,23,65,87,98))

  # The added grouping factor means we should get and extra column
  expect_equal(ncol(group(df, 5)), 4)

  # We expect the same amount of rows
  expect_equal(nrow(group(df, 5)), 12)

  # Outputted rows with force_equal = TRUE
  expect_equal(nrow(group(df, 5, force_equal = TRUE)), 10)
  expect_equal(nrow(group(df, 7, force_equal = TRUE)), 7)
  expect_equal(nrow(group(df, 4, force_equal = TRUE)), 12)

})

test_that("mean age of groups made with group()", {

  skip_test_if_old_R_version()

  # Create df 3x12
  df <- data.frame("x"=c(1:12),
                   "species" = rep(c('cat','pig', 'human'), 4),
                   "age" = c(5,65,34,54,32,54,23,65,23,65,87,98))

  int_mean_age <- function(df, n, method){

    df_means <- group(df, n, method = method)
    df_means <- dplyr::summarise(df_means, mean_age = mean(age))

    return(as.integer(df_means$mean_age))
  }

  #group(df, 5, method = 'n_fill')

  expect_equal(int_mean_age(df, 5, 'n_dist'), c(35,44,36,44,83))
  expect_equal(int_mean_age(df, 5, 'n_fill'), c(34,46,44,44,92))
  expect_equal(int_mean_age(df, 5, 'n_last'), c(35,44,43,44,68))

  expect_equal(int_mean_age(df, 7, 'n_dist'), c(5, 49, 43, 54, 44, 44, 92))
  expect_equal(int_mean_age(df, 7, 'n_fill'), c(35, 44, 43, 44, 44, 87, 98))
  expect_equal(int_mean_age(df, 7, 'n_last'), c(5, 65, 34, 54, 32, 54, 60))

  # For n_rand test how many groups has been made
  expect_equal(length(int_mean_age(df, 5, 'n_rand')), 5)
  expect_equal(length(int_mean_age(df, 7, 'n_rand')), 7)


})

test_that("error messages work in group()", {

  skip_test_if_old_R_version()

  # Create df 3x12
  df <- data.frame("x"=c(1:12),
                   "species" = rep(c('cat','pig', 'human'), 4),
                   "age" = c(5,65,34,54,32,54,23,65,23,65,87,98))

  expect_error(group(df, 13), "nrow(data) >= n is not TRUE", fixed=TRUE)
  expect_error(group(df, 0), "n > 0 is not TRUE", fixed=TRUE)

})

test_that("allow_zero works in group()", {

  skip_test_if_old_R_version()

  # Create df 3x12
  df <- data.frame("x"=c(1:12),
                   "species" = rep(c('cat','pig', 'human'), 4),
                   "age" = c(5,65,34,54,32,54,23,65,23,65,87,98))

  group_zero <- function(force_equal=FALSE){

    return(group(df, 0, allow_zero = TRUE,
                 force_equal = force_equal))
  }

  na_col <- function(){

    grouped_df <- group(df, 0, allow_zero = TRUE)

    return(grouped_df$.groups)

  }

  # Check that the .groups column contains NAs
  expect_equal(na_col(), c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))

  # We should still get the added grouping factor
  expect_equal(ncol(group_zero()), 4)
  # We should still have the same amount of rows
  expect_equal(nrow(group_zero()), 12)

  # The same with force_equal as there are no group sizes to force equal
  expect_equal(ncol(group_zero(force_equal = TRUE)), 4)
  expect_equal(nrow(group_zero(force_equal = TRUE)), 12)




})

test_that("col_name can be set correctly in group()",{

  skip_test_if_old_R_version()

  # Create df 3x12
  df <- data.frame("x"=c(1:12),
                   "species" = rep(c('cat','pig', 'human'), 4),
                   "age" = c(5,65,34,54,32,54,23,65,23,65,87,98))

  set_col_name <- function(df){

    grouped_data <- group(df, 5, col_name = '.cats')

    return(colnames(grouped_data[4]))

  }

  expect_equal(set_col_name(df), '.cats')

})

test_that("l_starts can take n = auto", {

  skip_test_if_old_R_version()

  df <- data.frame("x"=c(1:12),
                   "species" = rep(c('cat','cat', 'human', 'human'), 3),
                   "age" = c(5,65,34,54,32,54,23,65,23,65,87,98),
                   stringsAsFactors = FALSE)

  expect_equal(group(df, n = 'auto', method = 'l_starts',
                     starts_col = 'species')$.groups,
               factor(c(1,1,2,2,3,3,4,4,5,5,6,6)))

  expect_error(group(df, n = 'auto', method = 'l_sizes',
                     starts_col = 'species')$.groups,
               "!is.character(n) is not TRUE",
               fixed = TRUE
              )

})

test_that("l_starts can take starts_col = index / .index", {

  skip_test_if_old_R_version()

  df <- data.frame("x"=c(1:12))

  # index
  expect_equal(group(df, c(1,4,7), method = 'l_starts',
                     starts_col = 'index')$.groups,
               factor(c(1,1,1,2,2,2,3,3,3,3,3,3)))

  # .index
  expect_equal(group(df, c(1,4,7), method = 'l_starts',
                     starts_col = '.index')$.groups,
               factor(c(1,1,1,2,2,2,3,3,3,3,3,3)))

  df2 <- data.frame("x"=c(1:12),
                    'index' = c(2:13),
                    '.index' = c(3:14))

  expect_warning(expect_equal(group(df2, c(2,7,11), method = 'l_starts',
                     starts_col = '.index')$.groups,
               factor(c(1,2,2,2,2,2,3,3,3,3,4,4))),
               "data contains column named \'.index\' but this is ignored.", fixed = TRUE)

  expect_warning(expect_equal(group(df2, c(2,7,11), method = 'l_starts',
                                    starts_col = 'index')$.groups,
                              factor(c(1,1,1,1,1,2,2,2,2,3,3,3))),
                 "data contains column named \'index\'. This is used as starts_col instead", fixed = TRUE)


})
