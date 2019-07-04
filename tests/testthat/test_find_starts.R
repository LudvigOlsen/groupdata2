library(groupdata2)
context("find_starts()")

test_that("find_starts() find the right starts", {

  skip_test_if_old_R_version()

  v <- c('a','a','b','c','c','d','d')
  df <- data.frame(v = v,
                   v2 = c(1,1,1,2,2,2,3),
                   v3 = factor(c(1,1,1,2,2,2,3)),
                   stringsAsFactors = FALSE)

  check_find_starts_df <- function(col, return_index = FALSE){

    return(find_starts(df, col = col, return_index = return_index))

  }

  # v
  expect_equal(check_find_starts_df('v'), c('a','b','c','d'))
  expect_equal(check_find_starts_df('v', return_index = TRUE), c(1,3,4,6))

  # v2
  expect_equal(check_find_starts_df('v2'), c(1,2,3))
  expect_equal(check_find_starts_df('v2', return_index = TRUE), c(1,4,7))

  # v3 - notice: find_starts() converts factors to characters

  expect_warning(expect_equal(check_find_starts_df('v3'), c("1","2","3")),
                 "col is factor. Using as character.", fixed = TRUE)

  expect_warning(expect_equal(check_find_starts_df('v3', return_index = TRUE),
                              c(1,4,7)),
                 "col is factor. Using as character.", fixed = TRUE)

  expect_error(find_starts(df),
               "col must be specified when data is data frame",
               fixed = TRUE)

  expect_warning(find_starts(v, col = 'a'),
                 "col not used as data is not a data frame",
                 fixed = TRUE)

  expect_warning(find_starts(factor(v)),
                 "data is factor. Using as character.",
                 fixed = TRUE)

  expect_warning(find_starts(factor(v), return_index = TRUE),
                 "data is factor. Using as character.",
                 fixed = TRUE)

  expect_warning(find_starts(df, col = 'v3'),
                 "col is factor. Using as character.",
                 fixed = TRUE)

  expect_warning(find_starts(df, col = 'v3', return_index = TRUE),
                 "col is factor. Using as character.",
                 fixed = TRUE)

})
