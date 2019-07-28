library(groupdata2)
context("find_missing_starts()")

test_that("find_missing_starts() find the right missing starts", {

  set_seed_for_R_compatibility(1)

  v <- c('a','a','b','c','c','d','d')
  df <- data.frame(v = v,
                   v2 = c(1,1,1,2,2,2,3),
                   v3 = factor(c(1,1,1,2,2,2,3)),
                   stringsAsFactors = FALSE)

  check_find_missing_starts_df <- function(n, starts_col, return_skip_numbers=FALSE){

    return(find_missing_starts(df, n=n, starts_col = starts_col,
                               return_skip_numbers = return_skip_numbers))

  }

  find_missing_starts(df, n=c("r","d"), starts_col="v")

  # v
  expect_equal(check_find_missing_starts_df(n=c("r","d"), starts_col="v"), c('r'))
  expect_equal(check_find_missing_starts_df(n=c("r","d"), starts_col="v",
                                            return_skip_numbers=TRUE), list(c('r',"1")))
  expect_equal(check_find_missing_starts_df(n=c("b","r","o","d"), starts_col="v"), c('r','o'))
  expect_equal(check_find_missing_starts_df(n=c("b","r","o","d"), starts_col="v",
                                            return_skip_numbers=TRUE), list(c('r',"1"),c("o","1")))

  # v2
  expect_equal(check_find_missing_starts_df(n=c(1,3,5), starts_col = "v2"), c(5))
  expect_equal(check_find_missing_starts_df(n=c(1,3,5), starts_col = "v2",
                                            return_skip_numbers=TRUE), list(c(5,1)))
  expect_equal(check_find_missing_starts_df(n=c(5,2,1), starts_col = "v2"), c("5")) # TODO Why character type here?
  expect_equal(check_find_missing_starts_df(n=c(5,3,1), starts_col = "v2",
                                            return_skip_numbers=TRUE), list(c("5","1")))


  expect_equal(check_find_missing_starts_df(n=c(1,5,3,7), starts_col = 'v3'), c(5,7))

  # Example from docs:

  # Create a data frame
  df <- data.frame('a' = c('a','a','b',
                           'b','c','c'))

  # Create list of starts
  starts <- c("a", "e", "b", "d", "c")

  # Find missing starts with skip_to numbers
  expect_equal(find_missing_starts(df, starts, starts_col = 'a'), list(c("e","1"),c("d","1")))

  # Find missing starts without skip_to numbers
  expect_equal(find_missing_starts(df, starts, starts_col = 'a',
                      return_skip_numbers = FALSE),
               c("e","d"))

  # Warning and errors
  expect_error(find_missing_starts(c(1,2,3,4), starts_col = "v"),
               "'starts_col' should only be specified when 'data' is a data frame.", fixed=TRUE)
  expect_error(find_missing_starts(c(1,2,3,4), n = c("a"), starts_col = "v"),
               "'starts_col' should only be specified when 'data' is a data frame.", fixed=TRUE)
  expect_error(find_missing_starts(df, starts_col = NULL),
               "When 'data' is a data frame, 'starts_col' must be specified.", fixed=TRUE)
  expect_error(find_missing_starts(df, n = c("a"), starts_col = NULL),
               "When 'data' is a data frame, 'starts_col' must be specified.", fixed=TRUE)
  expect_error(find_missing_starts(c(1,2,3,4), n=data.frame("a"=c(3,4,5))),
               "'n' must be either a list or a vector.", fixed=TRUE)
  expect_error(find_missing_starts(c(1,2,3,4), n=c(3), return_skip_numbers = 3),
               "'return_skip_numbers' must be logical (TRUE/FALSE).", fixed=TRUE)





})
