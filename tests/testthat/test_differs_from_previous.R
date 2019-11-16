library(groupdata2)
context("differs_from_previous()")

test_that("differs_from_previous() find the right values and indices", {

  set_seed_for_R_compatibility(1)

  v <- c('a','a','b','c','c','d','d')
  df <- data.frame(v = v,
                   v2 = c(1,1,1,2,2,2,3),
                   v3 = factor(c(1,1,1,2,2,2,3)),
                   stringsAsFactors = FALSE)

  check_differs_from_previous <- function(col,
                                          threshold=NULL,
                                          direction="both",
                                          return_index = FALSE,
                                          include_first = FALSE,
                                          factor_conversion_warning=FALSE){

    return(differs_from_previous(df, col = col,
                                 threshold=threshold,
                                 direction=direction,
                                 return_index = return_index,
                                 include_first=include_first,
                                 factor_conversion_warning=factor_conversion_warning))

  }

  # v
  expect_equal(check_differs_from_previous('v'), c('b','c','d'))
  expect_equal(check_differs_from_previous('v', return_index = TRUE), c(3,4,6))
  expect_equal(check_differs_from_previous('v', include_first=TRUE), c('a','b','c','d'))
  expect_equal(check_differs_from_previous('v', include_first=TRUE, return_index = TRUE), c(1,3,4,6))

  # v2
  expect_equal(check_differs_from_previous('v2'), c(2,3))
  expect_equal(check_differs_from_previous('v2', return_index = TRUE), c(4,7))
  expect_equal(check_differs_from_previous('v2', threshold = 10), numeric(0))
  expect_equal(check_differs_from_previous('v2', threshold = 10, include_first=TRUE), 1)
  expect_equal(check_differs_from_previous('v2', include_first=TRUE, return_index = TRUE), c(1,4,7))

  # v3 - notice: converts factors to characters

  expect_equal(check_differs_from_previous('v3'), c("2","3"))
  expect_warning(expect_equal(check_differs_from_previous('v3', factor_conversion_warning = TRUE),
                              c("2","3")),
                 "col is factor. Using as character.", fixed = TRUE)
  expect_warning(expect_equal(check_differs_from_previous('v3', factor_conversion_warning = TRUE, include_first=TRUE),
                              c("1","2","3")),
                 "col is factor. Using as character.", fixed = TRUE)

  expect_warning(expect_equal(check_differs_from_previous('v3', return_index = TRUE, factor_conversion_warning = TRUE),
                              c(4,7)),
                 "col is factor. Using as character.", fixed = TRUE)

  expect_error(
    check_differs_from_previous('v3', threshold=2, return_index = TRUE,
                                factor_conversion_warning = TRUE),
                 "col is factor. 'threshold' must be NULL. Alternatively, convert factor to numeric vector.", fixed = TRUE)

  expect_error(differs_from_previous(df),
               "col must be specified when data is data frame",
               fixed = TRUE)

  expect_warning(differs_from_previous(v, col = 'a'),
                 "col not used as data is not a data frame",
                 fixed = TRUE)

  expect_warning(differs_from_previous(factor(v)),
                 "data is factor. Using as character.",
                 fixed = TRUE)

  expect_warning(differs_from_previous(factor(v), return_index = TRUE),
                 "data is factor. Using as character.",
                 fixed = TRUE)

  expect_warning(differs_from_previous(df, col = 'v3'),
                 "col is factor. Using as character.",
                 fixed = TRUE)

  expect_warning(differs_from_previous(df, col = 'v3', return_index = TRUE),
                 "col is factor. Using as character.",
                 fixed = TRUE)

  expect_error(differs_from_previous(df, col="v", threshold = c(1,2,3)),
               "'threshold' must be numeric scalar, a numeric vector of length 2, or NULL.",
               fixed = TRUE)
  expect_error(differs_from_previous(df, col="v", threshold = c("a","b","c")),
               "'threshold' must be numeric scalar, a numeric vector of length 2, or NULL.",
               fixed = TRUE)
  expect_error(differs_from_previous(df, col="v", threshold = -2),
               "When 'threshold' is a scalar it must be a positive number.",
               fixed = TRUE)
  expect_error(differs_from_previous(df, col="v", threshold = c(1,-2)),
               "When 'threshold' is a vector of length 2, the first element must be negative.",
               fixed = TRUE)
  expect_error(differs_from_previous(df, col="v", threshold = c(-1,-2)),
               "When 'threshold' is a vector of length 2, the second element must be positive.",
               fixed = TRUE)
  expect_error(differs_from_previous(df, col="lol", threshold = c(-1,2)),
               "col was not found in data frame.",
               fixed = TRUE)
  expect_error(differs_from_previous(df, col="v2", threshold = 10, direction="greater"),
               "'direction' must be one of 'both', 'negative', and 'positive'.",
               fixed = TRUE)
  expect_error(differs_from_previous(df, col="v2", threshold = c(-10,10), direction="negative"),
               "When 'threshold' is a vector of length 2, 'direction' must be 'both'.",
               fixed = TRUE)
  expect_error(differs_from_previous(df, col="v2", threshold = c(-10,10), direction="positive"),
               "When 'threshold' is a vector of length 2, 'direction' must be 'both'.",
               fixed = TRUE)

  expect_equal(differs_from_previous(df, col="v2", threshold = c(1), direction="positive"),
               c(2,3))
  expect_equal(differs_from_previous(df, col="v2", threshold = c(1), direction="negative"),
               numeric())

})

test_that("differs_from_previous() work with NAs", {

  set_seed_for_R_compatibility(1)

  v <- c('a','a','b','c',NA,'d','d',NA,"e","e")
  df <- data.frame(v = v,
                   v2 = c(1,1,1,2,NA,2,3,NA,4,4),
                   v3 = factor(c(1,1,1,2,NA,2,3,NA,4,4)),
                   stringsAsFactors = FALSE)

  check_differs_from_previous <- function(col,
                                          threshold=NULL,
                                          direction="both",
                                          return_index = FALSE,
                                          include_first = FALSE,
                                          handle_na = "remove",
                                          factor_conversion_warning=FALSE){

    return(differs_from_previous(df, col = col,
                                 threshold=threshold,
                                 direction=direction,
                                 return_index = return_index,
                                 include_first=include_first,
                                 handle_na=handle_na,
                                 factor_conversion_warning=factor_conversion_warning))

  }

  # return start vals
  expect_equal(find_different_from_previous_vec_(df$v, handle_na = "ignore"), c('b','c','d','e'))
  # Ensure that 1,NA,1 with 'ignore' does not create two group starts
  expect_equal(find_different_from_previous_vec_(c(0,1,NA,1,2,NA,3), handle_na = "ignore"), c(1,2,3))
  expect_equal(find_different_from_previous_vec_(df$v, handle_na = "as_element"),
               c('b','c',NA,'d',NA,'e'))
  expect_equal(find_different_from_previous_vec_(df$v, handle_na = -1),
               c('b','c',"-1",'d',"-1",'e'))
  expect_equal(find_different_from_previous_vec_(df$v2, handle_na = -1),
               c(2, -1, 2, 3, -1, 4))
  expect_equal(check_differs_from_previous('v3', handle_na = -1),
               as.character(c(2, -1, 2, 3, -1, 4)))

  # return start indices
  expect_equal(find_different_from_previous_vec_(df$v, handle_na = "ignore",
                                                 return_index = TRUE), c(3,4,6,9))
  expect_equal(find_different_from_previous_vec_(c(0,1,NA,1,2,NA,3), handle_na = "ignore",
                                                 return_index = TRUE), c(2,5,7))
  expect_equal(find_different_from_previous_vec_(df$v, handle_na = "as_element",
                                                 return_index = TRUE),
               c(3,4,5,6,8,9))
  expect_equal(find_different_from_previous_vec_(df$v, handle_na = -1,
                                                 return_index = TRUE),
               c(3,4,5,6,8,9))
  expect_equal(find_different_from_previous_vec_(df$v2, handle_na = -1,
                                                 return_index = TRUE),
               c(4,5,6,7,8,9))
  expect_equal(check_differs_from_previous('v3', handle_na = -1,
                                           return_index = TRUE),
               c(4,5,6,7,8,9))

  # error
  expect_error(find_different_from_previous_vec_(df$v, handle_na = "leave"),
               "'handle_na' must be either a method ('ignore' or 'convert') or a value to replace NAs with.",
               fixed=TRUE)



})

