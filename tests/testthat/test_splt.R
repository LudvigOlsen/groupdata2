library(groupdata2)
context("splt()")

test_that("dimensions of output with splt()", {

  df <- data.frame("x"=c(1:12),
                   "species" = rep(c('cat','pig', 'human'), 4),
                   "age" = c(5,65,34,54,32,54,23,65,23,65,87,98))

  get_element_sizes <- function(df, n){

    sizes <- plyr::llply(splt(df, n), function(d){

      return(nrow(d))

    })

    return(unname(unlist(sizes)))
  }

  # There should be no columns in the returned object
  expect_equal(ncol(splt(df, 3)), NULL)

  # There should be n elements in the list
  expect_equal(length(splt(df, 3)), 3)

  # Check that there are the right amount of rows in list elements
  expect_equal(get_element_sizes(df, 3), c(4,4,4))


})
