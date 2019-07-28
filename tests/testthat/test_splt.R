library(groupdata2)
context("splt()")

test_that("dimensions of output with splt()", {

  set_seed_for_R_compatibility(1)

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

  # There should be n elements in the list
  expect_equal(length(splt(df, 0, allow_zero = T)), 1)
  expect_equal(nrow(splt(df, 0, allow_zero = T)[[1]]), 12)

})


test_that("splt() works with force_equal on vector", {

  set_seed_for_R_compatibility(1)

  splt_equal <- function(data, n, method){

    splits <- splt(data, n, method,
                   force_equal = T)

    counts <- plyr::llply(splits, function(s){
      return(length(s))
    })

    counts <- unlist(counts)

    names(counts) <- NULL

    return(counts)
  }

  expect_equal(splt_equal( c(1:10), 3, 'greedy'), c(3,3,3) )
  expect_equal(splt_equal( c(1:10), .3, 'greedy'), c(3,3,3) )
  expect_equal(splt_equal( c(1:10), 3, 'n_dist'), c(3,3,3) )
  expect_equal(splt_equal( c(1:10), .3, 'n_dist'), c(3,3,3) )
  expect_equal(splt_equal( c(1:10), 3, 'n_fill'), c(3,3,3) )
  expect_equal(splt_equal( c(1:10), .3, 'n_fill'), c(3,3,3) )
  expect_equal(splt_equal( c(1:10), 3, 'n_last'), c(3,3,3) )
  expect_equal(splt_equal( c(1:10), .3, 'n_last'), c(3,3,3) )
  expect_equal(splt_equal( c(1:10), 3, 'n_rand'), c(3,3,3) )
  expect_equal(splt_equal( c(1:10), .3, 'n_rand'), c(3,3,3) )
  expect_equal(splt_equal( c(1:10), 3, 'l_sizes'), c(3) )
  expect_equal(splt_equal( c(1:10), c(0.2,0.3), 'l_sizes'), c(2,3) )
  # l_starts shouldn't cut any values.
  expect_equal(splt_equal( c(1:10), c(3,5), 'l_starts'), c(2,2,6) )

  expect_equal(splt_equal( c(1:57), 5, 'staircase'), c(5,10,15,20) )
  expect_equal(splt_equal( c(1:57), 0.2, 'staircase'), c(11,22) )
  expect_equal(splt_equal( c(1:57), 5, 'primes'), c(5,7,11,13,17) )

})

test_that("splt() works with force_equal on vector", {

  set_seed_for_R_compatibility(1)

  splt_equal <- function(data, n, method, starts_col = NULL){

    splits <- splt(data, n, method,
                   force_equal = T,
                   starts_col = starts_col)

    counts <- plyr::llply(splits, function(s){
      return(nrow(s))
    })

    counts <- unlist(counts)

    names(counts) <- NULL

    return(counts)
  }

  df <- data.frame("participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
                   "age" = rep(c(25,65,34), 3),
                   "diagnosis" = rep(c('a', 'b', 'a', 'a', 'b', 'b'), 3),
                   "score" = c(34,23,54,23,56,76,43,56,76,42,54,1,5,76,34,76,23,65))


  expect_equal(splt_equal( df, 3, 'greedy'), c(3,3,3,3,3,3) )
  expect_equal(splt_equal( df, .2, 'greedy'), c(3,3,3,3,3,3) )
  expect_equal(splt_equal( df, 3, 'n_dist'), c(6,6,6) )
  expect_equal(splt_equal( df, .2, 'n_dist'), c(6,6,6) )
  expect_equal(splt_equal( df, 3, 'n_fill'), c(6,6,6) )
  expect_equal(splt_equal( df, .2, 'n_fill'), c(6,6,6) )
  expect_equal(splt_equal( df, 3, 'n_last'), c(6,6,6) )
  expect_equal(splt_equal( df, .2, 'n_last'), c(6,6,6) )
  expect_equal(splt_equal( df, 3, 'n_rand'), c(6,6,6) )
  expect_equal(splt_equal( df, .2, 'n_rand'), c(6,6,6) )
  expect_equal(splt_equal( df, 3, 'l_sizes'), c(3) )
  expect_equal(splt_equal( df, c(0.2,0.3), 'l_sizes'), c(3,5) )
  # l_starts shouldn't cut any values.
  expect_equal(splt_equal( df, c(3,5), 'l_starts', starts_col = 1), c(2,2,14) )

  expect_equal(splt_equal( df, 5, 'staircase'), c(5,10) )
  expect_equal(splt_equal( df, 0.2, 'staircase'), c(3,6,9) )
  expect_equal(splt_equal( df, 5, 'primes'), c(5,7) )

})

