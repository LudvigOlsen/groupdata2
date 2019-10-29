library(groupdata2)
context("remove_identical_cols()")

# tests handling of identical columns - especially for repeated folds creation
test_that("finding identical columns work with find_identical_cols()",{

  df <-
    data.frame(
      "a" = c(1, 1, 2),
      "b" = c(1, 1, 3),
      "c" = c(1, 1, 2),
      "d" = c(2, 2, 1),
      "e" = c(1, 1, 2),
      "f" = c(1, 1, 2),
      "g" = c(1, 1, 3)
    )

  # colnames(unique(as.matrix(df), MARGIN=2))

  expect_equal(find_identical_cols(df), data.frame("V1"=c("a","a","a","b","c","c","e"),
                                                   "V2"=c("c","e","f","g","e","f","f"),
                                                   stringsAsFactors = FALSE))
  expect_equal(find_identical_cols(df, cols=c(1:7)),
               data.frame("V1" = as.integer(c(1,1,1,2,3,3,5)),
                          "V2" = as.integer(c(3,5,6,7,5,6,6))))

  })

test_that("removing identical columns work with remove_identical_cols()",{

  df <- data.frame("a"=c(1,1,2), "b"=c(1,1,3), "c"=c(1,1,2), "d"=c(2,2,1), "e"=c(1,1,2), "f"=c(1,1,2), "g"=c(1,1,3))

  expect_equal(remove_identical_cols(df), df[c("a","b","d")])
  expect_equal(remove_identical_cols(df, cols=c(1:7)), df[c("a","b","d")])
  expect_equal(remove_identical_cols(df, cols=c(2:7)), df[c("a","b","c","d")])

})

test_that("removing columns with identical but differently named groups work with remove_identical_cols()",{

  df <- data.frame("a"=c(1,1,2), "b"=c(1,1,3), "c"=c(1,1,2), "d"=c(2,2,1),
                   "e"=c(1,1,2), "f"=c(1,1,2), "g"=c(1,1,3), stringsAsFactors = FALSE)

  expect_equal(remove_identical_cols(df, group_wise=FALSE), df[c("a","b","d")])
  expect_equal(remove_identical_cols(df, group_wise=TRUE), df[c("a")])
  expect_equal(remove_identical_cols(df, cols=c(1:7), group_wise=TRUE), df[c("a")])
  expect_equal(remove_identical_cols(df, cols=c(2:7), group_wise=TRUE), df[c("a","b")])

  df <- data.frame("a"=c(1,1,2,3), "b"=c(1,2,1,3), "c"=c(2,1,1,2), "d"=c(2,2,1,3),
                   "e"=c(1,3,1,2), "f"=c(3,1,1,3), "g"=c(1,1,3,4))

  expect_equal(remove_identical_cols(df, group_wise=FALSE), df[c("a","b","c","d","e","f","g")])
  expect_equal(remove_identical_cols(df, group_wise=TRUE), df[c("a","b","c")])
  expect_equal(remove_identical_cols(df, cols=c(2:7), group_wise=TRUE), df[c("a","b","c","d")])
  expect_equal(remove_identical_cols(df, cols=c(3:7), group_wise=TRUE), df[c("a","b","c","d","e")])

})
