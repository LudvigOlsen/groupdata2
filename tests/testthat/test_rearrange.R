library(groupdata2)
context("rearrange()")

test_that("rearrange() works", {

  # Create data frame
  set.seed(1)
  df <- data.frame(
    "participant" = factor(c(1, 3, 5, 6, 7, 8, 9)),
    "score" = c(79,85,140,69,87,92,87))
  df <- df %>% dplyr::arrange(score)

  # with unequal number of rows
  df_rearranged <- rearrange(df, method="pair_extremes",
                             unequal_method = "first",
                             drop_rearrange_factor = FALSE)
  expect_equal(df_rearranged$.rearrange_factor, c(1,2,2,3,3,4,4))
  expect_equal(df_rearranged$participant, factor(c(6,1,5,3,8,7,9)))
  expect_equal(df_rearranged$score, c(69,79,140,85,92,87,87))

  df_rearranged <- rearrange(df, method="pair_extremes",
                             unequal_method = "middle",
                             drop_rearrange_factor = FALSE)
  expect_equal(df_rearranged$.rearrange_factor, c(1,1,2,2,3,4,4))
  expect_equal(df_rearranged$participant, factor(c(6,5,1,8,7,3,9)))
  expect_equal(df_rearranged$score, c(69,140,79,92,87,85,87))

  df_rearranged <- rearrange(df, method="pair_extremes",
                             unequal_method = "last",
                             drop_rearrange_factor = FALSE)
  expect_equal(df_rearranged$.rearrange_factor, c(1,1,2,2,3,3,4))
  expect_equal(df_rearranged$participant, factor(c(6,8,1,9,3,7,5)))
  expect_equal(df_rearranged$score, c(69,92,79,87,85,87,140))


  # with equal number of rows

  df <- df %>% dplyr::filter(dplyr::row_number() != 5) %>% droplevels()

  df_rearranged <- rearrange(df, method="pair_extremes",
                             unequal_method = "first",
                             drop_rearrange_factor = FALSE)
  expect_equal(df_rearranged$.rearrange_factor, c(1,1,2,2,3,3))
  expect_equal(df_rearranged$participant, factor(c(6,5,1,8,3,7)))
  expect_equal(df_rearranged$score, c(69,140,79,92,85,87))

  df_rearranged <- rearrange(df, method="pair_extremes",
                             unequal_method = "middle",
                             drop_rearrange_factor = FALSE)
  expect_equal(df_rearranged$.rearrange_factor, c(1,1,2,2,3,3))
  expect_equal(df_rearranged$participant, factor(c(6,5,1,8,3,7)))
  expect_equal(df_rearranged$score, c(69,140,79,92,85,87))

  df_rearranged <- rearrange(df, method="pair_extremes",
                             unequal_method = "last",
                             drop_rearrange_factor = FALSE)
  expect_equal(df_rearranged$.rearrange_factor, c(1,1,2,2,3,3))
  expect_equal(df_rearranged$participant, factor(c(6,5,1,8,3,7)))
  expect_equal(df_rearranged$score, c(69,140,79,92,85,87))

})

test_that("create_rearrange_factor_pair_extremes_() works", {

  # unequal_method "middle"

  # Equal size
  expect_equal(create_rearrange_factor_pair_extremes_(40, unequal_method = "middle"), c(c(1:20), rev(c(1:20))))
  expect_equal(create_rearrange_factor_pair_extremes_(200, unequal_method = "middle"), c(c(1:100), rev(c(1:100))))

  # Unequal size
  expect_equal(create_rearrange_factor_pair_extremes_(41, unequal_method = "middle"), c(c(1:10,12:21), 11, rev(c(1:10,12:21))))
  expect_equal(create_rearrange_factor_pair_extremes_(201, unequal_method = "middle"), c(c(1:50,52:101), 51, rev(c(1:50,52:101))))

  # unequal_method "first"

  # Equal size
  expect_equal(create_rearrange_factor_pair_extremes_(40, unequal_method = "first"), c(c(1:20), rev(c(1:20))))
  expect_equal(create_rearrange_factor_pair_extremes_(200, unequal_method = "first"), c(c(1:100), rev(c(1:100))))

  # Unequal size
  expect_equal(create_rearrange_factor_pair_extremes_(41, unequal_method = "first"), c(1,c(c(1:20), rev(c(1:20)))+1))
  expect_equal(create_rearrange_factor_pair_extremes_(201, unequal_method = "first"), c(1, c(c(1:100), rev(c(1:100)))+1))

  # unequal_method "last"

  # Equal size
  expect_equal(create_rearrange_factor_pair_extremes_(40, unequal_method = "last"), c(c(1:20), rev(c(1:20))))
  expect_equal(create_rearrange_factor_pair_extremes_(200, unequal_method = "last"), c(c(1:100), rev(c(1:100))))

  # Unequal size
  expect_equal(create_rearrange_factor_pair_extremes_(41, unequal_method = "last"), c(c(c(1:20), rev(c(1:20))),21))
  expect_equal(create_rearrange_factor_pair_extremes_(201, unequal_method = "last"), c(c(c(1:100), rev(c(1:100))), 101))
})
