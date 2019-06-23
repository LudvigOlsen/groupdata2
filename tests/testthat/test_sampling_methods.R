library(groupdata2)
context("sampling_methods()")

# test_intact_IDs <- function(data,balanced_data,id_col){
#   # get unique ids in balanced_data
#   unique_ids <- unique(balanced_data[[id_col]])
#   # get data with those ids
#   data <- data %>% dplyr::filter(!! as.name(id_col) %in% unique_ids)
#
#   data
#
# }

test_that("balance() works with method n_ids()", {

  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5)),
    "diagnosis" = factor(c(0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4, 1, 2, 1, 2, 3, 4),
    "score" = sample(c(1:100), 13)
  )

  # Max
  set.seed(1)
  balanced_by_n_ids <- balance(df, size="max", cat_col = "diagnosis",
                              id_col = "participant",
                              id_method = "n_ids", mark_new_rows = FALSE)
  counts <- balanced_by_n_ids %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(2, 4, 4, 2, 2))
  expect_equal(counts$diagnosis, factor(c(0, 0, 0, 1, 1)))

  # Test mark_new_rows = TRUE
  set.seed(1)
  balanced_by_n_ids <- balance(df, size="max", cat_col = "diagnosis",
                               id_col = "participant",
                               id_method = "n_ids", mark_new_rows = TRUE)
  expect_equal(balanced_by_n_ids$.new_row, c(0,0,0,0,0,0,0,0,0,0,0,1,0,0))
  counts <- balanced_by_n_ids %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(2, 4, 4, 2, 2))
  expect_equal(counts$diagnosis, factor(c(0, 0, 0, 1, 1)))

#   # Test intact IDs
#   test_intact_IDs(df, balanced_by_n_ids, "participant")

})

test_that("balance() works with method n_rows_c", {

  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5)),
    "diagnosis" = factor(c(0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4, 1, 2, 1, 2, 3, 4),
    "score" = sample(c(1:100), 13)
  )

  # Max
  set.seed(1)
  balanced_by_n_rows <- balance(df, size="max", cat_col = "diagnosis",
                               id_col = "participant",
                               id_method = "n_rows_c", mark_new_rows = FALSE)
  counts <- balanced_by_n_rows %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(2, 4, 4, 4, 6))
  expect_equal(counts$diagnosis, factor(c(0, 0, 0, 1, 1)))

  # Test mark_new_rows = TRUE
  set.seed(1)
  balanced_by_n_rows <- balance(df, size="max", cat_col = "diagnosis",
                               id_col = "participant",
                               id_method = "n_rows_c", mark_new_rows = TRUE)
  expect_equal(balanced_by_n_rows$.new_row, c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,1,0,1,1))
  counts <- balanced_by_n_rows %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(2, 4, 4, 4, 6))
  expect_equal(counts$diagnosis, factor(c(0, 0, 0, 1, 1)))
  cat_count <- balanced_by_n_rows %>%
    dplyr::count(diagnosis)
  expect_equal(cat_count$n, c(10,10))

  # Min
  set.seed(1)
  balanced_by_n_rows <- balance(df, size="min", cat_col = "diagnosis",
                                id_col = "participant",
                                id_method = "n_rows_c", mark_new_rows = FALSE)
  counts <- balanced_by_n_rows %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(4,1,2))
  expect_equal(counts$diagnosis, factor(c(0, 1, 1)))

  # Test mark_new_rows = TRUE
  set.seed(1)
  balanced_by_n_rows <- balance(df, size="min", cat_col = "diagnosis",
                                id_col = "participant",
                                id_method = "n_rows_c", mark_new_rows = TRUE)
  expect_equal(balanced_by_n_rows$.new_row, c(0,0,0,0,0,0,0))
  counts <- balanced_by_n_rows %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(4,1,2))
  expect_equal(counts$diagnosis, factor(c(0, 1, 1)))
  cat_count <- balanced_by_n_rows %>%
    dplyr::count(diagnosis)
  expect_equal(cat_count$n, c(4,3))


  # Specific n
  set.seed(1)
  balanced_by_n_rows <- balance(df, size=5, cat_col = "diagnosis",
                                id_col = "participant",
                                id_method = "n_rows_c", mark_new_rows = FALSE)
  counts <- balanced_by_n_rows %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(2,4,1,4))
  expect_equal(counts$diagnosis, factor(c(0, 0, 1, 1)))

  # Test mark_new_rows = TRUE
  set.seed(1)
  balanced_by_n_rows <- balance(df, size=5, cat_col = "diagnosis",
                                id_col = "participant",
                                id_method = "n_rows_c", mark_new_rows = TRUE)
  expect_equal(balanced_by_n_rows$.new_row, c(0,0,0,0,0,0,0,0,1,0,1))
  counts <- balanced_by_n_rows %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(2,4,1,4))
  expect_equal(counts$diagnosis, factor(c(0, 0, 1, 1)))
  cat_count <- balanced_by_n_rows %>%
    dplyr::count(diagnosis)
  expect_equal(cat_count$n, c(6,5))


  # Add tests to secure that IDs are respected - compare rows per id in data and balanced data


})

test_that("balance() works with method distributed", {

  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5)),
    "diagnosis" = factor(c(0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4, 1, 2, 1, 2, 3, 4),
    "score" = sample(c(1:100), 13)
  )

  # Max
  set.seed(4)
  balanced_by_n_rows <- balance(df, size="max", cat_col = "diagnosis",
                                id_col = "participant",
                                id_method = "distributed", mark_new_rows = FALSE)
  counts <- balanced_by_n_rows %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(2, 4, 4, 4, 6))
  expect_equal(counts$diagnosis, factor(c(0, 0, 0, 1, 1)))

  # Test mark_new_rows = TRUE
  set.seed(4)
  balanced_by_n_rows <- balance(df, size="max", cat_col = "diagnosis",
                                id_col = "participant",
                                id_method = "distributed", mark_new_rows = TRUE)
  expect_equal(balanced_by_n_rows$.new_row, c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,1))
  counts <- balanced_by_n_rows %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(2, 4, 4, 4, 6))
  expect_equal(counts$diagnosis, factor(c(0, 0, 0, 1, 1)))
  cat_count <- balanced_by_n_rows %>%
    dplyr::count(diagnosis)
  expect_equal(cat_count$n, c(10,10))

  # Min
  set.seed(1)
  balanced_by_n_rows <- balance(df, size="min", cat_col = "diagnosis",
                                id_col = "participant",
                                id_method = "distributed", mark_new_rows = FALSE)
  counts <- balanced_by_n_rows %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(1,2,1,2))
  expect_equal(counts$diagnosis, factor(c(0, 0, 1, 1)))

  # Test mark_new_rows = TRUE
  set.seed(1)
  balanced_by_n_rows <- balance(df, size="min", cat_col = "diagnosis",
                                id_col = "participant",
                                id_method = "distributed", mark_new_rows = TRUE)
  expect_equal(balanced_by_n_rows$.new_row, c(0,0,0,0,0,0))
  counts <- balanced_by_n_rows %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(1,2,1,2))
  expect_equal(counts$diagnosis, factor(c(0, 0, 1, 1)))
  cat_count <- balanced_by_n_rows %>%
    dplyr::count(diagnosis)
  expect_equal(cat_count$n, c(3,3))


  # Specific n
  set.seed(2)
  balanced_by_n_rows <- balance(df, size=5, cat_col = "diagnosis",
                                id_col = "participant",
                                id_method = "distributed", mark_new_rows = FALSE)
  counts <- balanced_by_n_rows %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(3,2,2,3))
  expect_equal(counts$diagnosis, factor(c(0, 0, 1, 1)))

  # Test mark_new_rows = TRUE
  set.seed(2)
  balanced_by_n_rows <- balance(df, size=5, cat_col = "diagnosis",
                                id_col = "participant",
                                id_method = "distributed", mark_new_rows = TRUE)
  expect_equal(balanced_by_n_rows$.new_row, c(0,0,0,0,0,0,1,0,0,1))
  counts <- balanced_by_n_rows %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(3,2,2,3))
  expect_equal(counts$diagnosis, factor(c(0, 0, 1, 1)))
  cat_count <- balanced_by_n_rows %>%
    dplyr::count(diagnosis)
  expect_equal(cat_count$n, c(5,5))

})


test_that("balance() works with method nested", {

  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5)),
    "diagnosis" = factor(c(0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4, 1, 2, 1, 2, 3, 4),
    "score" = sample(c(1:100), 13)
  )

  # Max
  set.seed(1)
  balanced_by_nested <- balance(df, size="max", cat_col = "diagnosis",
                                id_col = "participant",
                                id_method = "nested", mark_new_rows = FALSE)
  counts <- balanced_by_nested %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(4, 4, 4, 2, 2))
  expect_equal(counts$diagnosis, factor(c(0, 0, 0, 1, 1)))

  # Test mark_new_rows = TRUE
  set.seed(1)
  balanced_by_nested <- balance(df, size="max", cat_col = "diagnosis",
                                id_col = "participant",
                                id_method = "nested", mark_new_rows = TRUE)
  expect_equal(balanced_by_nested$.new_row, c(0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,0))
  counts <- balanced_by_nested %>%
    dplyr::count(diagnosis, participant)
  expect_equal(counts$n, c(4, 4, 4, 2, 2))
  expect_equal(counts$diagnosis, factor(c(0, 0, 0, 1, 1)))
  cat_count <- balanced_by_nested %>%
    dplyr::count(diagnosis)
  expect_equal(cat_count$n, c(12,4))

})



