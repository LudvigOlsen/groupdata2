library(groupdata2)
context("group_factor()")


test_that("group sizes works with group_factor with method greedy", {
  xpectr::set_test_seed(1)

  group_counts <- function(v, n) {
    gf <- group_factor(v, n, method = "greedy")
    counts <- plyr::count(gf)
    return(counts$freq)
  }

  v <- c(1:57)

  expect_equal(group_counts(v, 17), c(17, 17, 17, 6))
  expect_equal(group_counts(v, 10), c(10, 10, 10, 10, 10, 7))
  expect_equal(group_counts(v, 8), c(8, 8, 8, 8, 8, 8, 8, 1))
  expect_equal(group_counts(v, 3), c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3))
  expect_equal(group_counts(v, 0.2), c(11, 11, 11, 11, 11, 2))
  expect_equal(group_counts(v, 0.25), c(14, 14, 14, 14, 1))

  expect_equal(length(group_counts(v, 1)), 57)

  v <- c(1:100)

  expect_equal(group_counts(v, 0.25), c(25, 25, 25, 25))

  expect_error(
    xpectr::strip_msg(group_counts(v, 0.003)),
    xpectr::strip("Assertion on 'n converted to whole number' failed: Must be >= 1."),
    fixed = TRUE)

  v <- c(1:3)

  expect_error(
    xpectr::strip_msg(group_counts(v, 17)),
    xpectr::strip("Assertion on 'length(data) >= n' failed: Must be TRUE."),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(group_counts(v, 0)),
    xpectr::strip(paste0("1 assertions failed:\n * 'n' was 0. If this is on purpose, ",
                         "set 'allow_zero' to 'TRUE'.")),
    fixed = TRUE)

})

test_that("group sizes works with group_factor with method n_dist", {
  xpectr::set_test_seed(1)

  group_counts <- function(v, n) {
    gf <- group_factor(v, n, method = "n_dist")
    counts <- plyr::count(gf)
    return(counts$freq)
  }

  v <- c(1:57)

  expect_equal(group_counts(v, 1), 57)
  expect_equal(group_counts(v, 2), c(28, 29))
  expect_equal(group_counts(v, 3), c(19, 19, 19))
  expect_equal(group_counts(v, 4), c(14, 14, 14, 15))
  expect_equal(group_counts(v, 5), c(11, 11, 12, 11, 12))
  expect_equal(group_counts(v, 17), c(3, 3, 4, 3, 3, 4, 3, 3, 4, 3, 3, 4, 3, 3, 4, 3, 4))
  expect_equal(group_counts(v, 0.2), c(5, 5, 5, 5, 5, 6, 5, 5, 5, 5, 6))
  expect_equal(group_counts(v, 0.25), c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5))

  expect_equal(length(group_counts(v, 1)), 1)

  v <- c(1:100)

  expect_error(
    xpectr::strip_msg(group_counts(v, 0.003)),
    xpectr::strip("Assertion on 'n converted to whole number' failed: Must be >= 1."),
    fixed = TRUE)

  v <- c(1:3)

  expect_error(
    xpectr::strip_msg(group_counts(v, 4)),
    xpectr::strip("Assertion on 'length(data) >= n' failed: Must be TRUE."),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(group_counts(v, 0)),
    xpectr::strip(paste0("1 assertions failed:\n * 'n' was 0. If this is on purpose, ",
                         "set 'allow_zero' to 'TRUE'.")),
    fixed = TRUE)

})

test_that("group sizes works with group_factor with method n_fill", {
  xpectr::set_test_seed(1)

  group_counts <- function(v, n) {
    gf <- group_factor(v, n, method = "n_fill")
    counts <- plyr::count(gf)
    return(counts$freq)
  }

  v <- c(1:57)

  expect_equal(group_counts(v, 1), 57)
  expect_equal(group_counts(v, 2), c(29, 28))
  expect_equal(group_counts(v, 3), c(19, 19, 19))
  expect_equal(group_counts(v, 4), c(15, 14, 14, 14))
  expect_equal(group_counts(v, 5), c(12, 12, 11, 11, 11))
  expect_equal(group_counts(v, 17), c(4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3))
  expect_equal(group_counts(v, 0.2), c(6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5))
  expect_equal(group_counts(v, 0.25), c(5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4))

  expect_equal(length(group_counts(v, 1)), 1)
  expect_equal(length(group_counts(v, 9)), 9)
  expect_equal(length(group_counts(v, 57)), 57)

  v <- c(1:100)

  expect_error(
    xpectr::strip_msg(group_counts(v, 0.003)),
    xpectr::strip("Assertion on 'n converted to whole number' failed: Must be >= 1."),
    fixed = TRUE)

  v <- c(1:3)

  expect_error(
    xpectr::strip_msg(group_counts(v, 4)),
    xpectr::strip("Assertion on 'length(data) >= n' failed: Must be TRUE."),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(group_counts(v, 0)),
    xpectr::strip(paste0("1 assertions failed:\n * 'n' was 0. If this is on purpose, ",
                         "set 'allow_zero' to 'TRUE'.")),
    fixed = TRUE)

})

test_that("group sizes works with group_factor with method n_rand", {
  xpectr::set_test_seed(1)

  count_groups <- function(v, n) {
    gf <- group_factor(v, n, method = "n_rand")
    counts <- plyr::count(gf)
    return(length(counts$freq))
  }

  group_counts_sum <- function(v, n) {
    gf <- group_factor(v, n, method = "n_fill")
    counts <- plyr::count(gf)
    return(sum(counts$freq))
  }

  v <- c(1:57)

  expect_equal(count_groups(v, 1), 1)
  expect_equal(count_groups(v, 2), 2)
  expect_equal(count_groups(v, 3), 3)
  expect_equal(count_groups(v, 4), 4)
  expect_equal(count_groups(v, 5), 5)
  expect_equal(count_groups(v, 11), 11)
  expect_equal(count_groups(v, 57), 57)

  expect_equal(group_counts_sum(v, 2), 57)
  expect_equal(group_counts_sum(v, 3), 57)
  expect_equal(group_counts_sum(v, 4), 57)
  expect_equal(group_counts_sum(v, 5), 57)
  expect_equal(group_counts_sum(v, 17), 57)
  expect_equal(group_counts_sum(v, 0.2), 57)
  expect_equal(group_counts_sum(v, 0.25), 57)

  v <- c(1:100)

  expect_error(
    xpectr::strip_msg(group_counts_sum(v, 0.003)),
    xpectr::strip("Assertion on 'n converted to whole number' failed: Must be >= 1."),
    fixed = TRUE)

  v <- c(1:3)

  expect_error(
    xpectr::strip_msg(group_counts_sum(v, 4)),
    xpectr::strip("Assertion on 'length(data) >= n' failed: Must be TRUE."),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(group_counts_sum(v, 0)),
    xpectr::strip(paste0("1 assertions failed:\n * 'n' was 0. If this is on purpose, ",
                         "set 'allow_zero' to 'TRUE'.")),
    fixed = TRUE)

})

test_that("group sizes works with group_factor with method staircase", {
  xpectr::set_test_seed(1)

  group_counts <- function(v, n) {
    gf <- group_factor(v, n, method = "staircase")
    counts <- plyr::count(gf)
    return(counts$freq)
  }

  count_groups <- function(v, n) {
    gf <- group_factor(v, n, method = "staircase")
    counts <- plyr::count(gf)
    return(length(counts$freq))
  }

  group_counts_sum <- function(v, n) {
    gf <- group_factor(v, n, method = "staircase")
    counts <- plyr::count(gf)
    return(sum(counts$freq))
  }

  v <- c(1:57)

  expect_equal(group_counts(v, 4), c(4, 8, 12, 16, 17))
  expect_equal(group_counts(v, 5), c(5, 10, 15, 20, 7))
  expect_equal(group_counts(v, 10), c(10, 20, 27))
  expect_equal(group_counts(v, 11), c(11, 22, 24))


  expect_equal(count_groups(v, 1), 11)
  expect_equal(count_groups(v, 2), 8)
  expect_equal(count_groups(v, 3), 6)
  expect_equal(count_groups(v, 4), 5)
  expect_equal(count_groups(v, 5), 5)
  expect_equal(count_groups(v, 11), 3)
  expect_equal(count_groups(v, 56), 2)
  expect_equal(count_groups(v, 57), 1)

  expect_equal(group_counts_sum(v, 1), 57)
  expect_equal(group_counts_sum(v, 2), 57)
  expect_equal(group_counts_sum(v, 3), 57)
  expect_equal(group_counts_sum(v, 4), 57)
  expect_equal(group_counts_sum(v, 5), 57)
  expect_equal(group_counts_sum(v, 17), 57)
  expect_equal(group_counts_sum(v, 0.2), 57)
  expect_equal(group_counts_sum(v, 0.25), 57)

  v <- c(1:100)

  expect_error(
    xpectr::strip_msg(group_counts_sum(v, 0.003)),
    xpectr::strip("Assertion on 'n converted to whole number' failed: Must be >= 1."),
    fixed = TRUE)

  v <- c(1:3)

  expect_error(
    xpectr::strip_msg(group_counts(v, 4)),
    xpectr::strip("Assertion on 'length(data) >= n' failed: Must be TRUE."),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(group_counts(v, 0)),
    xpectr::strip(paste0("1 assertions failed:\n * 'n' was 0. If this is on purpose, ",
                         "set 'allow_zero' to 'TRUE'.")),
    fixed = TRUE)

  expect_equal(group_counts(v, 2), c(2, 1))
  expect_equal(group_counts(v, 0.5), c(1, 2))
})

test_that("group sizes works with group_factor with method primes", {
  xpectr::set_test_seed(1)

  group_counts <- function(v, n, force_equal = FALSE) {
    gf <- group_factor(v, n, method = "primes", force_equal = force_equal)
    counts <- plyr::count(gf)
    return(counts$freq)
  }

  count_groups <- function(v, n, force_equal = FALSE) {
    gf <- group_factor(v, n, method = "primes", force_equal = force_equal)
    counts <- plyr::count(gf)
    return(length(counts$freq))
  }

  group_counts_sum <- function(v, n, force_equal = FALSE) {
    gf <- group_factor(v, n, method = "primes", force_equal = force_equal)
    counts <- plyr::count(gf)
    return(sum(counts$freq))
  }

  v <- c(1:57)

  expect_equal(group_counts(v, 3), c(3, 5, 7, 11, 13, 17, 1))
  expect_equal(group_counts(v, 5), c(5, 7, 11, 13, 17, 4))
  expect_equal(group_counts(v, 11), c(11, 13, 17, 16))

  expect_equal(count_groups(v, 2), 7)
  expect_equal(count_groups(v, 3), 7)
  expect_equal(count_groups(v, 5), 6)

  expect_equal(group_counts_sum(v, 2), 57)
  expect_equal(group_counts_sum(v, 3), 57)
  expect_equal(group_counts_sum(v, 5), 57)
  expect_equal(group_counts_sum(v, 17), 57)
  expect_equal(group_counts_sum(v, 0.2), 57)

  v <- c(1:100)

  expect_error(
    xpectr::strip_msg(group_counts_sum(v, 0.003)),
    xpectr::strip("Assertion on 'n converted to whole number' failed: Must be >= 1."),
    fixed = TRUE)

  v <- c(1:3)

  expect_error(
    xpectr::strip_msg(group_counts(v, 4)),
    xpectr::strip("Assertion on 'length(data) >= n' failed: Must be TRUE."),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(group_counts(v, 0)),
    xpectr::strip(paste0("1 assertions failed:\n * 'n' was 0. If this is on purpose, ",
                         "set 'allow_zero' to 'TRUE'.")),
    fixed = TRUE)

  expect_equal(group_counts(v, 2), c(2, 1))
  expect_equal(group_counts(v, 0.67), c(2, 1))

  # force_equal

  v <- c(1:83)

  # Note: 83 %primes% 11 == 0, so tests force equal when no excessive elements to cut
  expect_equal(
    group_counts(v, 11, force_equal = TRUE),
    group_counts(v, 11, force_equal = FALSE)
  )

  # When excessive elements
  expect_equal(group_counts(v, 17, force_equal = FALSE), c(17, 19, 23, 24))
  expect_equal(group_counts(v, 17, force_equal = TRUE), c(17, 19, 23))
})

test_that("group sizes works with group_factor with method l_sizes", {
  xpectr::set_test_seed(1)

  group_f <- function(v, n) {
    return(group_factor(v, n, method = "l_sizes"))
  }

  group_counts <- function(v, n) {
    gf <- group_factor(v, n, method = "l_sizes")
    counts <- plyr::count(gf)
    return(counts$freq)
  }

  expect_equal(group_f(c(1:10), c(0.2)), factor(c(1, 1, 2, 2, 2, 2, 2, 2, 2, 2)))
  expect_equal(group_f(c(1:11), c(0.2)), factor(c(1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2)))
  expect_equal(group_f(c(1:14), c(0.2)), factor(c(1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)))
  expect_equal(group_f(c(1:15), c(0.2)), factor(c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)))

  expect_equal(group_f(c(1:2), c(0.2, 0.5)), factor(c(2, 3)))

  expect_equal(group_counts(c(1:200), c(20, 20, 40, 40)), c(20, 20, 40, 40, 80))
  expect_equal(group_counts(c(1:200), c(0.5)), c(100, 100))
  expect_equal(group_counts(c(1:200), c(0.55)), c(110, 90))
  expect_equal(group_counts(c(1:200), c(0.555)), c(111, 89))
})

test_that("group sizes works with group_factor with method l_starts", {
  xpectr::set_test_seed(1)

  group_f <- function(v, n) {
    return(group_factor(v, n, method = "l_starts"))
  }

  expect_equal(
    group_f(c(1:10), list(3, 5, 7)),
    factor(c(1, 1, 2, 2, 3, 3, 4, 4, 4, 4))
  )

  expect_equal(
    group_f(rep(c(1:5), 2), list(3, c(4, 2))),
    factor(c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3))
  )

  expect_equal(
    group_f(rep(c(1:5), 2), list(1, 3, c(4, 2))),
    factor(c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3))
  )

  expect_equal(
    group_f(
      c("a", "b", "a", "b", "a", "b"),
      list("a", c("b", 2), "b")
    ),
    factor(c(1, 1, 1, 2, 2, 3))
  )

  expect_equal(
    group_f(
      c("a", "b", "a", "b", "a", "b"),
      list(c("b", 2), "b")
    ),
    factor(c(1, 1, 1, 2, 2, 3))
  )

  # n = 'auto'

  expect_equal(
    group_f(c(1:10), "auto"),
    factor(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  )

  expect_equal(
    group_f(c(1, 1, 2, 2, 3, 3, 4, 5, 1, 1, 2, 2), "auto"),
    factor(c(1, 1, 2, 2, 3, 3, 4, 5, 6, 6, 7, 7))
  )

  expect_equal(
    group_f(c("a", "a", "b", "b", "c", "3", "4", "4", "c"), "auto"),
    factor(c(1, 1, 2, 2, 3, 4, 5, 5, 6))
  )

  # with NAs
  expect_equal(
    group_f(c("a", "a", "b", "b", NA, NA, "c", "3", "4", NA, "4", "c"), "auto"),
    factor(c(1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9))
  )
  expect_equal(
    group_f(c(NA, NA, "a", "a", "b", "b", "c", "3", "4", NA, "4", "c"), "auto"),
    factor(c(1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9))
  )
  expect_equal(
    group_f(c(NA, "a", "a", "b", "b", "c", "3", "4", NA, "4", "c", NA, NA), "auto"),
    factor(c(1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10, 10))
  )


  # If factor

  ## Testing 'group_f(factor(c(1:10)), list(3, 5, 7))'                      ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(group_f(factor(c(1:10)), list(3, 5, 7)), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['warnings']]),
    xpectr::strip("'data' is a factor. Converting to character."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['messages']]),
    xpectr::strip(character(0)),
    fixed = TRUE)
  # Assigning output
  output_19148 <- xpectr::suppress_mw(group_f(factor(c(1:10)), list(3, 5, 7)))
  # Testing is factor
  expect_true(
    is.factor(output_19148))
  # Testing values
  expect_equal(
    as.character(output_19148),
    c("1", "1", "2", "2", "3", "3", "4", "4", "4", "4"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    10L)
  # Testing number of levels
  expect_equal(
    nlevels(output_19148),
    4L)
  # Testing levels
  expect_equal(
    levels(output_19148),
    c("1", "2", "3", "4"),
    fixed = TRUE)
  ## Finished testing 'group_f(factor(c(1:10)), list(3, 5, 7))'             ####


  ## Testing 'group_f(factor(rep(c(1:5), 2)), list(3, c(4,...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(group_f(factor(rep(c(1:5), 2)), list(3, c(4, 2))), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['warnings']]),
    xpectr::strip("'data' is a factor. Converting to character."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['messages']]),
    xpectr::strip(character(0)),
    fixed = TRUE)
  # Assigning output
  output_19148 <- xpectr::suppress_mw(group_f(factor(rep(c(1:5), 2)), list(3, c(4, 2))))
  # Testing is factor
  expect_true(
    is.factor(output_19148))
  # Testing values
  expect_equal(
    as.character(output_19148),
    c("1", "1", "2", "2", "2", "2", "2", "2", "3", "3"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    10L)
  # Testing number of levels
  expect_equal(
    nlevels(output_19148),
    3L)
  # Testing levels
  expect_equal(
    levels(output_19148),
    c("1", "2", "3"),
    fixed = TRUE)
  ## Finished testing 'group_f(factor(rep(c(1:5), 2)), list(3, c(4,...'     ####


  ## Testing 'group_f(factor(rep(c(1:5), 2)),list(1, 3, c(...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(group_f(factor(rep(c(1:5), 2)),list(1, 3, c(4, 2))), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['warnings']]),
    xpectr::strip("'data' is a factor. Converting to character."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['messages']]),
    xpectr::strip(character(0)),
    fixed = TRUE)
  # Assigning output
  output_19148 <- xpectr::suppress_mw(group_f(factor(rep(c(1:5), 2)),list(1, 3, c(4, 2))))
  # Testing is factor
  expect_true(
    is.factor(output_19148))
  # Testing values
  expect_equal(
    as.character(output_19148),
    c("1", "1", "2", "2", "2", "2", "2", "2", "3", "3"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    10L)
  # Testing number of levels
  expect_equal(
    nlevels(output_19148),
    3L)
  # Testing levels
  expect_equal(
    levels(output_19148),
    c("1", "2", "3"),
    fixed = TRUE)
  ## Finished testing 'group_f(factor(rep(c(1:5), 2)),list(1, 3, c(...'     ####


  ## Testing 'group_f(factor(c("a", "b", "a", "b", "a", "b...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(group_f(factor(c("a", "b", "a", "b", "a", "b")), list("a", c("b", 2), "b")), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['warnings']]),
    xpectr::strip("'data' is a factor. Converting to character."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['messages']]),
    xpectr::strip(character(0)),
    fixed = TRUE)
  # Assigning output
  output_19148 <- xpectr::suppress_mw(group_f(factor(c("a", "b", "a", "b", "a", "b")), list("a", c("b", 2), "b")))
  # Testing is factor
  expect_true(
    is.factor(output_19148))
  # Testing values
  expect_equal(
    as.character(output_19148),
    c("1", "1", "1", "2", "2", "3"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    6L)
  # Testing number of levels
  expect_equal(
    nlevels(output_19148),
    3L)
  # Testing levels
  expect_equal(
    levels(output_19148),
    c("1", "2", "3"),
    fixed = TRUE)
  ## Finished testing 'group_f(factor(c("a", "b", "a", "b", "a", "b...'     ####


  ## Testing 'group_f(factor(c("a", "b", "a", "b", "a", "b...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(group_f(factor(c("a", "b", "a", "b", "a", "b")), list(c("b", 2), "b")), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['warnings']]),
    xpectr::strip("'data' is a factor. Converting to character."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['messages']]),
    xpectr::strip(character(0)),
    fixed = TRUE)
  # Assigning output
  output_19148 <- xpectr::suppress_mw(group_f(factor(c("a", "b", "a", "b", "a", "b")), list(c("b", 2), "b")))
  # Testing is factor
  expect_true(
    is.factor(output_19148))
  # Testing values
  expect_equal(
    as.character(output_19148),
    c("1", "1", "1", "2", "2", "3"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    6L)
  # Testing number of levels
  expect_equal(
    nlevels(output_19148),
    3L)
  # Testing levels
  expect_equal(
    levels(output_19148),
    c("1", "2", "3"),
    fixed = TRUE)
  ## Finished testing 'group_f(factor(c("a", "b", "a", "b", "a", "b...'     ####

  # n = 'auto'
  # Raises warning because find_starts converts factor to string

  expect_warning(
    expect_equal(
      group_f(factor(c("a", "a", "b", "b", "c", "3", "4", "4", "c")), "auto"),
      factor(c(1, 1, 2, 2, 3, 4, 5, 5, 6))
    ),
    "'data' is a factor. Converting to character.",
    fixed = TRUE
  )

  expect_warning(
    expect_equal(
      group_f(factor(c(1, 1, 2, 2, 3, 4, 4, 5, 5, 5)), "auto"),
      factor(c(1, 1, 2, 2, 3, 4, 4, 5, 5, 5))
    ),
    "'data' is a factor. Converting to character.",
    fixed = TRUE
  )
})

test_that("l_starts raises error correctly when value is not found", {
  xpectr::set_test_seed(1)

  expect_error(group(c(1:5), c(1, 3, 6), method = "l_starts"),
    "Start value \"6\" not found in vector",
    fixed = TRUE
  )

  expect_error(group(c("a", "b", "c", "d"), n = c(1, 3, 6), method = "l_starts"),
    "Start value \"1\" not found in vector",
    fixed = TRUE
  )

  expect_error(group(c("a", "b", "c", "d"), n = c("b", "d", "e"), method = "l_starts"),
    "Start value \"e\" not found in vector",
    fixed = TRUE
  )

  expect_error(group(c(3, 4, 5, 6), n = c(2, 4, 5), method = "l_starts"),
    "Start value \"2\" not found in vector",
    fixed = TRUE
  )

  df <- data.frame(
    "a" = c(1, 2, 3, 4),
    "b" = c(4, 5, 6, 7),
    stringsAsFactors = FALSE
  )

  # Testing side effect
  expect_error(xpectr::strip_msg(group(
    df,
    n = c(2, 4),
    method = "l_starts",
    starts_col = "c"
  )),
  xpectr::strip(
    paste0(
      "1 assertions failed:\n * 'starts_col' column, 'c', not foun",
      "d in 'data'."
    )
  ),
  fixed = TRUE)


  # Negative numbers in n

  ## Testing 'group(c(3, -3, 5, 6), n = c(-3, 5), method =...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_12655 <- group(c(3, -3, 5, 6), n = c(-3, 5), method = "l_starts")
  # Testing class
  expect_equal(
    class(output_12655),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_12655[["data"]],
    c(3, -3, 5, 6),
    tolerance = 1e-4)
  expect_equal(
    output_12655[[".groups"]],
    structure(c(1L, 2L, 3L, 3L), .Label = c("1", "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_12655),
    c("data", ".groups"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_12655),
    c("numeric", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_12655),
    c("double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_12655),
    c(4L, 2L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_12655)),
    ".groups",
    fixed = TRUE)
  ## Finished testing 'group(c(3, -3, 5, 6), n = c(-3, 5), method =...'     ####


})

test_that("force_equal works with group_factor with all methods", {
  xpectr::set_test_seed(1)

  group_counts <- function(v, n, method) {
    gf <- group_factor(v, n, method, force_equal = TRUE)
    counts <- plyr::count(gf)
    return(counts$freq)
  }

  count_groups <- function(v, n, method) {
    gf <- group_factor(v, n, method = method, force_equal = TRUE)
    counts <- plyr::count(gf)
    return(length(counts$freq))
  }


  v <- c(1:57)


  expect_equal(group_counts(v, 10, "greedy"), c(10, 10, 10, 10, 10))
  expect_equal(group_counts(v, 8, "greedy"), c(8, 8, 8, 8, 8, 8, 8))
  expect_equal(group_counts(v, 13, "greedy"), c(13, 13, 13, 13))
  expect_equal(group_counts(v, 0.5, "greedy"), c(28, 28))
  expect_equal(group_counts(v, 0.267, "greedy"), c(15, 15, 15))

  expect_equal(count_groups(v, 2, "greedy"), 28)
  expect_equal(count_groups(v, 7, "greedy"), 8)
  expect_equal(count_groups(v, 5, "greedy"), 11)

  expect_equal(group_counts(v, 10, "n_dist"), c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
  expect_equal(group_counts(v, 8, "n_dist"), c(7, 7, 7, 7, 7, 7, 7, 7))
  expect_equal(group_counts(v, 5, "n_dist"), c(11, 11, 11, 11, 11))

  expect_equal(count_groups(v, 2, "n_dist"), 2)
  expect_equal(count_groups(v, 7, "n_dist"), 7)
  expect_equal(count_groups(v, 5, "n_dist"), 5)

  expect_equal(group_counts(v, 10, "n_fill"), c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
  expect_equal(group_counts(v, 8, "n_fill"), c(7, 7, 7, 7, 7, 7, 7, 7))
  expect_equal(group_counts(v, 5, "n_fill"), c(11, 11, 11, 11, 11))

  expect_equal(count_groups(v, 2, "n_fill"), 2)
  expect_equal(count_groups(v, 7, "n_fill"), 7)
  expect_equal(count_groups(v, 5, "n_fill"), 5)

  expect_equal(group_counts(v, 10, "n_last"), c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
  expect_equal(group_counts(v, 8, "n_last"), c(7, 7, 7, 7, 7, 7, 7, 7))
  expect_equal(group_counts(v, 5, "n_last"), c(11, 11, 11, 11, 11))

  expect_equal(count_groups(v, 2, "n_last"), 2)
  expect_equal(count_groups(v, 7, "n_last"), 7)
  expect_equal(count_groups(v, 5, "n_last"), 5)

  expect_equal(group_counts(v, 10, "n_rand"), c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5))
  expect_equal(group_counts(v, 8, "n_rand"), c(7, 7, 7, 7, 7, 7, 7, 7))
  expect_equal(group_counts(v, 5, "n_rand"), c(11, 11, 11, 11, 11))

  expect_equal(count_groups(v, 2, "n_rand"), 2)
  expect_equal(count_groups(v, 7, "n_rand"), 7)
  expect_equal(count_groups(v, 5, "n_rand"), 5)

  expect_equal(group_counts(v, 10, "staircase"), c(10, 20))
  expect_equal(group_counts(v, 8, "staircase"), c(8, 16, 24))
  expect_equal(group_counts(v, 13, "staircase"), c(13, 26))
  expect_equal(group_counts(v, 3, "staircase"), c(3, 6, 9, 12, 15))

  expect_equal(group_counts(v, 11, "primes"), c(11, 13, 17))
  expect_equal(group_counts(v, 23, "primes"), c(23, 29))
  expect_equal(group_counts(v, 29, "primes"), c(29))
  expect_equal(group_counts(v, 2, "primes"), c(2, 3, 5, 7, 11, 13))

  expect_equal(group_counts(v, 2, "l_sizes"), c(2))
  expect_equal(group_counts(v, 0.2, "l_sizes"), c(11))
  expect_equal(group_counts(v, list(0.2, 0.3), "l_sizes"), c(11, 17))
  expect_equal(group_counts(v, c(0.2, 0.3), "l_sizes"), c(11, 17))
})

test_that("allow_zero works with group_factor", {
  xpectr::set_test_seed(1)

  v <- c(1:3)

  expect_equal(group_factor(v, 0, allow_zero = TRUE), c(NA, NA, NA))
  expect_is(group_factor(v, 0, allow_zero = TRUE), "logical")
  expect_equal(group_factor(1, 0, allow_zero = TRUE), NA)
})

test_that("l_sizes works with no groups", {
  xpectr::set_test_seed(1)

  expect_equal(group_factor(c(1:3), 0.2, method = "l_sizes"), factor(c(2, 2, 2)))
  expect_warning(group_factor(c(1:3), 0.2, method = "l_sizes", force_equal = TRUE), "No groups. Returned NA.", fixed = TRUE)
})


# Test descending when it has been implemented in all relevant methods

test_that("fuzz testing input checks for n_dist method in group_factor()", {
  xpectr::set_test_seed(1)

  df <- data.frame(
    "n" = c(1, 2, 3, 4, 2, 1, 5, 2, 1, 9),
    "s" = c(4, 4, 6, 6, 7, 7, 7, 8, 8, 1),
    "c" = as.character(c(4, 4, 6, 6, 7, 7, 7, 8, 8, 1)),
    "f" = as.factor(as.character(c(4, 4, 6, 6, 7, 7, 7, 8, 8, 1))),
    stringsAsFactors = FALSE
  )

  group_counts <- function(...) {
    gf <- group_factor(...)
    counts <- plyr::count(gf)
    counts$freq
  }

  # Tests focused on n_dist method
  xpectr::set_test_seed(1)
  # xpectr::gxs_function(group_counts,
  #                      args_values = list(
  #                        "data" = list(df, df$s, df$c, df$f, NA, 1),
  #                        "n" = list(3, 0.4, c(0.2, 4), "hej", "auto"),
  #                        "method" = list("n_dist", "l_sizes", "l_starts"),
  #                        "starts_col" = list(NULL, "s", "f"),
  #                        "force_equal" = list(FALSE, TRUE),
  #                        "allow_zero" = list(FALSE, TRUE),
  #                        "descending" = list(FALSE, TRUE),
  #                        "randomize" = list(FALSE, TRUE),
  #                        "remove_missing_starts" = list(FALSE, TRUE)
  #                      ), indentation = 2)


  ## Testing 'group_counts'                                                   ####
  ## Initially generated by xpectr
  # Testing different combinations of argument values

  # Testing group_counts(data = df, n = 3, method = "n_dis...
  xpectr::set_test_seed(42)
  # Assigning output
  output_12655 <- group_counts(data = df, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_12655),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_12655,
    type = "integer")
  # Testing values
  expect_equal(
    output_12655,
    c(3, 3, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_12655),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_12655),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_12655)),
    3L)

  # Testing group_counts(data = df, n = 3, method = "n_dis...
  # Changed from baseline: allow_zero
  xpectr::set_test_seed(42)
  # Assigning output
  output_13721 <- group_counts(data = df, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = TRUE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_13721),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_13721,
    type = "integer")
  # Testing values
  expect_equal(
    output_13721,
    c(3, 3, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_13721),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_13721),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_13721)),
    3L)

  # Testing group_counts(data = df, n = 3, method = "n_dis...
  # Changed from baseline: allow_zero
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = NULL, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'allow_zero': Must be of ",
                         "type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing group_counts(data = df$s, n = 3, method = "n_d...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Assigning output
  output_19082 <- group_counts(data = df$s, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_19082),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19082,
    type = "integer")
  # Testing values
  expect_equal(
    output_19082,
    c(3, 3, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19082),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19082),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19082)),
    3L)

  # Testing group_counts(data = df$c, n = 3, method = "n_d...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Assigning output
  output_12016 <- group_counts(data = df$c, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_12016),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_12016,
    type = "integer")
  # Testing values
  expect_equal(
    output_12016,
    c(3, 3, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_12016),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_12016),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_12016)),
    3L)

  # Testing group_counts(data = df$f, n = 3, method = "n_d...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Assigning output
  output_18983 <- group_counts(data = df$f, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_18983),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_18983,
    type = "integer")
  # Testing values
  expect_equal(
    output_18983,
    c(3, 3, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_18983),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_18983),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_18983)),
    3L)

  # Testing group_counts(data = NA, n = 3, method = "n_dis...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = NA, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("1 assertions failed:\n * 'data' cannot be 'NA'."),
    fixed = TRUE)

  # Testing group_counts(data = 1, n = 3, method = "n_dist...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = 1, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("Assertion on 'length(data) >= n' failed: Must be TRUE."),
    fixed = TRUE)

  # Testing group_counts(data = NULL, n = 3, method = "n_d...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = NULL, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("1 assertions failed:\n * 'data' cannot be 'NULL'"),
    fixed = TRUE)

  # Testing group_counts(data = df, n = 3, method = "n_dis...
  # Changed from baseline: descending
  xpectr::set_test_seed(42)
  # Assigning output
  output_10617 <- group_counts(data = df, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = TRUE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_10617),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_10617,
    type = "integer")
  # Testing values
  expect_equal(
    output_10617,
    c(3, 3, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_10617),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_10617),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_10617)),
    3L)

  # Testing group_counts(data = df, n = 3, method = "n_dis...
  # Changed from baseline: descending
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = NULL, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'descending': Must be of ",
                         "type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing group_counts(data = df, n = 3, method = "n_dis...
  # Changed from baseline: force_equal
  xpectr::set_test_seed(42)
  # Assigning output
  output_11765 <- group_counts(data = df, n = 3, method = "n_dist", starts_col = NULL, force_equal = TRUE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_11765),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_11765,
    type = "integer")
  # Testing values
  expect_equal(
    output_11765,
    c(3, 3, 3),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_11765),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_11765),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_11765)),
    3L)

  # Testing group_counts(data = df, n = 3, method = "n_dis...
  # Changed from baseline: force_equal
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = 3, method = "n_dist", starts_col = NULL, force_equal = NULL, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'force_equal': Must be of",
                         " type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing group_counts(data = df, n = 3, method = "l_siz...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Assigning output
  output_13841 <- group_counts(data = df, n = 3, method = "l_sizes", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_13841),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_13841,
    type = "integer")
  # Testing values
  expect_equal(
    output_13841,
    c(3, 7),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_13841),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_13841),
    2L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_13841)),
    2L)

  # Testing group_counts(data = df, n = 3, method = "l_sta...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = 3, method = "l_starts", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * when 'method' is 'l_starts' and 'd",
                         "ata' is a data.frame, 'starts_col' must be specified.")),
    fixed = TRUE)

  # Testing group_counts(data = df, n = 3, method = NULL, ...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = 3, method = NULL, starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'method': Must be of type",
                         " 'string', not 'NULL'.")),
    fixed = TRUE)

  # Testing group_counts(data = df, n = 0.4, method = "n_d...
  # Changed from baseline: n
  xpectr::set_test_seed(42)
  # Assigning output
  output_17176 <- group_counts(data = df, n = 0.4, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_17176),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_17176,
    type = "integer")
  # Testing values
  expect_equal(
    output_17176,
    c(2, 3, 2, 3),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_17176),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_17176),
    4L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_17176)),
    4L)

  # Testing group_counts(data = df, n = c(0.2, 4), method ...
  # Changed from baseline: n
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = c(0.2, 4), method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("when 'method' is 'n_dist', 'n' must be numeric scalar."),
    fixed = TRUE)

  # Testing group_counts(data = df, n = "hej", method = "n...
  # Changed from baseline: n
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = "hej", method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("1 assertions failed:\n * 'n' can only be character when method is 'l_starts'."),
    fixed = TRUE)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: n
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = "auto", method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("1 assertions failed:\n * 'n' can only be character when method is 'l_starts'."),
    fixed = TRUE)

  # Testing group_counts(data = df, n = NULL, method = "n_...
  # Changed from baseline: n
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = NULL, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("1 assertions failed:\n * 'n' cannot be 'NULL'"),
    fixed = TRUE)

  # Testing group_counts(data = df, n = 3, method = "n_dis...
  # Changed from baseline: randomize
  xpectr::set_test_seed(42)
  # Assigning output
  output_12121 <- group_counts(data = df, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = TRUE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_12121),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_12121,
    type = "integer")
  # Testing values
  expect_equal(
    output_12121,
    c(3, 3, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_12121),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_12121),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_12121)),
    3L)

  # Testing group_counts(data = df, n = 3, method = "n_dis...
  # Changed from baseline: randomize
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = NULL, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'randomize': Must be of t",
                         "ype 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing group_counts(data = df, n = 3, method = "n_dis...
  # Changed from baseline: remove_missing_starts
  xpectr::set_test_seed(42)
  # Assigning output
  output_11255 <- group_counts(data = df, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = TRUE)
  # Testing class
  expect_equal(
    class(output_11255),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_11255,
    type = "integer")
  # Testing values
  expect_equal(
    output_11255,
    c(3, 3, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_11255),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_11255),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_11255)),
    3L)

  # Testing group_counts(data = df, n = 3, method = "n_dis...
  # Changed from baseline: remove_missing_starts
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = 3, method = "n_dist", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = NULL)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'remove_missing_starts': ",
                         "Must be of type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing group_counts(data = df, n = 3, method = "n_dis...
  # Changed from baseline: starts_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = 3, method = "n_dist", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * when method is not 'l_starts', 'st",
                         "arts_col' must be 'NULL'.")),
    fixed = TRUE)

  # Testing group_counts(data = df, n = 3, method = "n_dis...
  # Changed from baseline: starts_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = 3, method = "n_dist", starts_col = "f", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * when method is not 'l_starts', 'st",
                         "arts_col' must be 'NULL'.")),
    fixed = TRUE)

  ## Finished testing 'group_counts'                                          ####
  #

})

test_that("fuzz testing input checks for l_starts method in group_factor()", {
  xpectr::set_test_seed(1)

  df <- data.frame(
    "n" = c(1, 2, 3, 4, 2, 1, 5, 2, 1, 9),
    "s" = c(4, 4, 6, 6, 7, 7, 7, 8, 8, 1),
    "c" = as.character(c(4, 4, 6, 6, 7, 7, 7, 8, 8, 1)),
    "f" = as.factor(as.character(c(4, 4, 6, 6, 7, 7, 7, 8, 8, 1))),
    stringsAsFactors = FALSE
  )

  group_counts <- function(...) {
    gf <- group_factor(...)
    counts <- plyr::count(gf)
    counts$freq
  }

  # Tests focused on n_dist method
  xpectr::set_test_seed(1)
  # xpectr::gxs_function(group_counts,
  #                      args_values = list(
  #                        "data" = list(df, df$s, df$c, df$f, NA, 1),
  #                        "n" = list("auto", list(6, c(7, 2)), 7, 3, 0.4,
  #                                   c(0.2, 4), 0, "hej"),
  #                        "method" = list("l_starts", "l_sizes", "n_dist"),
  #                        "starts_col" = list("s", "f", NA),
  #                        "force_equal" = list(FALSE, TRUE),
  #                        "allow_zero" = list(FALSE, TRUE),
  #                        "descending" = list(FALSE, TRUE),
  #                        "randomize" = list(FALSE, TRUE),
  #                        "remove_missing_starts" = list(FALSE, TRUE)
  #                      ), indentation = 2)


  ## Testing 'group_counts'                                                   ####
  ## Initially generated by xpectr
  # Testing different combinations of argument values

  # Testing group_counts(data = df, n = "auto", method = "...
  xpectr::set_test_seed(42)
  # Assigning output
  output_12655 <- group_counts(data = df, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_12655),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_12655,
    type = "integer")
  # Testing values
  expect_equal(
    output_12655,
    c(2, 2, 3, 2, 1),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_12655),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_12655),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_12655)),
    5L)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: allow_zero
  xpectr::set_test_seed(42)
  # Assigning output
  output_13721 <- group_counts(data = df, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = TRUE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_13721),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_13721,
    type = "integer")
  # Testing values
  expect_equal(
    output_13721,
    c(2, 2, 3, 2, 1),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_13721),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_13721),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_13721)),
    5L)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: allow_zero
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = NULL, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'allow_zero': Must be of ",
                         "type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing group_counts(data = df$s, n = "auto", method =...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df$s, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * when 'starts_col' is specified, 'd",
                         "ata' must be a data.frame.")),
    fixed = TRUE)

  # Testing group_counts(data = df$c, n = "auto", method =...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df$c, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * when 'starts_col' is specified, 'd",
                         "ata' must be a data.frame.")),
    fixed = TRUE)

  # Testing group_counts(data = df$f, n = "auto", method =...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df$f, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * when 'starts_col' is specified, 'd",
                         "ata' must be a data.frame.")),
    fixed = TRUE)

  # Testing group_counts(data = NA, n = "auto", method = "...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = NA, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("1 assertions failed:\n * 'data' cannot be 'NA'."),
    fixed = TRUE)

  # Testing group_counts(data = 1, n = "auto", method = "l...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = 1, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * when 'starts_col' is specified, 'd",
                         "ata' must be a data.frame.")),
    fixed = TRUE)

  # Testing group_counts(data = NULL, n = "auto", method =...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = NULL, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("1 assertions failed:\n * 'data' cannot be 'NULL'"),
    fixed = TRUE)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: descending
  xpectr::set_test_seed(42)
  # Assigning output
  output_10617 <- group_counts(data = df, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = TRUE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_10617),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_10617,
    type = "integer")
  # Testing values
  expect_equal(
    output_10617,
    c(2, 2, 3, 2, 1),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_10617),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_10617),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_10617)),
    5L)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: descending
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = NULL, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'descending': Must be of ",
                         "type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: force_equal
  xpectr::set_test_seed(42)
  # Assigning output
  output_11765 <- group_counts(data = df, n = "auto", method = "l_starts", starts_col = "s", force_equal = TRUE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_11765),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_11765,
    type = "integer")
  # Testing values
  expect_equal(
    output_11765,
    c(2, 2, 3, 2, 1),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_11765),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_11765),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_11765)),
    5L)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: force_equal
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = "auto", method = "l_starts", starts_col = "s", force_equal = NULL, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'force_equal': Must be of",
                         " type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = "auto", method = "l_sizes", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("2 assertions failed:\n * 'n' can only be character when met",
                         "hod is 'l_starts'.\n * when method is not 'l_starts', 'start",
                         "s_col' must be 'NULL'.")),
    fixed = TRUE)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = "auto", method = "n_dist", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("2 assertions failed:\n * 'n' can only be character when method is 'l_starts'.\n * when method is not 'l_starts', 'starts_col' must be 'NULL'."),
    fixed = TRUE)

  # Testing group_counts(data = df, n = "auto", method = N...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = "auto", method = NULL, starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'method': Must be of type",
                         " 'string', not 'NULL'.")),
    fixed = TRUE)

  # Testing group_counts(data = df, n = c(0.2, 4), method ...
  # Changed from baseline: n
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = c(0.2, 4), method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("group_factor: Start value \"0.2\" not found in vector."),
    fixed = TRUE)

  # Testing group_counts(data = df, n = 0, method = "l_sta...
  # Changed from baseline: n
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = 0, method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * 'n' was 0. If this is on purpose, ",
                         "set 'allow_zero' to 'TRUE'.")),
    fixed = TRUE)

  # Testing group_counts(data = df, n = "hej", method = "l...
  # Changed from baseline: n
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = "hej", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("group_factor: Start value \"hej\" not found in vector."),
    fixed = TRUE)

  # Testing group_counts(data = df, n = list(6, c(7, 2)), ...
  # Changed from baseline: n
  xpectr::set_test_seed(42)
  # Assigning output
  output_17774 <- group_counts(data = df, n = list(6, c(7, 2)), method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_17774),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_17774,
    type = "integer")
  # Testing values
  expect_equal(
    output_17774,
    c(2, 3, 5),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_17774),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_17774),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_17774)),
    3L)

  # Testing group_counts(data = df, n = 7, method = "l_sta...
  # Changed from baseline: n
  xpectr::set_test_seed(42)
  # Assigning output
  output_19347 <- group_counts(data = df, n = 7, method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_19347),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19347,
    type = "integer")
  # Testing values
  expect_equal(
    output_19347,
    c(4, 6),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19347),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19347),
    2L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19347)),
    2L)

  # Testing group_counts(data = df, n = 3, method = "l_sta...
  # Changed from baseline: n
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = 3, method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("group_factor: Start value \"3\" not found in vector."),
    fixed = TRUE)

  # Testing group_counts(data = df, n = 0.4, method = "l_s...
  # Changed from baseline: n
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = 0.4, method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("group_factor: Start value \"0.4\" not found in vector."),
    fixed = TRUE)

  # Testing group_counts(data = df, n = NULL, method = "l_...
  # Changed from baseline: n
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = NULL, method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip("1 assertions failed:\n * 'n' cannot be 'NULL'"),
    fixed = TRUE)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: randomize
  xpectr::set_test_seed(42)
  # Assigning output
  output_12672 <- group_counts(data = df, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = TRUE, remove_missing_starts = FALSE)
  # Testing class
  expect_equal(
    class(output_12672),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_12672,
    type = "integer")
  # Testing values
  expect_equal(
    output_12672,
    c(2, 2, 3, 2, 1),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_12672),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_12672),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_12672)),
    5L)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: randomize
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = NULL, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'randomize': Must be of t",
                         "ype 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: remove_missing_starts
  xpectr::set_test_seed(42)
  # Assigning output
  output_10133 <- group_counts(data = df, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = TRUE)
  # Testing class
  expect_equal(
    class(output_10133),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_10133,
    type = "integer")
  # Testing values
  expect_equal(
    output_10133,
    c(2, 2, 3, 2, 1),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_10133),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_10133),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_10133)),
    5L)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: remove_missing_starts
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = "auto", method = "l_starts", starts_col = "s", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = NULL)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'remove_missing_starts': ",
                         "Must be of type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: starts_col
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_18696 <- xpectr::capture_side_effects(group_counts(data = df, n = "auto", method = "l_starts", starts_col = "f", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_18696[['warnings']]),
    xpectr::strip("'data[[starts_col]]' is factor. Converting to character."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_18696[['messages']]),
    xpectr::strip(character(0)),
    fixed = TRUE)
  # Assigning output
  output_18696 <- xpectr::suppress_mw(group_counts(data = df, n = "auto", method = "l_starts", starts_col = "f", force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE))
  # Testing class
  expect_equal(
    class(output_18696),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_18696,
    type = "integer")
  # Testing values
  expect_equal(
    output_18696,
    c(2, 2, 3, 2, 1),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_18696),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_18696),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_18696)),
    5L)

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: starts_col
  xpectr::set_test_seed(42)
  # Testing side effects

  if (FALSE){
    # TODO Fix when checkmate is updated
    expect_error(
      xpectr::strip_msg(group_counts(data = df, n = "auto", method = "l_starts", starts_col = NA, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
      xpectr::strip(paste0("Assertion failed. One of the following must apply:\n * chec",
                           "kmate::check_string(starts_col): May not be NA\n * checkmate",
                           "::check_count(starts_col): May not be NA")),
      fixed = TRUE)
  }

  # Testing group_counts(data = df, n = "auto", method = "...
  # Changed from baseline: starts_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group_counts(data = df, n = "auto", method = "l_starts", starts_col = NULL, force_equal = FALSE, allow_zero = FALSE, descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * when 'method' is 'l_starts' and 'd",
                         "ata' is a data.frame, 'starts_col' must be specified.")),
    fixed = TRUE)

  ## Finished testing 'group_counts'                                          ####
  #

})


test_that("group_factor() works with group_by()", {
  xpectr::set_test_seed(42)

  df <- data.frame(
    "n" = c(1, 2, 3, 4, 2, 1, 5, 2, 1, 9),
    "s" = c(4, 4, 4, 4, 7, 7, 7, 7, 1, 1),
    "c" = as.character(c(4, 4, 6, 6, 7, 7, 7, 8, 8, 1)),
    "f" = as.factor(as.character(c(4, 4, 6, 6, 7, 7, 7, 8, 8, 1))),
    stringsAsFactors = FALSE
  )


  ## Testing 'xpectr::suppress_mw(df %>% dplyr::group_by(s...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- xpectr::suppress_mw(df %>%
                          dplyr::group_by(s) %>%
                          group_factor(n = 2))
  # Testing class
  expect_equal(
    class(output_19148),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19148[["s"]],
    c(4, 4, 4, 4, 7, 7, 7, 7, 1, 1),
    tolerance = 1e-4)
  expect_equal(
    output_19148[[".groups"]],
    structure(c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 2L), .Label = c("1",
      "2"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_19148),
    c("s", ".groups"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_19148),
    c("numeric", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_19148),
    c("double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_19148),
    c(10L, 2L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_19148)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'xpectr::suppress_mw(df %>% dplyr::group_by(s...'     ####


})

