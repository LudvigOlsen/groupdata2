library(groupdata2)
context("group()")

# Needs testing of vector and factor as input

test_that("dimensions of data frame with group()", {
  xpectr::set_test_seed(1)

  df <- data.frame(
    "x" = c(1:12),
    "species" = factor(rep(c("cat", "pig", "human"), 4)),
    "age" = c(5, 65, 34, 54, 32, 54, 23, 65, 23, 65, 87, 98)
  )

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
  xpectr::set_test_seed(1)

  # Create df 3x12
  df <- data.frame(
    "x" = c(1:12),
    "species" = factor(rep(c("cat", "pig", "human"), 4)),
    "age" = c(5, 65, 34, 54, 32, 54, 23, 65, 23, 65, 87, 98)
  )

  int_mean_age <- function(df, n, method) {
    df_means <- group(df, n, method = method)
    df_means <- dplyr::summarise(df_means, mean_age = mean(age))

    return(as.integer(df_means$mean_age))
  }

  # group(df, 5, method = 'n_fill')

  expect_equal(int_mean_age(df, 5, "n_dist"), c(35, 44, 36, 44, 83))
  expect_equal(int_mean_age(df, 5, "n_fill"), c(34, 46, 44, 44, 92))
  expect_equal(int_mean_age(df, 5, "n_last"), c(35, 44, 43, 44, 68))

  expect_equal(int_mean_age(df, 7, "n_dist"), c(5, 49, 43, 54, 44, 44, 92))
  expect_equal(int_mean_age(df, 7, "n_fill"), c(35, 44, 43, 44, 44, 87, 98))
  expect_equal(int_mean_age(df, 7, "n_last"), c(5, 65, 34, 54, 32, 54, 60))

  # For n_rand test how many groups has been made
  expect_equal(length(int_mean_age(df, 5, "n_rand")), 5)
  expect_equal(length(int_mean_age(df, 7, "n_rand")), 7)
})

test_that("error messages work in group()", {
  xpectr::set_test_seed(1)

  # Create df 3x12
  df <- data.frame(
    "x" = c(1:12),
    "species" = factor(rep(c("cat", "pig", "human"), 4)),
    "age" = c(5, 65, 34, 54, 32, 54, 23, 65, 23, 65, 87, 98)
  )

  expect_error(
    xpectr::strip_msg(group(df, 13)),
    xpectr::strip("Assertion on 'nrow(data) >= n' failed: Must be TRUE."),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(group(df, 0)),
    xpectr::strip(paste0("1 assertions failed:\n * 'n' was 0. If this is on purpose, ",
                         "set 'allow_zero' to 'TRUE'.")),
    fixed = TRUE)

})

test_that("allow_zero works in group()", {
  xpectr::set_test_seed(1)

  # Create df 3x12
  df <- data.frame(
    "x" = c(1:12),
    "species" = factor(rep(c("cat", "pig", "human"), 4)),
    "age" = c(5, 65, 34, 54, 32, 54, 23, 65, 23, 65, 87, 98)
  )

  group_zero <- function(force_equal = FALSE) {
    return(group(df, 0,
      allow_zero = TRUE,
      force_equal = force_equal
    ))
  }

  na_col <- function() {
    grouped_df <- group(df, 0, allow_zero = TRUE)

    return(grouped_df$.groups)
  }

  # Check that the .groups column contains NAs
  expect_equal(na_col(), c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

  # We should still get the added grouping factor
  expect_equal(ncol(group_zero()), 4)
  # We should still have the same amount of rows
  expect_equal(nrow(group_zero()), 12)

  # The same with force_equal as there are no group sizes to force equal
  expect_equal(ncol(group_zero(force_equal = TRUE)), 4)
  expect_equal(nrow(group_zero(force_equal = TRUE)), 12)
})

test_that("col_name can be set correctly in group()", {
  xpectr::set_test_seed(1)

  # Create df 3x12
  df <- data.frame(
    "x" = c(1:12),
    "species" = factor(rep(c("cat", "pig", "human"), 4)),
    "age" = c(5, 65, 34, 54, 32, 54, 23, 65, 23, 65, 87, 98)
  )

  set_col_name <- function(df) {
    grouped_data <- group(df, 5, col_name = ".cats")

    return(colnames(grouped_data[4]))
  }

  expect_equal(set_col_name(df), ".cats")
})

test_that("l_starts can take n = auto", {
  xpectr::set_test_seed(1)

  df <- data.frame(
    "x" = c(1:12),
    "x2" = c(1, 1, 1, 2, NA, 2, 2, 3, NA, NA, 6, 6),
    "species" = rep(c("cat", "cat", "human", "human"), 3),
    "age" = c(5, 65, 34, 54, 32, 54, 23, 65, 23, 65, 87, 98),
    stringsAsFactors = FALSE
  )

  expect_equal(
    group(df,
      n = "auto", method = "l_starts",
      starts_col = "species"
    )$.groups,
    factor(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6))
  )
  expect_equal(
    group(df,
      n = "auto", method = "l_starts",
      starts_col = "x2"
    )$.groups,
    factor(c(1, 1, 1, 2, 3, 4, 4, 5, 6, 6, 7, 7))
  )


  expect_error(
    xpectr::strip_msg(group(df,
          n = "auto", method = "l_sizes",
          starts_col = "species")),
    xpectr::strip(paste0("2 assertions failed:\n * 'n' can only be character when met",
                         "hod is 'l_starts'.\n * when method is not 'l_starts', 'start",
                         "s_col' must be 'NULL'.")),
    fixed = TRUE)

})

test_that("l_starts can take starts_col = index / .index", {
  xpectr::set_test_seed(1)

  df <- data.frame(
    "x" = c(1:12),
    stringsAsFactors = FALSE
  )

  # index
  expect_equal(
    group(df, c(1, 4, 7),
      method = "l_starts",
      starts_col = "index"
    )$.groups,
    factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3))
  )

  # .index
  expect_equal(
    group(df, c(1, 4, 7),
      method = "l_starts",
      starts_col = ".index"
    )$.groups,
    factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3))
  )

  df2 <- data.frame(
    "x" = c(1:12),
    "index" = c(2:13),
    ".index" = c(3:14),
    stringsAsFactors = FALSE
  )

  expect_warning(expect_equal(
    group(df2, c(2, 7, 11),
      method = "l_starts",
      starts_col = ".index"
    )$.groups,
    factor(c(1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4))
  ),
  "data contains column named \'.index\' but this is ignored.",
  fixed = TRUE
  )

  expect_warning(expect_equal(
    group(df2, c(2, 7, 11),
      method = "l_starts",
      starts_col = "index"
    )$.groups,
    factor(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3))
  ),
  "'data' contains column named 'index'. This is used as starts_col instead",
  fixed = TRUE
  )
})


test_that("simple fuzz test of group()", {
  xpectr::set_test_seed(1)

  # NOTE: Most things already tested in group_factor,
  # so we just make a simple set of arg values

  df <- data.frame(
    "x" = c(1:12),
    "x2" = c(1, 1, 1, 2, NA, 2, 2, 3, NA, NA, 6, 6),
    "species" = rep(c("cat", "cat", "human", "human"), 3),
    "age" = c(5, 65, 34, 54, 32, 54, 23, 65, 23, 65, 87, 98),
    stringsAsFactors = FALSE
  )

  xpectr::set_test_seed(3)
  # xpectr::gxs_function(group,
  #                      args_values = list(
  #                        "data" = list(df, df$x),
  #                        "n" = list(3),
  #                        "method" = list("n_dist", "n_rand"),
  #                        "return_factor" = list(FALSE, TRUE),
  #                        "col_name" = list(".groups", "myGroups")
  #                      ), indentation = 2)


  ## Testing 'group'                                                          ####
  ## Initially generated by xpectr
  # Testing different combinations of argument values

  # Testing group(data = df, n = 3, method = "n_dist", ret...
  xpectr::set_test_seed(42)
  # Assigning output
  output_11680 <- group(data = df, n = 3, method = "n_dist", return_factor = FALSE, col_name = ".groups")
  # Testing class
  expect_equal(
    class(output_11680),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_11680[["x"]],
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    tolerance = 1e-4)
  expect_equal(
    output_11680[["x2"]],
    c(1, 1, 1, 2, NA, 2, 2, 3, NA, NA, 6, 6),
    tolerance = 1e-4)
  expect_equal(
    output_11680[["species"]],
    c("cat", "cat", "human", "human", "cat", "cat", "human", "human",
      "cat", "cat", "human", "human"),
    fixed = TRUE)
  expect_equal(
    output_11680[["age"]],
    c(5, 65, 34, 54, 32, 54, 23, 65, 23, 65, 87, 98),
    tolerance = 1e-4)
  expect_equal(
    output_11680[[".groups"]],
    structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), .Label = c("1",
      "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_11680),
    c("x", "x2", "species", "age", ".groups"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_11680),
    c("integer", "numeric", "character", "numeric", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_11680),
    c("integer", "double", "character", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_11680),
    c(12L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_11680)),
    ".groups",
    fixed = TRUE)

  # Testing group(data = df, n = 3, method = "n_dist", ret...
  # Changed from baseline: col_name
  xpectr::set_test_seed(42)
  # Assigning output
  output_18075 <- group(data = df, n = 3, method = "n_dist", return_factor = FALSE, col_name = "myGroups")
  # Testing class
  expect_equal(
    class(output_18075),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_18075[["x"]],
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    tolerance = 1e-4)
  expect_equal(
    output_18075[["x2"]],
    c(1, 1, 1, 2, NA, 2, 2, 3, NA, NA, 6, 6),
    tolerance = 1e-4)
  expect_equal(
    output_18075[["species"]],
    c("cat", "cat", "human", "human", "cat", "cat", "human", "human",
      "cat", "cat", "human", "human"),
    fixed = TRUE)
  expect_equal(
    output_18075[["age"]],
    c(5, 65, 34, 54, 32, 54, 23, 65, 23, 65, 87, 98),
    tolerance = 1e-4)
  expect_equal(
    output_18075[["myGroups"]],
    structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), .Label = c("1",
      "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_18075),
    c("x", "x2", "species", "age", "myGroups"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_18075),
    c("integer", "numeric", "character", "numeric", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_18075),
    c("integer", "double", "character", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_18075),
    c(12L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_18075)),
    "myGroups",
    fixed = TRUE)

  # Testing group(data = df, n = 3, method = "n_dist", ret...
  # Changed from baseline: col_name
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group(data = df, n = 3, method = "n_dist", return_factor = FALSE, col_name = NULL)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'col_name': Must be of ty",
                         "pe 'string', not 'NULL'.")),
    fixed = TRUE)

  # Testing group(data = df$x, n = 3, method = "n_dist", r...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Assigning output
  output_13277 <- group(data = df$x, n = 3, method = "n_dist", return_factor = FALSE, col_name = ".groups")
  # Testing class
  expect_equal(
    class(output_13277),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_13277[["data"]],
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    tolerance = 1e-4)
  expect_equal(
    output_13277[[".groups"]],
    structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), .Label = c("1",
      "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_13277),
    c("data", ".groups"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_13277),
    c("integer", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_13277),
    c("integer", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_13277),
    c(12L, 2L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_13277)),
    ".groups",
    fixed = TRUE)

  # Testing group(data = NULL, n = 3, method = "n_dist", r...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group(data = NULL, n = 3, method = "n_dist", return_factor = FALSE, col_name = ".groups")),
    xpectr::strip("1 assertions failed:\n * 'data' cannot be 'NULL'"),
    fixed = TRUE)

  # Testing group(data = df, n = 3, method = "n_rand", ret...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Assigning output
  output_16043 <- group(data = df, n = 3, method = "n_rand", return_factor = FALSE, col_name = ".groups")
  # Testing class
  expect_equal(
    class(output_16043),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_16043[["x"]],
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    tolerance = 1e-4)
  expect_equal(
    output_16043[["x2"]],
    c(1, 1, 1, 2, NA, 2, 2, 3, NA, NA, 6, 6),
    tolerance = 1e-4)
  expect_equal(
    output_16043[["species"]],
    c("cat", "cat", "human", "human", "cat", "cat", "human", "human",
      "cat", "cat", "human", "human"),
    fixed = TRUE)
  expect_equal(
    output_16043[["age"]],
    c(5, 65, 34, 54, 32, 54, 23, 65, 23, 65, 87, 98),
    tolerance = 1e-4)
  expect_equal(
    output_16043[[".groups"]],
    structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), .Label = c("1",
      "2", "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_16043),
    c("x", "x2", "species", "age", ".groups"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_16043),
    c("integer", "numeric", "character", "numeric", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_16043),
    c("integer", "double", "character", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_16043),
    c(12L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_16043)),
    ".groups",
    fixed = TRUE)

  # Testing group(data = df, n = 3, method = NULL, return_...
  # Changed from baseline: method
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group(data = df, n = 3, method = NULL, return_factor = FALSE, col_name = ".groups")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'method': Must be of type",
                         " 'string', not 'NULL'.")),
    fixed = TRUE)

  # Testing group(data = df, n = NULL, method = "n_dist", ...
  # Changed from baseline: n
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group(data = df, n = NULL, method = "n_dist", return_factor = FALSE, col_name = ".groups")),
    xpectr::strip("1 assertions failed:\n * 'n' cannot be 'NULL'"),
    fixed = TRUE)

  # Testing group(data = df, n = 3, method = "n_dist", ret...
  # Changed from baseline: return_factor
  xpectr::set_test_seed(42)
  # Assigning output
  output_15776 <- group(data = df, n = 3, method = "n_dist", return_factor = TRUE, col_name = ".groups")
  # Testing is factor
  expect_true(
    is.factor(output_15776))
  # Testing values
  expect_equal(
    as.character(output_15776),
    c("1", "1", "1", "1", "2", "2", "2", "2", "3", "3", "3", "3"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_15776),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_15776),
    12L)
  # Testing number of levels
  expect_equal(
    nlevels(output_15776),
    3L)
  # Testing levels
  expect_equal(
    levels(output_15776),
    c("1", "2", "3"),
    fixed = TRUE)

  # Testing group(data = df, n = 3, method = "n_dist", ret...
  # Changed from baseline: return_factor
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(group(data = df, n = 3, method = "n_dist", return_factor = NULL, col_name = ".groups")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'return_factor': Must be ",
                         "of type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  ## Finished testing 'group'                                                 ####
  #

})

test_that("group() works with group_by()", {
  xpectr::set_test_seed(42)

  df <- data.frame(
    "n" = c(1, 2, 3, 4, 2, 1, 5, 2, 1, 9),
    "s" = c(4, 4, 4, 4, 7, 7, 7, 7, 1, 1),
    "c" = as.character(c(4, 4, 6, 6, 7, 7, 7, 8, 8, 1)),
    "f" = as.factor(as.character(c(4, 4, 6, 6, 7, 7, 7, 8, 8, 1))),
    stringsAsFactors = FALSE
  )

  ## Testing 'xpectr::suppress_mw( df %>% dplyr::group_by(...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- xpectr::suppress_mw(
      df %>%
        dplyr::group_by(s) %>%
        group(n = 2)
    )
  # Testing class
  expect_equal(
    class(output_19148),
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19148[["n"]],
    c(1, 9, 1, 2, 3, 4, 2, 1, 5, 2),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["s"]],
    c(1, 1, 4, 4, 4, 4, 7, 7, 7, 7),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["c"]],
    c("8", "1", "4", "4", "6", "6", "7", "7", "7", "8"),
    fixed = TRUE)
  expect_equal(
    output_19148[["f"]],
    structure(c(5L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 4L, 5L), .Label = c("1",
      "4", "6", "7", "8"), class = "factor"))
  expect_equal(
    output_19148[[".groups"]],
    structure(c(1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L), .Label = c("1",
      "2"), class = "factor"))
  # Testing column names
  expect_equal(
    names(output_19148),
    c("n", "s", "c", "f", ".groups"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_19148),
    c("numeric", "numeric", "character", "factor", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_19148),
    c("double", "double", "character", "integer", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_19148),
    c(10L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_19148)),
    c("s", ".groups"),
    fixed = TRUE)
  ## Finished testing 'xpectr::suppress_mw( df %>% dplyr::group_by(...'     ####


})
