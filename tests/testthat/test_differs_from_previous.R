library(groupdata2)
context("differs_from_previous()")

test_that("differs_from_previous() find the right values and indices", {
  xpectr::set_test_seed(1)

  v <- c("a", "a", "b", "c", "c", "d", "d")
  df <- data.frame(
    v = v,
    v2 = c(1, 1, 1, 2, 2, 2, 3),
    v3 = factor(c(1, 1, 1, 2, 2, 2, 3)),
    stringsAsFactors = FALSE
  )

  check_differs_from_previous <- function(col,
                                          threshold = NULL,
                                          direction = "both",
                                          return_index = FALSE,
                                          include_first = FALSE,
                                          factor_conversion_warning = FALSE) {
    return(differs_from_previous(df,
      col = col,
      threshold = threshold,
      direction = direction,
      return_index = return_index,
      include_first = include_first,
      factor_conversion_warning = factor_conversion_warning
    ))
  }

  # v
  expect_equal(check_differs_from_previous("v"), c("b", "c", "d"))
  expect_equal(check_differs_from_previous("v", return_index = TRUE), c(3, 4, 6))
  expect_equal(check_differs_from_previous("v", include_first = TRUE), c("a", "b", "c", "d"))
  expect_equal(check_differs_from_previous("v", include_first = TRUE, return_index = TRUE), c(1, 3, 4, 6))

  # v2
  expect_equal(check_differs_from_previous("v2"), c(2, 3))
  expect_equal(check_differs_from_previous("v2", return_index = TRUE), c(4, 7))
  expect_equal(check_differs_from_previous("v2", threshold = 10), numeric(0))
  expect_equal(check_differs_from_previous("v2", threshold = 10, include_first = TRUE), 1)
  expect_equal(check_differs_from_previous("v2", include_first = TRUE, return_index = TRUE), c(1, 4, 7))

  # v3 - notice: converts factors to characters

  expect_equal(check_differs_from_previous("v3"), c("2", "3"))
  expect_warning(expect_equal(
    check_differs_from_previous("v3", factor_conversion_warning = TRUE),
    c("2", "3")
  ),
  "'col' is factor. Using as character.",
  fixed = TRUE
  )
  expect_warning(expect_equal(
    check_differs_from_previous("v3", factor_conversion_warning = TRUE, include_first = TRUE),
    c("1", "2", "3")
  ),
  "'col' is factor. Using as character.",
  fixed = TRUE
  )

  expect_warning(expect_equal(
    check_differs_from_previous("v3", return_index = TRUE, factor_conversion_warning = TRUE),
    c(4, 7)
  ),
  "'col' is factor. Using as character.",
  fixed = TRUE
  )

  expect_error(
    check_differs_from_previous("v3",
      threshold = 2, return_index = TRUE,
      factor_conversion_warning = TRUE
    ),
    "'col' is factor. 'threshold' must be 'NULL'. Alternatively, convert factor to numeric vector.",
    fixed = TRUE
  )

  expect_error(differs_from_previous(df),
    "'col' must be specified when 'data' is data frame",
    fixed = TRUE
  )

  expect_warning(differs_from_previous(v, col = "a"),
    "'col' not used as 'data' is not a data frame",
    fixed = TRUE
  )

  expect_warning(differs_from_previous(factor(v)),
    "'data' is factor. Using as character.",
    fixed = TRUE
  )

  expect_warning(differs_from_previous(factor(v), return_index = TRUE),
    "'data' is factor. Using as character.",
    fixed = TRUE
  )

  expect_warning(differs_from_previous(df, col = "v3"),
    "'col' is factor. Using as character.",
    fixed = TRUE
  )

  expect_warning(differs_from_previous(df, col = "v3", return_index = TRUE),
    "'col' is factor. Using as character.",
    fixed = TRUE
  )

  expect_error(
    xpectr::strip_msg(differs_from_previous(df, col = "v", threshold = c(1, 2, 3))),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'threshold': Must have le",
                         "ngth <= 2, but has length 3.")),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(differs_from_previous(df, col = "v", threshold = c("a", "b", "c"))),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'threshold': Must be of t",
                         "ype 'numeric' (or 'NULL'), not 'character'.")),
    fixed = TRUE)

  expect_error(differs_from_previous(df, col = "v", threshold = -2),
    "When 'threshold' is a scalar it must be a positive number.",
    fixed = TRUE
  )

  expect_error(
    xpectr::strip_msg(differs_from_previous(df, col = "v", threshold = c(1, -2))),
    xpectr::strip(paste0("2 assertions failed:\n * when 'threshold' has length 2, 'th",
                         "reshold[[1]]' must be a negative number.\n * 'threshold[[2]]",
                         "' must be a positive number.")),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(differs_from_previous(df, col = "v", threshold = c(-1, -2))),
    xpectr::strip("1 assertions failed:\n * 'threshold[[2]]' must be a positive number."),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(differs_from_previous(df, col = "lol", threshold = c(-1, 2))),
    xpectr::strip("'col' was not found in 'data'."),
    fixed = TRUE)

  expect_error(
    xpectr::strip_msg(differs_from_previous(df, col = "v2", threshold = 10, direction = "greater")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'direction': Must be a su",
                         "bset of set {both,positive,negative}.")),
    fixed = TRUE)

  expect_error(differs_from_previous(df, col = "v2", threshold = c(-10, 10), direction = "negative"),
    "When 'threshold' is a vector of length 2, 'direction' must be 'both'.",
    fixed = TRUE
  )
  expect_error(differs_from_previous(df, col = "v2", threshold = c(-10, 10), direction = "positive"),
    "When 'threshold' is a vector of length 2, 'direction' must be 'both'.",
    fixed = TRUE
  )

  expect_equal(
    differs_from_previous(df, col = "v2", threshold = c(1), direction = "positive"),
    c(2, 3)
  )
  expect_equal(
    differs_from_previous(df, col = "v2", threshold = c(1), direction = "negative"),
    numeric()
  )
})

test_that("differs_from_previous() work with NAs", {
  xpectr::set_test_seed(1)

  v <- c("a", "a", "b", "c", NA, "d", "d", NA, "e", "e")
  df <- data.frame(
    v = v,
    v2 = c(1, 1, 1, 2, NA, 2, 3, NA, 4, 4),
    v3 = factor(c(1, 1, 1, 2, NA, 2, 3, NA, 4, 4)),
    stringsAsFactors = FALSE
  )

  check_differs_from_previous <- function(col,
                                          threshold = NULL,
                                          direction = "both",
                                          return_index = FALSE,
                                          include_first = FALSE,
                                          handle_na = "remove",
                                          factor_conversion_warning = FALSE) {
    return(differs_from_previous(df,
      col = col,
      threshold = threshold,
      direction = direction,
      return_index = return_index,
      include_first = include_first,
      handle_na = handle_na,
      factor_conversion_warning = factor_conversion_warning
    ))
  }

  # return start vals
  expect_equal(find_different_from_previous_vec_(df$v, handle_na = "ignore"), c("b", "c", "d", "e"))
  # Ensure that 1,NA,1 with 'ignore' does not create two group starts
  expect_equal(find_different_from_previous_vec_(c(0, 1, NA, 1, 2, NA, 3), handle_na = "ignore"), c(1, 2, 3))
  expect_equal(
    find_different_from_previous_vec_(df$v, handle_na = "as_element"),
    c("b", "c", NA, "d", NA, "e")
  )
  expect_equal(
    find_different_from_previous_vec_(df$v, handle_na = -1),
    c("b", "c", "-1", "d", "-1", "e")
  )
  expect_equal(
    find_different_from_previous_vec_(df$v2, handle_na = -1),
    c(2, -1, 2, 3, -1, 4)
  )
  expect_equal(
    check_differs_from_previous("v3", handle_na = -1),
    as.character(c(2, -1, 2, 3, -1, 4))
  )

  # return start indices
  expect_equal(find_different_from_previous_vec_(df$v,
    handle_na = "ignore",
    return_index = TRUE
  ), c(3, 4, 6, 9))
  expect_equal(find_different_from_previous_vec_(c(0, 1, NA, 1, 2, NA, 3),
    handle_na = "ignore",
    return_index = TRUE
  ), c(2, 5, 7))
  expect_equal(
    find_different_from_previous_vec_(df$v,
      handle_na = "as_element",
      return_index = TRUE
    ),
    c(3, 4, 5, 6, 8, 9)
  )
  expect_equal(
    find_different_from_previous_vec_(df$v,
      handle_na = -1,
      return_index = TRUE
    ),
    c(3, 4, 5, 6, 8, 9)
  )
  expect_equal(
    find_different_from_previous_vec_(df$v2,
      handle_na = -1,
      return_index = TRUE
    ),
    c(4, 5, 6, 7, 8, 9)
  )
  expect_equal(
    check_differs_from_previous("v3",
      handle_na = -1,
      return_index = TRUE
    ),
    c(4, 5, 6, 7, 8, 9)
  )

  # error
  expect_error(find_different_from_previous_vec_(df$v, handle_na = "leave"),
    "'handle_na' must be either a method ('ignore' or 'convert') or a value to replace NAs with.",
    fixed = TRUE
  )
})

test_that("fuzz testing input checks for differs_from_previous()", {
  xpectr::set_test_seed(1)

  df <- data.frame(
    "a" = factor(c("a", "a", "b", "b", "c", "c")),
    "n" = c(1, 3, 6, 2, 2, 4)
  )

  xpectr::set_test_seed(7)
  # xpectr::gxs_function(differs_from_previous,
  #                      args_values = list(
  #                        "data" = list(df, df$a, df$n, NA),
  #                        "col" = list("n", "a", NA, "t"),
  #                        "threshold" = list(NULL, 1, c(-1, 1), c(1, -1)),
  #                        "direction" = list("both", NA, "hellomama"), # test "positive", "negative" elsewhere
  #                        "return_index" = list(FALSE, TRUE, "naaah"),
  #                        "include_first" = list(TRUE, FALSE, "naaah"),
  #                        "handle_na" = list("ignore", "as_element", 8, factor(c("f"))),
  #                        "factor_conversion_warning" = list(TRUE, FALSE)
  #                      ), indentation = 2)


  ## Testing 'differs_from_previous'                                          ####
  ## Initially generated by xpectr
  # Testing different combinations of argument values

  # Testing differs_from_previous(data = df, col = "n", th...
  xpectr::set_test_seed(42)
  # Assigning output
  output_19889 <- differs_from_previous(data = df, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)
  # Testing class
  expect_equal(
    class(output_19889),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19889,
    type = "double")
  # Testing values
  expect_equal(
    output_19889,
    c(1, 3, 6, 2, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19889),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19889),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19889)),
    5L)

  # Testing differs_from_previous(data = df, col = "a", th...
  # Changed from baseline: col
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_13977 <- xpectr::capture_side_effects(differs_from_previous(data = df, col = "a", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_13977[['warnings']]),
    xpectr::strip("'col' is factor. Using as character."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_13977[['messages']]),
    xpectr::strip(character(0)),
    fixed = TRUE)
  # Assigning output
  output_13977 <- xpectr::suppress_mw(differs_from_previous(data = df, col = "a", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE))
  # Testing class
  expect_equal(
    class(output_13977),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_13977,
    type = "character")
  # Testing values
  expect_equal(
    output_13977,
    c("a", "b", "c"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_13977),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_13977),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_13977)),
    3L)

  # Testing differs_from_previous(data = df, col = NA, thr...
  # Changed from baseline: col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(differs_from_previous(data = df, col = NA, threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)),
    xpectr::strip("1 assertions failed:\n * Variable 'col': May not be NA."),
    fixed = TRUE)

  # Testing differs_from_previous(data = df, col = "t", th...
  # Changed from baseline: col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(differs_from_previous(data = df, col = "t", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)),
    xpectr::strip("'col' was not found in 'data'."),
    fixed = TRUE)

  # Testing differs_from_previous(data = df, col = NULL, t...
  # Changed from baseline: col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(differs_from_previous(data = df, col = NULL, threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)),
    xpectr::strip("'col' must be specified when 'data' is data frame."),
    fixed = TRUE)

  # Testing differs_from_previous(data = df$a, col = "n", ...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_17920 <- xpectr::capture_side_effects(differs_from_previous(data = df$a, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17920[['warnings']]),
    xpectr::strip(c("'data' is factor. Using as character.", "'col' not used as 'data' is not a data frame")),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17920[['messages']]),
    xpectr::strip(character(0)),
    fixed = TRUE)
  # Assigning output
  output_17920 <- xpectr::suppress_mw(differs_from_previous(data = df$a, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE))
  # Testing class
  expect_equal(
    class(output_17920),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_17920,
    type = "character")
  # Testing values
  expect_equal(
    output_17920,
    c("a", "b", "c"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_17920),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_17920),
    3L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_17920)),
    3L)

  # Testing differs_from_previous(data = df$n, col = "n", ...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_13400 <- xpectr::capture_side_effects(differs_from_previous(data = df$n, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_13400[['warnings']]),
    xpectr::strip("'col' not used as 'data' is not a data frame"),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_13400[['messages']]),
    xpectr::strip(character(0)),
    fixed = TRUE)
  # Assigning output
  output_13400 <- xpectr::suppress_mw(differs_from_previous(data = df$n, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE))
  # Testing class
  expect_equal(
    class(output_13400),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_13400,
    type = "double")
  # Testing values
  expect_equal(
    output_13400,
    c(1, 3, 6, 2, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_13400),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_13400),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_13400)),
    5L)

  # Testing differs_from_previous(data = NA, col = "n", th...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(differs_from_previous(data = NA, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)),
    xpectr::strip("1 assertions failed:\n * 'data' cannot be 'NA'."),
    fixed = TRUE)

  # Testing differs_from_previous(data = NULL, col = "n", ...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(differs_from_previous(data = NULL, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)),
    xpectr::strip("1 assertions failed:\n * 'data' cannot be 'NULL'"),
    fixed = TRUE)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: direction
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(differs_from_previous(data = df, col = "n", threshold = NULL, direction = NA, return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)),
    xpectr::strip("1 assertions failed:\n * Variable 'direction': May not be NA."),
    fixed = TRUE)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: direction
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(differs_from_previous(data = df, col = "n", threshold = NULL, direction = "hellomama", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'direction': Must be a su",
                         "bset of set {both,positive,negative}.")),
    fixed = TRUE)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: direction
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(differs_from_previous(data = df, col = "n", threshold = NULL, direction = NULL, return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'direction': Must be of t",
                         "ype 'string', not 'NULL'.")),
    fixed = TRUE)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: factor_conversion_warning
  xpectr::set_test_seed(42)
  # Assigning output
  output_17728 <- differs_from_previous(data = df, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = FALSE)
  # Testing class
  expect_equal(
    class(output_17728),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_17728,
    type = "double")
  # Testing values
  expect_equal(
    output_17728,
    c(1, 3, 6, 2, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_17728),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_17728),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_17728)),
    5L)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: factor_conversion_warning
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(differs_from_previous(data = df, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = NULL)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'factor_conversion_warnin",
                         "g': Must be of type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: handle_na
  xpectr::set_test_seed(42)
  # Assigning output
  output_14534 <- differs_from_previous(data = df, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "as_element", factor_conversion_warning = TRUE)
  # Testing class
  expect_equal(
    class(output_14534),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_14534,
    type = "double")
  # Testing values
  expect_equal(
    output_14534,
    c(1, 3, 6, 2, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_14534),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_14534),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_14534)),
    5L)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: handle_na
  xpectr::set_test_seed(42)
  # Assigning output
  output_10847 <- differs_from_previous(data = df, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = 8, factor_conversion_warning = TRUE)
  # Testing class
  expect_equal(
    class(output_10847),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_10847,
    type = "double")
  # Testing values
  expect_equal(
    output_10847,
    c(1, 3, 6, 2, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_10847),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_10847),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_10847)),
    5L)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: handle_na
  xpectr::set_test_seed(42)
  # Testing side effects
  if (FALSE){
    # TODO Fix when checkmate is updated!
    expect_error(
      xpectr::strip_msg(differs_from_previous(data = df, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = factor(c("f")), factor_conversion_warning = TRUE)),
      xpectr::strip(paste0("Assertion failed. One of the following must apply:\n * chec",
                           "kmate::check_string(handle_na): Must be of type 'string', no",
                           "t 'factor'\n * checkmate::check_number(handle_na): Must be o",
                           "f type 'number', not 'factor'")),
      fixed = TRUE)
  }

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: handle_na
  xpectr::set_test_seed(42)
  # Testing side effects
  if (FALSE){
    # TODO Fix when checkmate is updated!
    expect_error(
      xpectr::strip_msg(differs_from_previous(data = df, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = NULL, factor_conversion_warning = TRUE)),
      xpectr::strip(paste0("Assertion failed. One of the following must apply:\n * chec",
                           "kmate::check_string(handle_na): Must be of type 'string', no",
                           "t 'NULL'\n * checkmate::check_number(handle_na): Must be of ",
                           "type 'number', not 'NULL'")),
      fixed = TRUE)
  }

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: include_first
  xpectr::set_test_seed(42)
  # Assigning output
  output_19857 <- differs_from_previous(data = df, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = FALSE, handle_na = "ignore", factor_conversion_warning = TRUE)
  # Testing class
  expect_equal(
    class(output_19857),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19857,
    type = "double")
  # Testing values
  expect_equal(
    output_19857,
    c(3, 6, 2, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19857),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19857),
    4L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19857)),
    4L)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: include_first
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(differs_from_previous(data = df, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = "naaah", handle_na = "ignore", factor_conversion_warning = TRUE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'include_first': Must be ",
                         "of type 'logical flag', not 'character'.")),
    fixed = TRUE)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: include_first
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(differs_from_previous(data = df, col = "n", threshold = NULL, direction = "both", return_index = FALSE, include_first = NULL, handle_na = "ignore", factor_conversion_warning = TRUE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'include_first': Must be ",
                         "of type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: return_index
  xpectr::set_test_seed(42)
  # Assigning output
  output_12952 <- differs_from_previous(data = df, col = "n", threshold = NULL, direction = "both", return_index = TRUE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)
  # Testing class
  expect_equal(
    class(output_12952),
    "integer",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_12952,
    type = "integer")
  # Testing values
  expect_equal(
    output_12952,
    c(1, 2, 3, 4, 6),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_12952),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_12952),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_12952)),
    5L)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: return_index
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(differs_from_previous(data = df, col = "n", threshold = NULL, direction = "both", return_index = "naaah", include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'return_index': Must be o",
                         "f type 'logical flag', not 'character'.")),
    fixed = TRUE)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: return_index
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(differs_from_previous(data = df, col = "n", threshold = NULL, direction = "both", return_index = NULL, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'return_index': Must be o",
                         "f type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: threshold
  xpectr::set_test_seed(42)
  # Assigning output
  output_19887 <- differs_from_previous(data = df, col = "n", threshold = 1, direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)
  # Testing class
  expect_equal(
    class(output_19887),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19887,
    type = "double")
  # Testing values
  expect_equal(
    output_19887,
    c(1, 3, 6, 2, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_19887),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19887),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19887)),
    5L)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: threshold
  xpectr::set_test_seed(42)
  # Assigning output
  output_10656 <- differs_from_previous(data = df, col = "n", threshold = c(-1, 1), direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)
  # Testing class
  expect_equal(
    class(output_10656),
    "numeric",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_10656,
    type = "double")
  # Testing values
  expect_equal(
    output_10656,
    c(1, 3, 6, 2, 4),
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_10656),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_10656),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_10656)),
    5L)

  # Testing differs_from_previous(data = df, col = "n", th...
  # Changed from baseline: threshold
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(differs_from_previous(data = df, col = "n", threshold = c(1, -1), direction = "both", return_index = FALSE, include_first = TRUE, handle_na = "ignore", factor_conversion_warning = TRUE)),
    xpectr::strip(paste0("2 assertions failed:\n * when 'threshold' has length 2, 'th",
                         "reshold[[1]]' must be a negative number.\n * 'threshold[[2]]",
                         "' must be a positive number.")),
    fixed = TRUE)

  ## Finished testing 'differs_from_previous'                                 ####
  #

})
