
## group_factor
#' @title Create grouping factor for subsetting your data
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("stable")}
#'
#'  Divides data into groups by a wide range of methods.
#'  Creates and returns a grouping factor
#'  with \code{1}s for \emph{group 1}, \code{2}s for \emph{group 2}, etc.
#'
#'  By default, the data points in a group are connected sequentially (e.g. \code{c(1, 1, 2, 2, 3, 3)})
#'  and splitting is done from top to bottom.
#'
#'  There are \strong{four} types of grouping methods:
#'
#'  The \code{"n_*"} methods split the data into a given \emph{number of groups}.
#'  They differ in how they handle excess data points.
#'
#'  The \code{"greedy"} method uses a \emph{group size} to split the data into groups,
#'  greedily grabbing \code{`n`} data points from the top.
#'  The last group may thus differ in size (e.g. \code{c(1, 1, 2, 2, 3)}).
#'
#'  The \code{"l_*"} methods use a \emph{list} of either starting points (\code{"l_starts"})
#'  or group sizes (\code{"l_sizes"}). The \code{"l_starts"} method can also auto-detect group starts
#'  (when a value differs from the previous value).
#'
#'  The step methods \code{"staircase"} and \code{"primes"} increase the group size by a step for each group.
#'
#'  \strong{Note}: To create groups balanced by a categorical and/or numerical variable, see the
#'  \code{\link[groupdata2:fold]{fold()}} and \code{\link[groupdata2:partition]{partition()}} functions.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param data \code{data.frame} or \code{vector}.
#'  When a \emph{grouped} \code{data.frame}, the function is applied group-wise.
#' @param n \emph{Depends on \code{`method`}.}
#'
#'  Number of groups (default), group size, list of group sizes,
#'  list of group starts, step size or prime number to start at. See \code{`method`}.
#'
#'  Passed as whole number(s) and/or percentage(s) (\code{0} < \code{n} < \code{1})
#'  and/or character.
#'
#'  Method \code{"l_starts"} allows \code{'auto'}.
#' @param method \code{"greedy"}, \code{"n_dist"}, \code{"n_fill"}, \code{"n_last"},
#'  \code{"n_rand"}, \code{"l_sizes"}, \code{"l_starts"}, \code{"staircase"}, or
#'  \code{"primes"}.
#'
#'  \strong{Note}: examples are sizes of the generated groups
#'  based on a vector with \code{57} elements.
#'
#'  \subsection{greedy}{Divides up the data greedily given a specified group size
#'  \eqn{(e.g. 10, 10, 10, 10, 10, 7)}.
#'
#'  \code{`n`} is group size}
#'
#'  \subsection{n_dist (default)}{Divides the data into a specified number of groups and
#'  distributes excess data points across groups
#'  \eqn{(e.g. 11, 11, 12, 11, 12)}.
#'
#'  \code{`n`} is number of groups}
#'
#'  \subsection{n_fill}{Divides the data into a specified number of groups and
#'  fills up groups with excess data points from the beginning
#'  \eqn{(e.g. 12, 12, 11, 11, 11)}.
#'
#'  \code{`n`} is number of groups}
#'
#'  \subsection{n_last}{Divides the data into a specified number of groups.
#'  It finds the most equal group sizes possible,
#'  using all data points. Only the last group is able to differ in size
#'  \eqn{(e.g. 11, 11, 11, 11, 13)}.
#'
#'  \code{`n`} is number of groups}
#'
#'  \subsection{n_rand}{Divides the data into a specified number of groups.
#'  Excess data points are placed randomly in groups (max. 1 per group)
#'  \eqn{(e.g. 12, 11, 11, 11, 12)}.
#'
#'  \code{`n`} is number of groups}
#'
#'  \subsection{l_sizes}{Divides up the data by a \code{list} of group sizes.
#'  Excess data points are placed in an extra group at the end.
#'
#'  \eqn{E.g. n = list(0.2, 0.3) outputs groups with sizes (11, 17, 29)}.
#'
#'  \code{`n`} is a \code{list} of group sizes}
#'
#'  \subsection{l_starts}{Starts new groups at specified values in the \code{`starts_col`} vector.
#'
#'  \code{n} is a \code{list} of starting positions.
#'  Skip values by \code{c(value, skip_to_number)} where \code{skip_to_number} is the
#'  nth appearance of the value in the vector after the previous group start.
#'  The first data point is automatically a starting position.
#'
#'  \eqn{E.g. n = c(1, 3, 7, 25, 50) outputs groups with sizes (2, 4, 18, 25, 8)}.
#'
#'  To skip: \eqn{given vector c("a", "e", "o", "a", "e", "o"), n = list("a", "e", c("o", 2))
#'  outputs groups with sizes (1, 4, 1)}.}
#'
#'  If passing \eqn{n = 'auto'} the starting positions are automatically found
#'  such that a group is started whenever a value differs from the previous value
#'  (see \code{\link{find_starts}()}).
#'  Note that all \code{NA}s are first replaced by a single unique value,
#'  meaning that they will also cause group starts.
#'  See \code{\link{differs_from_previous}()}
#'  to set a threshold for what is considered "different".
#'
#'  \eqn{E.g. n = "auto" for c(10, 10, 7, 8, 8, 9) would start groups at
#'  the first 10, 7, 8 and 9, and give c(1, 1, 2, 3, 3, 4).}
#'
#'  \subsection{staircase}{Uses step size to divide up the data.
#'  Group size increases with 1 step for every group,
#'  until there is no more data
#'  \eqn{(e.g. 5, 10, 15, 20, 7)}.
#'
#'  \code{`n`} is step size}
#'
#'  \subsection{primes}{Uses prime numbers as group sizes.
#'  Group size increases to the next prime number
#'  until there is no more data.
#'  \eqn{(e.g. 5, 7, 11, 13, 17, 4)}.
#'
#'  \code{`n`} is the prime number to start at}
#' @param starts_col Name of column with values to match in method \code{"l_starts"}
#' when \code{`data`} is a \code{data.frame}. Pass \code{'index'} to use row names. (Character)
#' @param force_equal Create equal groups by discarding excess data points.
#'  Implementation varies between methods. (Logical)
#' @param allow_zero Whether \code{`n`} can be passed as \code{0}.
#' Can be useful when programmatically finding `n`. (Logical)
#' @param descending Change the direction of the method. (Not fully implemented)
#'  (Logical)
#' @param randomize Randomize the grouping factor. (Logical)
#' @param remove_missing_starts Recursively remove elements from the
#'  list of starts that are not found.
#'  For method \code{"l_starts"} only.
#'  (Logical)
#' @return Grouping factor with \code{1}s for group 1, \code{2}s for group 2, etc.
#'
#'  \strong{N.B.} If \code{`data`} is a \emph{grouped} \code{data.frame},
#'  the output is a \code{data.frame} with the existing groupings
#'  and the generated grouping factor. The row order from \code{`data`} is maintained.
#' @family grouping functions
#' @family staircase tools
#' @family l_starts tools
#' @examples
#' # Attach packages
#' library(groupdata2)
#' library(dplyr)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "x" = c(1:12),
#'   "species" = factor(rep(c("cat", "pig", "human"), 4)),
#'   "age" = sample(c(1:100), 12)
#' )
#'
#' # Using group_factor() with n_dist
#' groups <- group_factor(df, 5, method = "n_dist")
#' df$groups <- groups
#'
#' # Using group_factor() with greedy
#' groups <- group_factor(df, 5, method = "greedy")
#' df$groups <- groups
#'
#' # Using group_factor() with l_sizes
#' groups <- group_factor(df, list(0.2, 0.3), method = "l_sizes")
#' df$groups <- groups
#'
#' # Using group_factor() with l_starts
#' groups <- group_factor(df, list("cat", c("pig", 2), "human"),
#'   method = "l_starts", starts_col = "species"
#' )
#' df$groups <- groups
group_factor <- function(data, n, method = "n_dist", starts_col = NULL, force_equal = FALSE,
                         allow_zero = FALSE, descending = FALSE,
                         randomize = FALSE, remove_missing_starts = FALSE) {

  # Check and prep inputs
  checks <- check_group_factor_once(
    data = data, n = n,
    method = method,
    starts_col = starts_col,
    force_equal = force_equal,
    allow_zero = allow_zero,
    descending = descending,
    randomize = randomize,
    remove_missing_starts = remove_missing_starts)

  starts_col <- checks[["starts_col"]]

  # Apply by group (recursion)
  if (dplyr::is_grouped_df(data)) {
    message_once_about_group_by("group_factor")
  }

  run_by_group_col(
    data = data,
    .fn = run_group_factor_,
    .col_name = ".groups",
    n = n,
    method = method,
    starts_col = starts_col,
    force_equal = force_equal,
    allow_zero = allow_zero,
    descending = descending,
    randomize = randomize,
    remove_missing_starts = remove_missing_starts
  )

}

run_group_factor_ <- function(data, n, method, starts_col, force_equal,
                              allow_zero, descending,
                              randomize, remove_missing_starts){

  # Checks and conversion of 'n'
  checks <- group_factor_check_convert_n(
    data = data,
    n = n,
    method = method,
    allow_zero = allow_zero
  )

  n <- checks[["n"]]

  ### Allow zero ###

  # If allow_zero is TRUE and n is 0
  # return NAs instead of giving an error
  if (isTRUE(allow_zero) &&
      checkmate::test_number(n) &&
      n == 0) {
    if (is.data.frame(data)) {
      return(rep(NA, each = nrow(data)))
    } else {
      return(rep(NA, each = length(data)))
    }
  }

  # For method l_starts
  # If data is a data frame and starts_col is not NULL
  # We want to get the column with values to match

  if (method == "l_starts") {
    if (is.data.frame(data)){
      starts_col <- assign_starts_col(data = data, starts_col = starts_col)
    } else {
      if (is.factor(data)){
        warning("'data' is a factor. Converting to character.")
        data <- as.character(data)
      }
    }
  }

  # Set data as vector
  if (is.data.frame(data)) {
    # The grouping factor methods work on a vector
    if (!is.null(starts_col)) # for l_starts
      data <- starts_col
    else
      data <- seq_len(nrow(data))
  }

  # Create grouping factors
  if (method == "greedy") {
    groups <- greedy_group_factor_(
      v = data,
      size = n,
      force_equal = force_equal,
      descending = descending
    )
  } else if (method == "n_dist") {
    groups <- n_dist_group_factor_(
      v = data,
      n_windows =  n,
      force_equal = force_equal,
      descending = descending
    )
  } else if (method == "n_last") {
    groups <- n_last_group_factor_(
      v = data,
      n_windows = n,
      force_equal = force_equal,
      descending = descending
    )
  } else if (method == "n_fill") {
    groups <- n_fill_group_factor_(
      v = data,
      n_windows = n,
      force_equal = force_equal,
      descending = descending
    )
  } else if (method == "n_rand") {
    groups <- n_rand_group_factor_(
      v = data,
      n_windows = n,
      force_equal = force_equal,
      descending = descending
    )
  } else if (method == "l_sizes") {
    groups <- l_sizes_group_factor_(
      v = data,
      n = n,
      force_equal = force_equal,
      descending = descending
    )
  } else if (method == "l_starts") {
    groups <- l_starts_group_factor_(
      v = data, # is starts_col when originally data frame
      n = n,
      force_equal = force_equal,
      descending = descending,
      remove_missing_starts = remove_missing_starts
    )
  } else if (method == "staircase") {
    groups <- stair_split_group_factor_(
      v = data,
      step_size = n,
      force_equal = force_equal,
      descending = descending
    )
  } else if (method == "primes") {
    groups <- primes_split_group_factor_(
      v = data,
      start_at = n,
      force_equal = force_equal,
      descending = descending
    )
  }

  # Shuffle groups
  if (isTRUE(randomize)) {
    groups <- sample(groups)
  }

  # Return grouping factor
  groups
}

check_group_factor_once <- function(data, n, method, starts_col, force_equal,
                                    allow_zero, descending,
                                    randomize, remove_missing_starts,
                                    available_methods = c(
                                      "greedy", "n_dist", "n_fill", "n_last", "n_rand",
                                      "l_sizes", "l_starts", "staircase", "primes")){
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()

  if (is.null(n)){
    assert_collection$push("'n' cannot be 'NULL'")
  }
  if (is.null(data)){
    assert_collection$push("'data' cannot be 'NULL'")
  }
  if (!is.data.frame(data) && length(data) == 1 && is.na(data)){
    assert_collection$push("'data' cannot be 'NA'.")
  }
  checkmate::reportAssertions(assert_collection)

  checkmate::assert(
    checkmate::check_data_frame(x = data, min.cols = 1, min.rows = 1),
    checkmate::check_vector(x = data, min.len = 1, strict = TRUE),
    checkmate::check_factor(x = data, min.len = 1),
    .var.name = "data")
  checkmate::assert(
    checkmate::check_numeric(x = n, finite = TRUE,
                             any.missing = FALSE),
    checkmate::check_character(x = n, any.missing = FALSE),
    checkmate::check_list(x = n, types = c("character", "numeric", "list"),
                          any.missing = FALSE),
    .var.name = "n")
  checkmate::assert_string(x = method, min.chars = 1, add = assert_collection)
  checkmate::assert(
    checkmate::check_string(x = starts_col, min.chars = 1, null.ok = TRUE),
    checkmate::check_count(x = starts_col, positive = TRUE, null.ok = TRUE),
    .var.name = "starts_col")
  checkmate::assert_flag(x = force_equal, add = assert_collection)
  checkmate::assert_flag(x = allow_zero, add = assert_collection)
  checkmate::assert_flag(x = descending, add = assert_collection)
  checkmate::assert_flag(x = randomize, add = assert_collection)
  checkmate::assert_flag(x = remove_missing_starts, add = assert_collection)

  checkmate::reportAssertions(assert_collection)

  checkmate::assert_names(x = method, subset.of = available_methods,
                          what = "method", add = assert_collection)

  checkmate::reportAssertions(assert_collection)

  if (!is.null(starts_col)) {
    if (!is.data.frame(data)){
      assert_collection$push("when 'starts_col' is specified, 'data' must be a data.frame.")
      checkmate::reportAssertions(assert_collection)
    }
    if (is.character(starts_col) && starts_col %ni% c(colnames(data), "index", ".index")){
      assert_collection$push(paste0("'starts_col' column, '", starts_col, "', not found in 'data'."))
    } else if (is.numeric(starts_col) && ncol(data) < starts_col){
      assert_collection$push(
        paste0("'starts_col' was passed as a column index but was larger th",
               "an the number of columns in 'data'.")
      )
    }
    checkmate::reportAssertions(assert_collection)
    if (is.numeric(starts_col)) starts_col <- colnames(data)[[starts_col]]
    checkmate::assert(
      checkmate::check_string(x = starts_col, pattern = "^\\.?index$"),
      checkmate::check_numeric(x = data[[starts_col]]),
      checkmate::check_character(x = data[[starts_col]]),
      checkmate::check_factor(x = data[[starts_col]]),
      .var.name = "data[[starts_col]]"
    )
  }

  if (method != "l_starts") {
    if (is.character(n)){
      assert_collection$push("'n' can only be character when method is 'l_starts'.")
    }
    if (!is.null(starts_col)){
      assert_collection$push("when method is not 'l_starts', 'starts_col' must be 'NULL'.")
    }
  }
  if (is.list(n) && method %ni% c("l_starts", "l_sizes")){
    assert_collection$push("'n' can only be a list when method is either 'l_starts' or 'l_sizes'.")
  }
  if (method == "l_starts" &&
      is.data.frame(data) &&
      is.null(starts_col)){
    assert_collection$push("when 'method' is 'l_starts' and 'data' is a data.frame, 'starts_col' must be specified.")
  }

  checkmate::reportAssertions(assert_collection)

  list("starts_col" = starts_col)

}


group_factor_check_convert_n <- function(data, n, method,
                                         allow_zero) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()

  if (!isTRUE(allow_zero) &&
      checkmate::test_number(n) &&
      n == 0) {
    assert_collection$push("'n' was 0. If this is on purpose, set 'allow_zero' to 'TRUE'.")
    checkmate::reportAssertions(assert_collection)
  }

  # Convert n if given as single percentage
  n <-
    convert_n(
      n = n,
      data = data,
      method = method,
      allow_zero = allow_zero
    )

  # Check number of elements in n
  if (is.data.frame(data)) {
    if (length(n) > nrow(data)) {
      assert_collection$push("'n' cannot have more elements than the number of rows in 'data'.")
    }
  } else if (length(n) > length(data)) {
    assert_collection$push("'n' cannot have more elements than 'data'.")
  }

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  list("n" = n)

}
