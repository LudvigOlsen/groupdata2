
## splt
#' @title Split data by a range of methods
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("stable")}
#'
#'  Divides data into groups by a range of methods.
#'  Splits data by these groups.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @inheritParams group_factor
#' @return \code{list} of the split \code{`data`}.
#'
#'  \strong{N.B.} If \code{`data`} is a \emph{grouped} \code{data.frame}, there's an outer list
#'  for each group. The names are based on the group indices
#'  (see \code{\link[dplyr:group_indices]{dplyr::group_indices()}}).
#' @family grouping functions
#' @examples
#' # Attach packages
#' library(groupdata2)
#' library(dplyr)
#'
#' # Create data frame
#' df <- data.frame(
#'   "x" = c(1:12),
#'   "species" = factor(rep(c("cat", "pig", "human"), 4)),
#'   "age" = sample(c(1:100), 12)
#' )
#'
#' # Using splt()
#' df_list <- splt(df, 5, method = "n_dist")
splt <- function(data,
                 n,
                 method = "n_dist",
                 starts_col = NULL,
                 force_equal = FALSE,
                 allow_zero = FALSE,
                 descending = FALSE,
                 randomize = FALSE,
                 remove_missing_starts = FALSE) {

  # Check and prep inputs
  checks <- check_group_factor_once(
    data = data,
    n = n,
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
    warn_once_about_group_by("splt")
  }

  run_by_group_list(
    data = data,
    .fn = run_splt_,
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

run_splt_ <- function(data,
                      n,
                      method,
                      starts_col,
                      force_equal,
                      allow_zero,
                      descending,
                      randomize,
                      remove_missing_starts){

  # Checks and conversion of 'n'
  checks <- group_factor_check_convert_n(
    data = data,
    n = n,
    method = method,
    allow_zero = allow_zero
  )

  n <- checks[["n"]]

  # If allow_zero is TRUE, and n is 0
  # .. Return the given data in a list
  # .. instead of giving an error
  if (isTRUE(allow_zero) &&
      checkmate::test_number(n) &&
      n == 0) {
    return(split(data, factor(1)))
  }

  # Force equal
  # .. Some methods have a different way of calculating
  # .. "equality". They will do this themselves,
  # .. the others can be forced equal here.

  if (isTRUE(force_equal)) {
    if (!(method %in% c("staircase", "l_sizes", "l_starts"))) {

      # If force_equal is set to TRUE,
      # and we don't already have equally sized windows,
      # remove elements/rows from data, until we get
      # largest possible equally sized windows
      if (is.data.frame(data)) {
        if (!(is_wholenumber_(nrow(data) / n))) {

          # Multiply window size and number of windows to find
          # how much data to keep
          to_keep <- floor(nrow(data) / n) * n

          # Keep the first to_keep elements/rows
          data <- head(data, to_keep)
        }
      } else {
        if (!(is_wholenumber_(length(data) / n))) {

          # Multiply window size and number of windows to find
          # how much data to keep
          to_keep <- floor(length(data) / n) * n

          # Keep the first to_keep elements/rows
          data <- head(data, to_keep)
        }
      }
    }
  }


  # Split the data
  # .. Checks if data is data frame or vector
  # .. Calls the right splitter

  if (is.data.frame(data)) {
    spl <- dsplit_(
      data = data,
      n = n,
      method = method,
      starts_col = starts_col,
      force_equal = force_equal,
      allow_zero = allow_zero,
      descending = descending,
      randomize = randomize,
      remove_missing_starts = remove_missing_starts
    )
  } else {
    spl <- vsplit_(
      v = data,
      n = n,
      method = method,
      force_equal = force_equal,
      allow_zero = allow_zero,
      descending = descending,
      randomize = randomize,
      remove_missing_starts = remove_missing_starts
    )
  }

  spl

}
