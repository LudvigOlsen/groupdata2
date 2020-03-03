
## splt
#' @title Split data by a range of methods.
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("stable")}
#'
#'  Divides data into groups by a range of methods.
#'  Splits data by these groups.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @inheritParams group_factor
#' @return List of the split data
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

  #
  # Takes data frame or vector
  # Splits into the specified windows
  # Returns list with the windows (data frames or vectors)
  #

  # If allow_zero is TRUE, and n is 0
  # .. Return the given data in a list
  # .. instead of giving an error
  if (isTRUE(allow_zero) && n == 0) {
    return(split(data, factor(1)))
  }

  # Check arguments
  # Convert n if given as percentage
  # Check more arguments
  n <- check_convert_check_(
    data = data,
    n =  n,
    method = method,
    force_equal = force_equal,
    allow_zero = allow_zero,
    descending = descending,
    starts_col = starts_col,
    remove_missing_starts = remove_missing_starts
  )

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
