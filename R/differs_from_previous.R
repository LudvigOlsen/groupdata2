

## differs_from_previous
#' @title Find values in a vector that differ from the previous value
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  Finds values, or indices of values, that differ from the previous value by some threshold(s).
#'
#'  Operates with both a positive and a negative threshold.
#'  Depending on \code{`direction`}, it checks if the difference to the previous value is:
#'  \itemize{
#'    \item greater than or equal to the positive threshold.
#'    \item less than or equal to the negative threshold.
#'  }
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param data \code{data.frame} or \code{vector}.
#'
#'  \strong{N.B.} If checking a \code{factor}, it is converted to a \code{character vector}.
#'  This means that factors can only be used when \code{`threshold`} is \code{NULL}.
#'  Conversion will generate a warning, which can be turned off by setting \code{`factor_conversion_warning`} to \code{FALSE}.
#'
#'  \strong{N.B.} If \code{`data`} is a \emph{grouped} \code{data.frame},
#'  the function is applied group-wise and the output is a \code{list} of \code{vector}s.
#'  The names are based on the group indices
#'  (see \code{\link[dplyr:group_data]{dplyr::group_indices()}}).
#'
#' @param threshold Threshold to check difference to previous value to.
#'
#'  \code{NULL}, \emph{numeric scalar} or \emph{numeric vector with length \code{2}}.
#'
#'  \subsection{NULL}{
#'  Checks if the value is different from the previous value.
#'
#'  Ignores \code{`direction`}.
#'
#'  N.B. Works for both numeric and character vectors.
#'  }
#'  \subsection{Numeric scalar}{
#'  Positive number.
#'
#'  Negative threshold is the negated number.
#'
#'  N.B. Only works for numeric vectors.
#'  }
#'  \subsection{Numeric vector with length 2}{
#'  Given as \code{c(negative threshold, positive threshold)}.
#'
#'  Negative threshold must be a negative number and positive threshold must be a positive number.
#'
#'  N.B. Only works for numeric vectors.
#'  }
#'
#' @param direction
#'  \code{both}, \code{positive} or \code{negative}. (character)
#'  \subsection{both}{
#'  Checks whether the difference to the previous value is
#'    \itemize{
#'      \item greater than or equal to the positive threshold.
#'      \item less than or equal to the negative threshold.
#'    }
#'  }
#'  \subsection{positive}{
#'  Checks whether the difference to the previous value is
#'    \itemize{
#'      \item greater than or equal to the positive threshold.
#'    }
#'  }
#'  \subsection{negative}{
#'  Checks whether the difference to the previous value is
#'    \itemize{
#'      \item less than or equal to the negative threshold.
#'    }
#'  }
#' @param return_index Return indices of values that differ. (Logical)
#' @param col Name of column to find values that differ in. Used when \code{`data`} is
#'  \code{data.frame}. (Character)
#' @param include_first Whether to include the first element of the vector in the output. (Logical)
#' @param handle_na How to handle \code{NA}s in the column.
#'
#'  \subsection{"ignore"}{
#'  Removes the \code{NA}s before finding the differing values, ensuring
#'  that the first value after an \code{NA} will be correctly identified as new,
#'  if it differs from the value before the \code{NA}(s).
#'  }
#'
#'  \subsection{"as_element"}{
#'  Treats all \code{NA}s as the string \code{"NA"}.
#'  This means, that \code{threshold} must be \code{NULL} when using this method.
#'  }
#'
#'  \subsection{Numeric scalar}{
#'  A numeric value to replace \code{NA}s with.
#'  }
#' @param factor_conversion_warning Whether to throw a warning when converting a \code{factor} to a \code{character}. (Logical)
#' @return \code{vector} with either the differing values or the indices of the differing values.
#'
#'  \strong{N.B.} If \code{`data`} is a \emph{grouped} \code{data.frame},
#'  the output is a \code{list} of \code{vector}s
#'  with the differing values. The names are based on the group indices
#'  (see \code{\link[dplyr:group_data]{dplyr::group_indices()}}).
#' @aliases not_previous
#' @family l_starts tools
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "a" = factor(c("a", "a", "b", "b", "c", "c")),
#'   "n" = c(1, 3, 6, 2, 2, 4)
#' )
#'
#' # Get differing values in column 'a' with no threshold.
#' # This will simply check, if it is different to the previous value or not.
#' differs_from_previous(df, col = "a")
#'
#' # Get indices of differing values in column 'a' with no threshold.
#' differs_from_previous(df, col = "a", return_index = TRUE)
#'
#' # Get values, that are 2 or more greater than the previous value
#' differs_from_previous(df, col = "n", threshold = 2, direction = "positive")
#'
#' # Get values, that are 4 or more less than the previous value
#' differs_from_previous(df, col = "n", threshold = 4, direction = "negative")
#'
#' # Get values, that are either 2 or more greater than the previous value
#' # or 4 or more less than the previous value
#' differs_from_previous(df, col = "n", threshold = c(-4, 2), direction = "both")
differs_from_previous <- function(data,
                                  col = NULL,
                                  threshold = NULL,
                                  direction = "both",
                                  return_index = FALSE,
                                  include_first = FALSE,
                                  handle_na = "ignore",
                                  factor_conversion_warning = TRUE) {
  #
  # Run find_different_from_previous_vec_ for either a vector or data frame
  #

  # Check inputs
  checks <- check_differs_from_previous_once(
    data = data,
    col = col,
    threshold = threshold,
    direction = direction,
    return_index = return_index,
    include_first = include_first,
    handle_na = handle_na,
    factor_conversion_warning = factor_conversion_warning
  )

  data <- checks[["data"]]

  # Apply by group (recursion)
  if (dplyr::is_grouped_df(data)) {
    warn_once_about_group_by("differs_from_previous")
  }

  run_by_group_list(
    data = data,
    .fn = run_differs_from_previous_,
    col = col,
    threshold = threshold,
    direction = direction,
    return_index = return_index,
    include_first = include_first,
    handle_na = handle_na,
    factor_conversion_warning = factor_conversion_warning
  )

}


run_differs_from_previous_ <- function(data,
                                       col,
                                       threshold,
                                       direction,
                                       return_index,
                                       include_first,
                                       handle_na,
                                       factor_conversion_warning) {

  check_differs_from_previous_always(data = data)

  if (is.data.frame(data)) {
    v <- data[[col]]
  } else {
    v <- data
  }

  # Create and return start values or indices of values that differ from the previous value

  find_different_from_previous_vec_(
    v,
    threshold = threshold,
    direction = direction,
    return_index = return_index,
    include_first = include_first,
    handle_na = handle_na
  )

}


check_differs_from_previous_once <- function(data,
                                             col,
                                             threshold,
                                             direction,
                                             return_index,
                                             include_first,
                                             handle_na,
                                             factor_conversion_warning) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  if (is.null(data)) {
    assert_collection$push("'data' cannot be 'NULL'")
  }
  if (!is.data.frame(data) && length(data) == 1 && is.na(data)) {
    assert_collection$push("'data' cannot be 'NA'.")
  }
  checkmate::assert_flag(x = return_index, add = assert_collection)
  checkmate::assert_flag(x = include_first, add = assert_collection)
  checkmate::assert_flag(x = factor_conversion_warning, add = assert_collection)
  checkmate::assert_numeric(
    x = threshold,
    min.len = 1,
    max.len = 2,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_string(x = col, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(x = direction, add = assert_collection)
  checkmate::assert(
    checkmate::check_string(x = handle_na),
    checkmate::check_number(x = handle_na, finite = TRUE),
    .var.name = "handle_na"
  )
  checkmate::reportAssertions(assert_collection)
  if (length(threshold) == 2) {
    if (threshold[[1]] >= 0) {
      assert_collection$push("when 'threshold' has length 2, 'threshold[[1]]' must be a negative number.")
    }
    if (threshold[[2]] <= 0) {
      assert_collection$push("'threshold[[2]]' must be a positive number.")
    }
  }
  checkmate::assert(
    checkmate::check_data_frame(
      x = data,
      min.cols = 1,
      min.rows = 1
    ),
    checkmate::check_vector(
      x = data,
      min.len = 1,
      strict = TRUE
    ),
    checkmate::check_factor(x = data, min.len = 1),
    .var.name = "data"
  )
  checkmate::assert_names(
    x = direction,
    subset.of = c("both", "positive", "negative"),
    add = assert_collection
  )
  if (checkmate::test_string(x = handle_na)) {
    checkmate::assert_names(
      x = handle_na,
      subset.of = c("ignore", "as_element"),
      add = assert_collection
    )
  }

  checkmate::reportAssertions(assert_collection)

  # If data is a data frame
  if (is.data.frame(data)) {
    if (is.null(col)) {
      # If not, raise error
      assert_collection$push("'col' must be specified when 'data' is data.frame.")
      checkmate::reportAssertions(assert_collection)
    }
    if (col %ni% colnames(data)) {
      assert_collection$push("'col' was not found in 'data'.")
      checkmate::reportAssertions(assert_collection)
    }

    # If col is a factor
    if (is.factor(data[[col]])) {
      if (!is.null(threshold)) {
        assert_collection$push(
          "'col' is factor. 'threshold' must be 'NULL'. Alternatively, convert factor to numeric vector."
        )
      }
      if (isTRUE(factor_conversion_warning)) {
        warning("'col' is factor. Using as character.")
      }
      # Convert col to character
      data[[col]] <- as.character(data[[col]])
    }
  } else {
    if (!is.null(col)){
      warning("'col' is ignored when 'data' is not a data.frame.")
    }
    # If data is a factor
    if (is.factor(data)) {
      if (isTRUE(factor_conversion_warning)) {
        warning("'data' is factor. Using as character.")
      }
      # Convert data to character
      data <- as.character(data)
    }
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  list("data" = data)

}


check_differs_from_previous_always <- function(data) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()

  checkmate::assert(
    checkmate::check_data_frame(
      x = data,
      min.cols = 1,
      min.rows = 1
    ),
    checkmate::check_vector(
      x = data,
      min.len = 1,
      strict = TRUE
    ),
    checkmate::check_factor(x = data, min.len = 1),
    .var.name = "data"
  )

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####
}

find_different_from_previous_vec_ <- function(v,
                                              threshold = NULL,
                                              direction = "both",
                                              return_index = FALSE,
                                              handle_na = "ignore",
                                              include_first = FALSE) {
  #
  # Find values or index at which
  # value changes in vector
  # E.g. vector c(1,1,2,2,2,3,3) would return
  # values c(1,2,3) or indices c(1,3,6)
  # Uses a kind of rolling windows to determine
  # if a value is the same as the previous or new

  # Threshold can be a numeric scalar, a numeric vector of length 2, or NULL.
  # Threshold is inclusive. I.e. if threshold is 2 and the difference is 2, it's a match.
  # If threshold is NULL
  # .. TRUE if value is different than previous value.
  # .. .. Works for both numeric and character vectors.
  # If threshold is numeric scalar
  # .. Depending on direction, the difference between the value
  # .. and the previous value is checked against the threshold.
  # If threshold is a numeric vector of length 2
  # .. The first element is the negative threshold (and must be a negative number).
  # .. .. Returns TRUE if the difference from the previous value is negative and below or equal to this threshold.
  # .. The second element is the positive threshold (and must be a positive number).
  # .. .. Returns TRUE if the difference from the previous value is positive and above or equal to this threshold.

  # Direction is used when threshold is not NULL.
  # If direction is 'both'
  # .. Check whether the difference to the previous value is
  # .. .. greater than or equal to the positive threshold
  # .. .. less than or equal to the negative threshold
  # If direction is 'positive'
  # .. Check whether the difference to the previous value is
  # .. .. greater than or equal to the positive threshold
  # If direction is 'negative'
  # .. Check whether the difference to the previous value is
  # .. .. less than or equal to the negative threshold

  v_orig <- v

  contains_na <- anyNA(v)
  if (length(handle_na) > 1) {
    stop("'handle_na' had length > 1.")
  }
  if (handle_na == "as_element" &&
      !is.null(threshold)) {
    stop("when 'handle_na' is 'as_element', 'threshold' must be NULL.")
  }

  ## Handle NAs
  if (isTRUE(contains_na)) {
    if (handle_na == "ignore") {
      not_na_indices <- which(!is.na(v))
      v <- v[!is.na(v)]
    } else if (handle_na == "as_element") {
      v[is.na(v)] <- "NA"
    } else if (is.numeric(handle_na)) {
      v[is.na(v)] <- handle_na
      v_orig <- v
    } else {
      stop(
        "'handle_na' must be either a method ('ignore' or 'convert') or a value to replace NAs with."
      )
    }
  }

  # Adds vector to data frame
  # Creates a new column shifted down one row.
  # Checks if the current value is the same as the previous.

  # Shift / offset v one row down
  # Insert v[1] at beginning and remove last element of v
  # to get same length as v
  v2 <- c(v[1], v[seq_along(v) - 1])

  # Create data frame with v, v2 and
  # a logical column stating whether
  # v is new or not.
  if (!is.null(threshold)) {
    if (!is.numeric(threshold)) {
      stop("'threshold' must be numeric scalar, a numeric vector of length 2, or NULL.")
    }
    if (length(threshold) == 2) {
      if (threshold[1] >= 0) {
        stop("When 'threshold' is a vector of length 2, the first element must be negative.")
      }
      if (threshold[2] <= 0) {
        stop("When 'threshold' is a vector of length 2, the second element must be positive.")
      }
      if (direction != "both") {
        stop("When 'threshold' is a vector of length 2, 'direction' must be 'both'.")
      }

      neg_threshold <- threshold[1]
      threshold <- threshold[2]
    } else if (length(threshold) == 1) {
      if (threshold <= 0) {
        stop("When 'threshold' is a scalar it must be a positive number.")
      }

      neg_threshold <- -threshold
    } else {
      stop("'threshold' must be numeric scalar, a numeric vector of length 2, or NULL.")
    }

    if (direction == "both") {
      df <- data.frame(
        v,
        v2,
        new = !is_between_(v - v2, neg_threshold, threshold),
        stringsAsFactors = FALSE
      )
    } else if (direction == "positive") {
      df <- data.frame(v,
                       v2,
                       new = v - v2 >= threshold,
                       stringsAsFactors = FALSE)
    } else if (direction == "negative") {
      df <- data.frame(v,
                       v2,
                       new = v - v2 <= neg_threshold,
                       stringsAsFactors = FALSE)
    } else {
      stop("'direction' must be one of 'both', 'negative', and 'positive'.")
    }
  } else {
    df <- data.frame(v, v2,
                     new = v != v2,
                     stringsAsFactors = FALSE)
  }

  if (isTRUE(include_first)) {
    # Set first value to TRUE
    df[["new"]][1] <- TRUE
  }

  # Add back NA rows
  if (isTRUE(contains_na) && handle_na == "ignore") {
    df[["orig_indices"]] <- not_na_indices
    # Get indices where v contains a new value
    new_indices <- df[["orig_indices"]][df[["new"]]]
  } else {
    # Get indices where v contains a new value
    new_indices <- which(df[["new"]])
  }

  # If return_index is TRUE
  if (isTRUE(return_index)) {
    # Return only the indices where
    # v contains a new value
    return(new_indices)
  } else {
    # Return values at the indices
    return(v_orig[new_indices])
  }
}
