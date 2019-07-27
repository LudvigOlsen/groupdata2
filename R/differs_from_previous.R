
## differs_from_previous
#' @title Find values in a vector that differ from the previous value.
#' @description Finds values, or indices of values, that differ from the previous value by some threshold(s).
#'
#'  Operates with both a positive and a negative threshold.
#'  Depending on \code{direction}, it checks if the difference to the previous value is:
#'  \itemize{
#'    \item greater than or equal to the positive threshold.
#'    \item less than or equal to the negative threshold.
#'  }
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param data Data frame or Vector
#'
#' N.B. If checking a factor, it is converted to a character vector.
#' This means that factors can only be used when \code{threshold} is \code{NULL}.
#' Conversion will generate a warning, which can be turned off by setting \code{factor_conversion_warning} to \code{FALSE}.
#'
#' @param threshold Threshold to check difference to previous value to.
#'
#'  \code{NULL}, numerical scalar or numerical vector with length 2.
#'
#'  \subsection{NULL}{
#'  Checks if the value is different from the previous value.
#'
#'  Ignores \code{direction}.
#'
#'  N.B. Works for both numerical and character vectors.
#'  }
#'  \subsection{Numerical scalar}{
#'  Positive number.
#'
#'  Negative threshold is the negated number.
#'
#'  N.B. Only works for numerical vectors.
#'  }
#'  \subsection{Numerical vector with length 2}{
#'  Given as \code{c(negative threshold, positive threshold)}.
#'
#'  Negative threshold must be a negative number and positive threshold must be a positive number.
#'
#'  N.B. Only works for numerical vectors.
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
#' @param col Name of column to find values that differ in. Used when data is
#'  data frame. (Character)
#' @param include_first Whether to include first element in vector in output. (Logical)
#' @param factor_conversion_warning Generate warning when converting factor to character. (Logical)
#' @return Vector with either differing values or indices of differing values.
#' @aliases not_previous
#' @family l_starts tools
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create a data frame
#' df <- data.frame('a' = c('a','a','b','b','c','c'),
#'                  'n' = c(1,3,6,2,2,4))
#'
#' # Get differing values in column 'a' with no threshold.
#' # This will simply check, if it is different to the previous value or not.
#' differs_from_previous(df, col = 'a')
#'
#' # Get indices of differing values in column 'a' with no threshold.
#' differs_from_previous(df, col = 'a', return_index = TRUE)
#'
#' # Get values, that are 2 or more greater than the previous value
#' differs_from_previous(df, col = 'n', threshold=2, direction="positive")
#'
#' # Get values, that are 4 or more less than the previous value
#' differs_from_previous(df, col = 'n', threshold=4, direction="negative")
#'
#' # Get values, that are either 2 or more greater than the previous value
#' # or 4 or more less than the previous value
#' differs_from_previous(df, col = 'n', threshold=c(-4,2), direction="both")
differs_from_previous <- function(data,
                                  col = NULL,
                                  threshold = NULL,
                                  direction = "both",
                                  return_index = FALSE,
                                  include_first = FALSE,
                                  factor_conversion_warning=TRUE) {
    #
    # Run find_different_from_previous_vec_ for either a vector or data frame
    #

    # If data is a data frame
    if (is.data.frame(data)) {
      # Check if col is specified
      if (is.null(col)) {
        # If not, raise error
        stop("col must be specified when data is data frame.")

      }
      if (col %ni% colnames(data)){
        stop("col was not found in data frame.")
      }

      # If col is a factor
      if (is.factor(data[[col]])) {
        if (!is.null(threshold)){
          stop("col is factor. 'threshold' must be NULL. Alternatively, convert factor to numeric vector.")
        }
        if (isTRUE(factor_conversion_warning)){
          warning("col is factor. Using as character.")
        }

        # Convert col to character
        data[[col]] <- as.character(data[[col]])

      }

      v <- data[[col]]


    } else {
      # If data is a factor
      if (is.factor(data)) {
        if (isTRUE(factor_conversion_warning)){
          warning("data is factor. Using as character.")
        }

        # Convert data to character
        data <- as.character(data)
      }

      # TODO I SHOULD CHECK IF DATA IS A VECTOR SOMEWHERE.

      # Check if col is specified.
      if (!is.null(col)) {
        # If it is, warn the user that it won't be used
        warning("col not used as data is not a data frame")

      }

      v <- data

    }

    # Create and return start values or indices of values that differ from the previous value
    return(
      find_different_from_previous_vec_(
        v,
        threshold = threshold,
        direction = direction,
        return_index = return_index,
        include_first = include_first
      )
    )

  }




find_different_from_previous_vec_ <-
  function(v,
           threshold = NULL,
           direction = "both",
           return_index = FALSE,
           include_first = FALSE) {
    #
    # Find values or index at which
    # value changes in vector
    # E.g. vector c(1,1,2,2,2,3,3) would return
    # values c(1,2,3) or indices c(1,3,6)
    # Uses a kind of rolling windows to determine
    # if a value is the same as the previous or new

    # Threshold can be a numerical scalar, a numerical vector of length 2, or NULL.
    # Threshold is inclusive. I.e. if threshold is 2 and the difference is 2, it's a match.
    # If threshold is NULL
    # .. TRUE if value is different than previous value.
    # .. .. Works for both numerical and character vectors.
    # If threshold is numerical scalar
    # .. Depending on direction, the difference between the value
    # .. and the previous value is checked against the threshold.
    # If threshold is a numerical vector of length 2
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


    # Adds vector to data frame
    # Creates a new column shifted (what is called???)
    # down one row.
    # Checks if the current value is the same as the previous.

    # Shift / offset v one row down
    # Insert v[1] at beginning and remove last element of v
    # to get same length as v
    v2 <- c(v[1], v[1:length(v) - 1])

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
        df <-
          data.frame(v, v2, new = !is_between_(v - v2, neg_threshold, threshold))
      } else if (direction == "positive") {
        df <- data.frame(v, v2, new = v - v2 >= threshold)
      } else if (direction == "negative") {
        df <- data.frame(v, v2, new = v - v2 <= neg_threshold)
      } else {
        stop("'direction' must be one of 'both', 'negative', and 'positive'.")
      }

    } else {
      df <- data.frame(v, v2, new = v != v2)
    }


    if (isTRUE(include_first)){
      # Set first value to TRUE
      df$new[1] = TRUE
    }

    # Get indices where v contains a new value
    new_indices <- which(df$new)

    # If return_index is TRUE
    if (isTRUE(return_index)) {
      # Return only the indices where
      # v contains a new value
      return(new_indices)

    } else {
      # Return values at the indices
      return(v[new_indices])
    }


  }
