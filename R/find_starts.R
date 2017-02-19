## find_starts
#' @title Find start positions of groups in data.
#' @description Find values or indices of values that are not the same
#' as the previous value. E.g. use with the \code{l_starts} method.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @param data Dataframe or Vector
#' @param return_index Return indices of starts. (Logical)
#' @param col Name of column to find starts in. Used when data is
#'  dataframe. (Character)
#' @return Vector with either start values or indices of start values.
#' @family grouping functions
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create a dataframe
#' df <- data.frame('a' = c('a','a','b','b','c','c'))
#'
#' # Get start values for new groups in column 'a'
#' find_starts(df, col = 'a')
#'
#' # Get indices of start values for new groups in column 'a'
#' find_starts(df, col = 'a', return_index = TRUE)
#'
#' ## Use found starts with l_starts method
#'
#' # Get start values for new groups in column 'a'
#' starts <- find_starts(df, col = 'a')
#'
#' # Use starts in group() with 'l_starts' method
#' group(df, starts, method = 'l_starts', starts_col = 'a')
#'
#' # Similar but with indices instead of values
#'
#' # Get indices of start values for new groups in column 'a'
#' starts_ind <- find_starts(df, col = 'a', return_index = TRUE)
#'
#' # Use starts in group() with 'l_starts' method
#' group(df, starts_ind, method = 'l_starts', starts_col = 'index')
#'
find_starts <- function(data, col = NULL, return_index = FALSE){

  #
  # Run find_starts_vec_ for either a vector or dataframe
  #

  # If data is a dataframe
  if(is.data.frame(data)){

    # Check if col is specified
    if (is.null(col)){

      # If not, raise error
      stop("col must be specified when data is dataframe")

    }

    # If col is a factor
    if (is.factor(data[[col]])){

      warning("col is factor. Using as character.")

      # Convert col to character
      data[[col]] <- as.character(data[[col]])

      }

    # Create and return start values or indices of start values
    return(find_starts_vec_(data[[col]], return_index = return_index))

  } else {

    # If data is a factor
    if (is.factor(data)){

      warning("data is factor. Using as character.")

      # Convert data to character
      data <- as.character(data)
    }

    # I SHOULD CHECK IF DATA IS A VECTOR SOMEWHERE.

    # Check if col is specified.
    if (!is.null(col)){

      # If it is, warn the user that it won't be used
      warning("col not used as data is not a dataframe")

    }

    # Create and return start values or indices of start values
    return(find_starts_vec_(data, return_index = return_index))

  }


}


find_starts_vec_ <- function(v, return_index = FALSE){

  #
  # Find values or index at which
  # value changes in vector
  # E.g. vector c(1,1,2,2,2,3,3) would return
  # values c(1,2,3) or indices c(1,3,6)
  # Uses a kind of rolling windows to determine
  # if a value is the same as the previous or new

  # Adds vector to dataframe
  # Creates a new column shifted (what is called???)
  # down one row.
  # Checks if the current value is the same as the previous.

  # Shift / offset v one row down
  # Insert NA at beginning and remove last element of v
  # to get same length as v
  v2 <- c(NA, v[1:length(v)-1])

  # Create dataframe with v, v2 and
  # a logical column stating whether
  # v is new or not.
  df <- data.frame(v, v2, new = v != v2)

  # The first value must always be TRUE
  df$new[1] = TRUE

  # Get indices where v contains a new value
  new_indices <- which(df$new)

  # If return_index is TRUE
  if (isTRUE(return_index)){

    # Return only the indices where
    # v contains a new value
    return(new_indices)

  } else {

    # Return values at the indices
    return(v[new_indices])
  }


}
