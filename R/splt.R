
## splt
#' @title Split data by a range of methods.
#' @description Divides data into groups by a range of methods.
#'  Splits data by these groups.
#' @author Ludvig Renbo Olsen, \email{mail@@ludvigolsen.dk}
#' @export
#' @inheritParams group_factor
#' @return List of splitted data
#' @family grouping functions
#' @aliases split
#' @examples
#' # Create dataframe
#' df <- data.frame("x"=c(1:12),
#'  "species" = rep(c('cat','pig', 'human'), 4),
#'  "age" = sample(c(1:100), 12))
#'
#' # Using splt()
#' df_list <- splt(df, 5, method = 'n_dist')
#'
splt <- function(data, n, method = 'n_dist', force_equal = FALSE,
                 allow_zero = FALSE, descending = FALSE, randomize = FALSE){

  #
  # Takes dataframe or vector
  # Splits into the specified windows
  # Returns list with the windows (dataframes or vectors)
  #

  # If allow_zero is TRUE, and n is 0
  # .. Return the given data in a list
  # .. instead of giving an error
  if (isTRUE(allow_zero) && n == 0){

    return(split(data, factor(1)))

  }

  # Check arguments
  # Convert n if given as percentage
  # Check more arguments
  n <- check_convert_check_(data, n, method, force_equal, allow_zero, descending)


  # Force equal
  # .. Some methods have a different way of calculating
  # .. "equality". They will do this themselves,
  # .. the others can be forced equal here.

  if(isTRUE(force_equal)){

    if(!(method %in% c('staircase'))){

      # If force_equal is set to TRUE,
      # and we don't already have equally sized windows,
      # remove elements/rows from data, until we get
      # largest possible equally sized windows
      if(is.data.frame(data)){

        if (!(is_wholenumber_(nrow(data)/n))){

          # Multiply window size and number of windows to find
          # how much data to keep
          to_keep <- floor(nrow(data)/n)*n

          # Keep the first to_keep elements/rows
          data <- head(data, to_keep)

        }

      } else {

        if (!(is_wholenumber_(length(data)/n))){

          # Multiply window size and number of windows to find
          # how much data to keep
          to_keep <- floor(length(data)/n)*n

          # Keep the first to_keep elements/rows
          data <- head(data, to_keep)

        }

      }


    }
  }


  # Split the data
  # .. Checks if data is dataframe or vector
  # .. Calls the right splitter

  if (is.data.frame(data)){

    return(dsplit_(data, n, method, force_equal, allow_zero, descending, randomize))

  } else {

    return(vsplit_(data, n, method, force_equal, allow_zero, descending, randomize))

  }

}
