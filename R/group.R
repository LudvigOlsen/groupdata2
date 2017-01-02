
## group
#' @title Create groups / windows from your data.
#' @description Divides up data into groups / windows by a range of methods.
#'  Creates a grouping factor with 1s for group 1, 2s for group 2, etc.
#'  Groups data by the grouping factor for easy use in dplyr pipelines.
#' @details
#' @author Ludvig Renbo Olsen \{mail@@ludvigolsen.dk}
#' @export
#' @inheritParams group_factor
#' @param return_factor Return only grouping factor (Logical)
#' @return Dataframe grouped by new grouping factor
#' @family grouping functions
#' @aliases window split group_by
#' @examples
#' # Coming soon
group <- function(data, n, method = 'n_dist', force_equal = FALSE,
                  allow_zero = FALSE, return_factor = FALSE,
                  descending = FALSE, randomize = FALSE){

  #
  # Takes dataframe or vector
  # Creates a grouping factor
  # If data is a vector
  # .. Return dataframe with vector and grouping factor
  # .. grouped by grouping factor
  # If data is a dataframe:
  # .. Return dataframe grouped by grouping factor
  #

  # Create grouping factor
  grouping_factor <- group_factor(data, n, method, force_equal = force_equal,
                                  allow_zero = allow_zero, descending = descending,
                                  randomize = randomize)

  # If return_factor is set to TRUE
  # .. return the created grouping factor
  if (isTRUE(return_factor)){

    return(grouping_factor)

  }


  # If data is a dataframe
  # .. Check if force_equal is TRUE
  # .... if so, shorten data to the length of the
  # .... grouping factor
  # .. Add grouping factor to data
  # .. Group by grouping factor and return data
  # If data is a vector
  # .. Check if force_equal is TRUE
  # .... if so, shorten data to the length of the
  # .... grouping factor
  # .. Create a dataframe
  # .... with data and the grouping factor
  # .. Group by grouping factor and return data

  # If data is dataframe
  if(is.data.frame(data)){

    # If force_equal is TRUE
    if(isTRUE(force_equal)){

      # Shorten data to the length of the grouping factor
      data <- head(data, length(grouping_factor))

    }

    # Add the grouping factor to data
    data$.groups <- grouping_factor

    # Return data grouped by the grouping factor
    return(dplyr::group_by(data, .groups))

  } else { # If data is vector

    # If force_equal is TRUE
    if(isTRUE(force_equal)){

      # Shorten data to the length of the grouping factor
      data <- head(data, length(grouping_factor))

    }

    # Create dataframe with data and the grouping factor
    data <- data.frame(data, ".groups" = grouping_factor)

    # Return data grouped by the grouping factor
    return(dplyr::group_by(data, .groups))

  }

}

