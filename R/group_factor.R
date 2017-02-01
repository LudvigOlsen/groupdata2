
## group_factor
#' @title Create grouping factor for subsetting your data.
#' @description Divides data into groups by a range of methods.
#'  Creates and returns a grouping factor
#'  with 1s for group 1, 2s for group 2, etc.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @param data Dataframe or Vector.
#' @param n Number of groups, group size, step size or number to start at
#'  (depending on chosen method)
#'  Given as whole numbers or percentage (\code{0} < n < \code{1}).
#' @param method \code{greedy, n_dist, n_fill, n_last, n_rand, or staircase}.
#'
#'  Notice: examples are sizes of the generated groups
#'  based on a vector with 57 elements.
#'
#'  \code{greedy} divides up the data greedily given a specified group size
#'  \eqn{(e.g. 10, 10, 10, 10, 10, 7)}.
#'
#'  \code{n_dist} divides the data into a specified number of groups and
#'  distributes excess data points across groups
#'  \eqn{(e.g. 11, 11, 12, 11, 12)}.
#'
#'  \code{n_fill} divides the data into a specified number of groups and
#'  fills up groups with excess data points from the beginning
#'  \eqn{(e.g. 12, 12, 11, 11, 11)}.
#'
#'  \code{n_last} divides the data into a specified number of groups.
#'  The algorithm finds the most equal group sizes possible,
#'  using all data points. Only the last group is able to differ in size
#'  \eqn{(e.g. 11, 11, 11, 11, 13)}.
#'
#'  \code{n_rand} divides the data into a specified number of groups.
#'  Excess data points are placed randomly in groups (only 1 per group)
#'  \eqn{(e.g. 12, 11, 11, 11, 12)}.
#'
#'  \code{l_sizes} divides the data by a list of group sizes.
#'  Excess data points are placed in extra group at the end.
#'  \eqn{(e.g. n = c(0.2,0.3) outputs groups with sizes (11,17,29))}.
#'
#'  \code{l_starts} starts new groups at specified values of vector.
#'  \code{n} is a list of starting positions.
#'  Skip values by c(value, skip_to_number) where skip_to_number is the nth appearance of the value
#'  in the vector.
#'  Lists automatically starts from first data point.
#'  \eqn{(e.g. n = list(1,3,7,25,50) outputs groups with sizes (2,4,18,25,8))}.
#'  To skip:\eqn{(given vector c(1,2,3,1,2,3), n = list(1,2,c(3,2))
#'  outputs groups with sizes (1,4,1))}.
#'
#'  \code{staircase} uses step_size to divide up the data.
#'  Group size increases with 1 step for every group,
#'  until there is no more data
#'  \eqn{(e.g. 5, 10, 15, 20, 7)}.
#'
#'  \code{primes} uses prime numbers as group sizes.
#'  Group size increases to the next prime number
#'  until there is no more data.
#'  \code{n} is the prime number to start at.
#'  \eqn{(e.g. 5, 7, 11, 13, 17, 4)}.
#'
#' @param col Name of column with values to match in method \code{l_starts}
#' when data is a dataframe. Pass 'index' to use rownames. (Character)
#' @param force_equal Create equal groups by discarding excess data points.
#'  Implementation varies between methods. (Logical)
#' @param allow_zero Whether n can be passed as \code{0}. (Logical)
#' @param descending Change direction of method. (Not fully implemented)
#'  (Logical)
#' @param randomize Randomize the grouping factor (Logical)
#' @return Grouping factor with 1s for group 1, 2s for group 2, etc.
#' @family grouping functions
#' @family staircase tools
#' @examples
#' # Attach packages
#' library(groupdata2)
#' library(dplyr)
#'
#' # Create a dataframe
#' df <- data.frame("x"=c(1:12),
#'  "species" = rep(c('cat','pig', 'human'), 4),
#'  "age" = sample(c(1:100), 12))
#'
#' # Using group_factor() with n_dist
#' groups <- group_factor(df, 5, method = 'n_dist')
#' df$groups <- groups
#'
#' # Using group_factor() with greedy
#' groups <- group_factor(df, 5, method = 'greedy')
#' df$groups <- groups
#'
#' # Using group_factor() with l_sizes
#' groups <- group_factor(df, c(0.2), method = 'l_sizes')
#' df$groups <- groups
#'
#'# Using group_factor() with l_starts
#' groups <- group_factor(df, list('cat', c('pig',2), 'human'), method = 'l_starts', col = 'species')
#' df$groups <- groups
#'
group_factor <- function(data, n, method = 'n_dist', col = NULL, force_equal = FALSE,
                         allow_zero = FALSE, descending = FALSE,
                         randomize = FALSE){

  #
  # Takes dataframe or vector
  # Returns a grouping factor
  #


  ### Allow zero ###

  # If allow_zero is TRUE and n is 0
  # return NAs instead of giving an error
  if (isTRUE(allow_zero) && n == 0){

    if(is.data.frame(data)){

      return(rep(NA, each = nrow(data)))

    } else {

      return(rep(NA, each = length(data)))

    }


  }

  # Check arguments
  # Convert n if given as percentage
  # Check more arguments
  n <- check_convert_check_(data, n, method, force_equal, allow_zero, descending)



  # For method is l_starts
  # If data is a dataframe and col is not NULL
  # We want to get the column with values to match

  if(is.data.frame(data) && !is.null(col)){

    # If col is 'index', create column with rownames for matching values
    if (col == 'index'){

      starts_col <- rownames(data)

    # If col is not NULL (and not 'index')
    # Get the column from data
    } else {

      starts_col <- data[[col]]

    }

  }

  # Create grouping factors
  # .. Check if data is a dataframe or a vector
  # .. Call grouping factor function for specified method

  if(is.data.frame(data)){

    if(method == 'greedy'){

      groups <- greedy_group_factor_(data[[1]], n, force_equal, descending)

    } else if (method == 'n_dist'){

      groups <- n_dist_group_factor_(data[[1]], n, force_equal, descending)

    } else if (method == 'n_last'){

      groups <- n_last_group_factor_(data[[1]], n, force_equal, descending)

    } else if (method == 'n_fill'){

      groups <- n_fill_group_factor_(data[[1]], n, force_equal, descending)

    } else if (method == 'n_rand'){

      groups <- n_rand_group_factor_(data[[1]], n, force_equal, descending)

    } else if (method == 'l_sizes'){

      groups <- l_sizes_group_factor_(data[[1]], n, force_equal, descending)

    } else if (method == 'l_starts'){

      # Notice that we pass the starts_col as data
      groups <- l_starts_group_factor_(starts_col, n, force_equal, descending)

    }else if (method == 'staircase'){

      groups <- stair_split_group_factor_(data[[1]], n, force_equal, descending)

    } else if (method == 'primes'){

      groups <- primes_split_group_factor_(data[[1]], n, force_equal, descending)

    }

  } else {

    if(method == 'greedy'){

      groups <- greedy_group_factor_(data, n, force_equal, descending)

    } else if (method == 'n_dist'){

      groups <- n_dist_group_factor_(data, n, force_equal, descending)

    } else if (method == 'n_last'){

      groups <- n_last_group_factor_(data, n, force_equal, descending)

    } else if (method == 'n_fill'){

      groups <- n_fill_group_factor_(data, n, force_equal, descending)

    } else if (method == 'n_rand'){

      groups <- n_rand_group_factor_(data, n, force_equal, descending)

    } else if (method == 'l_sizes'){

      groups <- l_sizes_group_factor_(data, n, force_equal, descending)

    } else if (method == 'l_starts'){

      groups <- l_starts_group_factor_(data, n, force_equal, descending)

    } else if (method == 'staircase'){

      groups <- stair_split_group_factor_(data, n, force_equal, descending)

    } else if (method == 'primes'){

      groups <- primes_split_group_factor_(data, start_at=n, force_equal, descending)

    }

  }

  # If randomize is set to TRUE
  # .. reorganize the grouping factor randomly

  if (isTRUE(randomize)){

    groups <- sample(groups)

  }

  # Return grouping factor
  return(groups)

}

