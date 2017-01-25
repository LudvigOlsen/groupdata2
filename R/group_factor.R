
## group_factor
#' @title Create grouping factor for subsetting your data.
#' @description Divides data into groups by a range of methods.
#'  Creates and returns a grouping factor
#'  with 1s for group 1, 2s for group 2, etc.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @param data Dataframe or Vector
#' @param n Number of groups, group size, or step size
#'  (depending on chosen method)
#'  Given as whole numbers or percentage (\code{0} < n < \code{1}).
#' @param method \code{greedy, n_dist, n_fill, n_last, n_rand, or staircase}
#'
#'  \code{greedy} divides up the data greedily given a specified group size
#'  \eqn{(e.g. 10, 10, 10, 10, 10, 7)}
#'
#'  \code{n_dist} divides the data into a specified number of groups and
#'  distributes excess data points across groups
#'  \eqn{(e.g. 11, 11, 12, 11, 12)}
#'
#'  \code{n_fill} divides the data into a specified number of groups and
#'  fills up groups with excess data points from the beginning
#'  \eqn{(e.g. 12, 12, 11, 11, 11)}
#'
#'  \code{n_last} divides the data into a specified number of groups.
#'  The algorithm finds the most equal group sizes possible,
#'  using all data points. Only the last group is able to differ in size
#'  \eqn{(e.g. 11, 11, 11, 11, 13)}
#'
#'  \code{n_rand} divides the data into a specified number of groups.
#'  Excess data points are placed randomly in groups (only 1 per group)
#'  \eqn{(e.g. 12, 11, 11, 11, 12)}
#'
#'  \code{staircase} uses step_size to divide up the data.
#'  Group size increases with 1 step for every group,
#'  until there is no more data
#'  \eqn{(e.g. 5, 10, 15, 20, 7)}
#'
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
#' # Create a dataframe
#' df <- data.frame("x"=c(1:12),
#'  "species" = rep(c('cat','pig', 'human'), 4),
#'  "age" = sample(c(1:100), 12))
#'
#' # Using group_factor()
#' groups <- group_factor(df, 5, method = 'n_dist')
#' df$groups <- groups
#'
group_factor <- function(data, n, method = 'n_dist', force_equal = FALSE,
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

    } else if (method == 'staircase'){

      groups <- stair_split_group_factor_(data[[1]], n, force_equal, descending)

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

    } else if (method == 'staircase'){

      groups <- stair_split_group_factor_(data, n, force_equal, descending)

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

