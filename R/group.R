
## group
#' @title Create groups from your data.
#' @description Divides data into groups by a range of methods.
#'  Creates a grouping factor with 1s for group 1, 2s for group 2, etc.
#'  Returns a dataframe grouped by the grouping factor for easy use in dplyr pipelines.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @inheritParams group_factor
#' @param return_factor Return only grouping factor (Logical)
#' @param col_name Name of added grouping factor
#' @return Dataframe grouped by new grouping factor
#' @family grouping functions
#' @family staircase tools
#' @family l_starts tools
#' @aliases window binning split
#' @examples
#' # Attach packages
#' library(groupdata2)
#' library(dplyr)
#'
#' # Create dataframe
#' df <- data.frame("x"=c(1:12),
#'  "species" = rep(c('cat','pig', 'human'), 4),
#'  "age" = sample(c(1:100), 12))
#'
#' # Using group()
#' df_grouped <- group(df, 5, method = 'n_dist')
#'
#' # Using group() with dplyr pipeline to get mean age
#' df_means <- df %>%
#'  group(5, method = 'n_dist') %>%
#'  dplyr::summarise(mean_age = mean(age))
#'
#' # Using group_factor() with l_starts
#' # "c('pig',2)" skips to the second appearance of
#' # "pig" after the first appearance of "cat"
#' df_grouped <- group(df,
#'                     list('cat', c('pig',2), 'human'),
#'                     method = 'l_starts',
#'                     starts_col = 'species')
#'
group <- function(data, n, method = 'n_dist', starts_col = NULL,
                  force_equal = FALSE, allow_zero = FALSE,
                  return_factor = FALSE, descending = FALSE,
                  randomize = FALSE, col_name = '.groups',
                  remove_missing_starts = FALSE){

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
  grouping_factor <- group_factor(data, n, method, starts_col,
                                  force_equal = force_equal,
                                  allow_zero = allow_zero,
                                  descending = descending,
                                  randomize = randomize,
                                  remove_missing_starts = remove_missing_starts)

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

  # Create local tmp variable name
  local_tmp_var <- create_tmp_var(data, ".TempGroupsName")

  # If data is dataframe
  if(is.data.frame(data)){

    # If force_equal is TRUE
    if(isTRUE(force_equal)){

      # Shorten data to the length of the grouping factor
      data <- head(data, length(grouping_factor))

    }

    # Add the grouping factor to data
    #data$.groups <- grouping_factor
    data[[local_tmp_var]] <- grouping_factor

    # Replace temporary column name with passed column name
    # e.g. '.groups'
    data <- replace_col_name(data, local_tmp_var, col_name)

    # Return data grouped by the grouping factor
    return(dplyr::group_by(data, !! as.name(col_name)))

  } else { # If data is vector

    # If force_equal is TRUE
    if(isTRUE(force_equal)){

      # Shorten data to the length of the grouping factor
      data <- head(data, length(grouping_factor))

    }

    # Create dataframe with data and the grouping factor
    data <- data.frame(data)
    data[[local_tmp_var]] <- grouping_factor

    # Replace temporary column name with passed column name
    # e.g. '.groups'
    data <- replace_col_name(data, local_tmp_var, col_name)

    # Return data grouped by the grouping factor
    return(dplyr::group_by(data, !! as.name(col_name)))

  }

}

