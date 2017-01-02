## fold
#' @title Create balanced folds for cross-validation.
#' @description Divides up data into groups by a range of methods.
#'  Balances a given categorical variable between folds and keeps (if possible)
#'  all data points with the same ID (e.g. participant_id) in the same fold.
#' @details
#' @author Ludvig Renbo Olsen \{mail@@ludvigolsen.dk}
#' @export
#' @param k Number of folds
#' @param cat_col Categorical variable to balance between folds.
#'
#'  E.g. when predicting a binary variable (a or b), it is necessary to have
#'  both represented in every fold
#'
#'  N.B. If also passing an id_col, cat_col must be a constant for that ID.
#' @param id_col Factor with IDs.
#'  This will be used to keep all rows with an ID in the same fold
#'  (if possible).
#'
#'  E.g. If we have measured a participant multiple times and want to see the
#'  effect of time, we want to have all observations of this participant in
#'  the same fold.
#' @inheritParams group_factor
#' @return Dataframe with grouping factor for subsetting in cross-validation.
#' @family grouping functions
#' @examples
#' # Coming soon
#' @importFrom dplyr group_by_ do %>%
fold <- function(data, k=5, cat_col = NULL, id_col = NULL, method = 'n_dist'){

  #
  # Takes:
  # .. dataframe
  # .. number of folds
  # .. a categorical variable to balance in folds
  # .... e.g. to predict between 2 diagnoses,
  # ..... you need both of them in the fold
  # .. an id variable for keeping a subject in the same fold
  # .. method for creating grouping factor
  #
  # Returns:
  # .. dataframe with grouping factor (folds)
  #

  # If cat_col is not NULL
  if (!is.null(cat_col)){

    print('cat_col not null')

    # If id_col is not NULL
    if (!is.null(id_col)){

      print('id_col not null')

      print(cat_col)

      # Group by cat_col
      # For each group:
      # .. create groups of the unique IDs (e.g. subjects)
      # .. add grouping factor to data
      # Group by new grouping factor '.groups'

      data <- data %>%
        group_by_(cat_col) %>%
        do(group_uniques_(., k, id_col, method)) %>%
        group_by_('.groups')


      # If id_col is NULL
    } else {

      print('id_col null')

      # Group by cat_col
      # Create groups from data
      # .. and add grouping factor to data

      data <- data %>%
        group_by_(cat_col) %>%
        do(group(., k, method = method, randomize = TRUE))


    }


    # If cat_col is NULL
  } else {

    print('cat_col null')

    # If id_col is not NULL
    if (!is.null(id_col)){

      print('id_col not null')

      # Create groups of unique IDs
      # .. and add grouping factor to data

      data <- data %>%
        group_uniques_(k, id_col, method)


      # If id_col is NULL
    } else {

      print('id_col null')

      # Create groups from all the data points
      # .. and add grouping factor to data

      data <- group(data, k, method = method, randomize = TRUE)

    }

  }


  # Return data
  return(data)


}

