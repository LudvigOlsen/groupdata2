
# R CMD check NOTE handling
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

## partition
#' @title Create balanced partitions.
#' @description Splits data into partitions.
#'  Balances a given categorical variable and/or numerical variable between partitions and keeps (if possible)
#'  all data points with a shared ID (e.g. participant_id) in the same partition.
#' @details
#'  \code{cat_col}: data is first subset by \code{cat_col}.
#'  Subsets are grouped and merged.
#'
#'  \code{id_col}: groups are created from unique IDs.
#'
#'  \code{cat_col} AND \code{id_col}: data is subset by \code{cat_col}
#'  and groups are created from unique IDs in each subset.
#'  Subsets are merged.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @inheritParams group_factor
#' @param p List / vector of partition sizes.
#'  Given as whole number(s) and/or percentage(s) (\code{0} < \code{n} < \code{1}).
#'  E.g. \eqn{c(0.2, 3, 0.1)}.
#' @param cat_col Name of categorical variable to balance between partitions.
#'
#'  E.g. when training/testing a model for predicting a binary variable (a or b),
#'  it is necessary to have both represented in both the training set and the test set.
#'
#'  N.B. If also passing an id_col, cat_col should be constant within each ID.
#' @param num_col Name of numerical variable to balance between partitions.
#'
#'  N.B. When used with \code{id_col}, values for each ID are aggregated using \code{id_aggregation_fn} before being balanced.
#' @param id_col Name of factor with IDs. Used to keep all rows that share an ID in
#'  the same partition (if possible).
#'
#'  E.g. If we have measured a participant multiple times and want to see the
#'  effect of time, we want to have all observations of this participant in
#'  the same partition.
#' @param id_aggregation_fn Function for aggregating values in \code{num_col} for each ID, before balancing \code{num_col}.
#'
#'  N.B. Only used when \code{num_col} and \code{id_col} are both specified.
#' @param list_out Return partitions in a list. (Logical)
#' @param force_equal Discard excess data. (Logical)
#' @return If \code{list_out is TRUE}:
#'
#' A list of partitions where partitions are dataframes.
#'
#' If \code{list_out is FALSE}:
#'
#' A dataframe with grouping factor for subsetting.
#' @examples
#' # Attach packages
#' library(groupdata2)
#' library(dplyr)
#'
#' # Create dataframe
#' df <- data.frame(
#'  "participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
#'  "age" = rep(sample(c(1:100), 6), 3),
#'  "diagnosis" = rep(c('a', 'b', 'a', 'a', 'b', 'b'), 3),
#'  "score" = sample(c(1:100), 3*6))
#' df <- df[order(df$participant),]
#' df$session <- rep(c('1','2', '3'), 6)
#'
#' # Using partition()
#' # Without cat_col and id_col
#' partitions <- partition(df, c(0.2,0.3))
#'
#' # With cat_col
#' partitions <- partition(df, c(0.5), cat_col = 'diagnosis')
#'
#' # With id_col
#' partitions <- partition(df, c(0.5), id_col = 'participant')
#'
#' # With cat_col and id_col
#' partitions <- partition(df, c(0.5), cat_col = 'diagnosis',
#'                         id_col = 'participant')
#'
#' # Return dataframe with grouping factor
#' # with list_out = FALSE
#' partitions <- partition(df, c(0.5), list_out = FALSE)
#'
#' @importFrom dplyr group_by do %>%
partition <- function(data, p = 0.2, cat_col = NULL,
                      num_col = NULL, id_col = NULL,
                      id_aggregation_fn=sum,
                      force_equal = FALSE,
                      list_out = TRUE) {

  #
  # Balanced partitioning
  # data: dataframe or vector
  # p: list of partitions given as percentage (0-1) or group sizes (wholenumber)
  # cat_col: Categorical variable to balance by
  # num_col: Numerical variable to balance by
  # id_col: ID column to keep rows with shared IDs in the same partition
  # force_equal: Whether you only want the inputted partitions or the exceeding values gets a partition (logical)
  #        FALSE allows you to pass "p = 0.2" and get 2 partions - 0.2 and 0.8
  #



  # If num_col is not NULL
  if (!is.null(num_col)){
    data <- create_num_col_groups(data, n=p, num_col=num_col, cat_col=cat_col,
                                  id_col=id_col, col_name=".partitions",
                                  id_aggregation_fn = id_aggregation_fn,
                                  method="l_sizes", force_equal=force_equal)

  } else {

    # If cat_col is not NULL
    if (!is.null(cat_col)){

      # If id_col is not NULL
      if (!is.null(id_col)){

        # Group by cat_col
        # For each group:
        # .. create groups of the unique IDs (e.g. subjects)
        # .. add grouping factor to data
        # Group by new grouping factor '.partitions'

        data <- data %>%
          group_by(!! as.name(cat_col)) %>%
          do(group_uniques_(., n = p, id_col, method = 'l_sizes',
                            col_name = '.partitions',
                            force_equal = force_equal)) %>%
          group_by(!! as.name('.partitions'))

      # If id_col is NULL
      } else {

        # Group by cat_col
        # Create groups from data
        # .. and add grouping factor to data

        data <- data %>%
          group_by(!! as.name(cat_col)) %>%
          do(group(., n = p, method = 'l_sizes',
                   randomize = TRUE,
                   col_name = '.partitions',
                   force_equal = force_equal))
      }

    # If cat_col is NULL
    } else {

      # If id_col is not NULL
      if (!is.null(id_col)){

        # Create groups of unique IDs
        # .. and add grouping factor to data

        data <- data %>%
          group_uniques_(n = p, id_col, method = 'l_sizes',
                         col_name = '.partitions',
                         force_equal = force_equal)

      # If id_col is NULL
      } else {


        # Create groups from all the data points
        # .. and add grouping factor to data

        data <- group(data, n = p,
                      method = 'l_sizes',
                      randomize = TRUE,
                      col_name = '.partitions',
                      force_equal = force_equal)
      }
    }
  }

  if (isTRUE(list_out)){

    # If we have any NAs in .partitions
    if (anyNA(data$.partitions)){

      stop("NA in .partitions column.")

    } else {

      plyr::llply(c(1:max(as.integer(data[['.partitions']]))), function(part){

        temp_data <- data[data$.partitions == part,]

        temp_data$.partitions <- NULL

        temp_data %>% dplyr::ungroup() %>% return()

      }) %>% return()

    }

  } else {

    # Return data
    return(data)

  }

}

