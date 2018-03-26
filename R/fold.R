# R CMD check NOTE handling
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

## fold
#' @title Create balanced folds for cross-validation.
#' @description Divides data into groups by a range of methods.
#'  Balances a given categorical variable and/or numerical variable between folds and keeps (if possible)
#'  all data points with a shared ID (e.g. participant_id) in the same fold.
#' @details
#'  \code{cat_col}: data is first subset by \code{cat_col}.
#'  Subsets are folded/grouped and merged.
#'
#'  \code{id_col}: folds are created from unique IDs.
#'
#'  \code{cat_col} AND \code{id_col}: data is subset by \code{cat_col}
#'  and folds are created from unique IDs in each subset.
#'  Subsets are merged.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @param k \emph{Dependent on method.}
#'
#'  Number of folds (default), fold size, with more (see \code{method}).
#'
#'  Given as whole number(s) and/or percentage(s) (0 < n < 1).
#' @param cat_col Name of categorical variable to balance between folds.
#'
#'  E.g. when predicting a binary variable (a or b), it is necessary to have
#'  both represented in every fold.
#'
#'  N.B. If also passing an \code{id_col}, \code{cat_col} should be constant within each ID.
#' @param num_col Name of numerical variable to balance between folds.
#'
#'  N.B. When used with \code{id_col}, values for each ID are aggregated using \code{id_aggregation_fn} before being balanced.
#'
#'  N.B. When passing \code{num_col}, the \code{method} parameter is not used.
#' @param id_col Name of factor with IDs.
#'  This will be used to keep all rows that share an ID in the same fold
#'  (if possible).
#'
#'  E.g. If we have measured a participant multiple times and want to see the
#'  effect of time, we want to have all observations of this participant in
#'  the same fold.
#' @param id_aggregation_fn Function for aggregating values in \code{num_col} for each ID, before balancing \code{num_col}.
#'
#'  N.B. Only used when \code{num_col} and \code{id_col} are both specified.
#' @inheritParams group_factor
#' @aliases create_balanced_groups
#' @return Dataframe with grouping factor for subsetting in cross-validation.
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
#' df <- df %>% arrange(participant)
#' df$session <- rep(c('1','2', '3'), 6)
#'
#' # Using fold()
#'
#' ## Without balancing
#' df_folded <- fold(df, 3, method = 'n_dist')
#'
#' ## With cat_col
#' df_folded <- fold(df, 3, cat_col = 'diagnosis',
#'  method = 'n_dist')
#'
#' ## With id_col
#' df_folded <- fold(df, 3, id_col = 'participant',
#'  method = 'n_dist')
#'
#' ## With num_col
#' # Note: 'method' would not be used in this case
#' df_folded <- fold(df, 3, num_col = 'score')
#'
#' # With cat_col and id_col
#' df_folded <- fold(df, 3, cat_col = 'diagnosis',
#'  id_col = 'participant', method = 'n_dist')
#'
#' ## With cat_col, id_col and num_col
#' df_folded <- fold(df, 3, cat_col = 'diagnosis',
#'  id_col = 'participant', num_col = 'score')
#'
#' # Order by folds
#' df_folded <- df_folded %>% arrange(.folds)
#'
#' @importFrom dplyr group_by_ do %>%
fold <- function(data, k=5, cat_col = NULL, num_col = NULL,
                 id_col = NULL, starts_col = NULL,
                 method = 'n_dist', id_aggregation_fn=sum,
                 remove_missing_starts = FALSE){

  #
  # Takes:
  # .. dataframe
  # .. number of folds
  # .. a categorical variable to balance in folds
  # .... e.g. to predict between 2 diagnoses,
  # ..... you need both of them in the fold
  # .. an id variable for keeping a subject in the same fold
  # .. a value variable to balance in folds
  # .. method for creating grouping factor
  #
  # Returns:
  # .. dataframe with grouping factor (folds)
  #

  # Convert k to wholenumber if given as percentage
  if(!arg_is_wholenumber_(k) && is_between_(k,0,1)){

    k = convert_percentage_(k, data)

  }

  # Stop if k is not a wholenumber
  stopifnot(arg_is_wholenumber_(k))

  # If num_col is specified, warn that method is ignored
  if (!is.null(num_col) & method != "n_dist"){
    message("Method is ignored when num_col is not NULL. This message occurs, because method is not default value.")
  }

  # If method is either greedy or staircase and cat_col is not NULL
  # we don't want k elements per level in cat_col
  # so we divide k by the number of levels in cat_col
  if(method %in% c('greedy', 'staircase') && !is.null(cat_col)){

    n_levels_cat_col = length(unique(data[[cat_col]]))
    k = ceiling(k/n_levels_cat_col)

  }

  # If cat_col is not NULL
  if (!is.null(cat_col)){

    # If id_col is not NULL
    if (!is.null(id_col)){

      # If num_col is not NULL
      if (!is.null(num_col)){

        # Aggregate num_col for IDs with the passed id_aggregation_fn
        # For each category in cat_col
        # .. create value balanced group factor based on aggregated values
        # Join the groups back into the data

        # aggregate val col per ID
        ids_aggregated <- data %>%
          group_by(!! as.name(cat_col), !! as.name(id_col)) %>%
          dplyr::summarize(aggr_val = id_aggregation_fn(!!as.name(num_col))) %>%
          dplyr::ungroup()

        # Find groups for each category
        ids_grouped <- plyr::ldply(unique(ids_aggregated[[cat_col]]), function(category){
          ids_for_cat <- ids_aggregated %>%
            dplyr::filter(!!as.name(cat_col) == category)
          ids_for_cat$.folds <- value_balanced_group_factor_(
            ids_for_cat, n=k, num_col = "aggr_val")
          ids_for_cat %>%
            dplyr::select(-c(aggr_val))
        })

        # Transfer groups to data
        data <- data %>%
          dplyr::inner_join(ids_grouped, by=c(cat_col, id_col))

      # If num_col is NULL
      } else {

        # Group by cat_col
        # For each group:
        # .. create groups of the unique IDs (e.g. subjects)
        # .. add grouping factor to data
        # Group by new grouping factor '.folds'

        data <- data %>%
          group_by(!! as.name(cat_col)) %>%
          do(group_uniques_(., k, id_col, method,
                            col_name = '.folds',
                            starts_col = starts_col,
                            remove_missing_starts = remove_missing_starts)) %>%
          group_by(!! as.name('.folds'))
      }

    # If id_col is NULL
    } else {

      # If num_col is not NULL
      if (!is.null(num_col)){

        # For each category in cat_col
        # .. create value balanced group factor

        # Find groups for each category
        data <- plyr::ldply(unique(data[[cat_col]]), function(category){
          data_for_cat <- data %>%
            dplyr::filter(!!as.name(cat_col) == category)
          data_for_cat$.folds <- value_balanced_group_factor_(
            data_for_cat, n=k, num_col = num_col)
          data_for_cat
        })

      # If num_col is NULL
      } else {

        # Group by cat_col
        # Create groups from data
        # .. and add grouping factor to data

        data <- data %>%
          group_by(!! as.name(cat_col)) %>%
          do(group(., k, method = method,
                   randomize = TRUE,
                   col_name = '.folds',
                   starts_col = starts_col,
                   remove_missing_starts = remove_missing_starts))
      }
    }

  # If cat_col is NULL
  } else {

    # If id_col is not NULL
    if (!is.null(id_col)){

      # If num_col is not NULL
      if (!is.null(num_col)){

        # Aggregate num_col for IDs with the passed id_aggregation_fn
        # Create value balanced group factor based on aggregated values
        # Join the groups back into the data

        # aggregate val col per ID
        ids_aggregated <- data %>%
          group_by(!! as.name(id_col)) %>%
          dplyr::summarize(aggr_val = id_aggregation_fn(!!as.name(num_col))) %>%
          dplyr::ungroup()

        # Create group factor
        vb_factor <- value_balanced_group_factor_(ids_aggregated, n=k, num_col = "aggr_val")
        ids_aggregated$.folds <- vb_factor
        ids_aggregated$aggr_val <- NULL

        # Transfer groups to data
        data <- data %>%
          dplyr::inner_join(ids_aggregated, by=c(id_col))

      # If num_col is NULL
      } else {

        # Create groups of unique IDs
        # .. and add grouping factor to data

        data <- data %>%
          group_uniques_(k, id_col, method,
                         col_name = '.folds',
                         starts_col = starts_col,
                         remove_missing_starts = remove_missing_starts)
      }

    # If id_col is NULL
    } else {

      # If num_col is not NULL
      if (!is.null(num_col)){

        # Add group factor
        data$.folds <- value_balanced_group_factor_(data, n=k, num_col = num_col)

      # If num_col is NULL
      } else {

        # Create groups from all the data points
        # .. and add grouping factor to data

        data <- group(data, k,
                      method = method,
                      randomize = TRUE,
                      col_name = '.folds',
                      starts_col = starts_col,
                      remove_missing_starts = remove_missing_starts)
      }
    }
  }

  # Return data
  return(data)


}

