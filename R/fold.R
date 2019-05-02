# R CMD check NOTE handling
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

## fold
#' @title Create balanced folds for cross-validation.
#' @description Divides data into groups by a range of methods.
#'  Balances a given categorical variable and/or numerical variable between folds and keeps (if possible)
#'  all data points with a shared ID (e.g. participant_id) in the same fold.
#' @details
#'  \subsection{cat_col}{
#'    \enumerate{
#'      \item Data is subset by \code{cat_col}.
#'      \item Subsets are grouped and merged.
#'    }
#'  }
#'
#'  \subsection{id_col}{
#'    \enumerate{
#'      \item Groups are created from unique IDs.
#'    }
#'  }
#'
#'  \subsection{num_col}{
#'    \enumerate{
#'      \item Rows are shuffled.
#'
#'      \strong{Note} that this will only have an effect on rows that have the same value in num_col.
#'      \item Rows are ordered as smallest, largest, second smallest, second largest, ...
#'      \item By their pairwise sum, these are once again ordered as smallest, largest, second smallest, second largest, ...
#'      \item The ordered data is grouped using method "n_fill".
#'    }
#'
#'  N.B. In case \code{data} has an unequal number of rows,
#'  the row with the smallest value becomes the first group by itself in (1) and (2), and the order is instead:
#'  smallest, second smallest, largest, third smallest, second largest, ...
#'  This row will end up in the first fold.
#'  }
#'
#'  \subsection{cat_col AND id_col}{
#'    \enumerate{
#'      \item Data is subset by \code{cat_col}.
#'      \item Groups are created from unique IDs in each subset.
#'      \item Subsets are merged.
#'    }
#'  }
#'
#'  \subsection{cat_col AND num_col}{
#'    \enumerate{
#'      \item Data is subset by \code{cat_col}.
#'      \item Subsets are grouped by \code{num_col}.
#'      \item Subsets are merged.
#'    }
#'  }
#'
#'  \subsection{num_col AND id_col}{
#'    \enumerate{
#'      \item Values in \code{num_col} are aggregated for each ID, using \code{id_aggregation_fn}.
#'      \item The IDs are grouped, using the aggregated values as "\code{num_col}".
#'      \item The group numbers for IDs are transferred to their rows.
#'    }
#'  }
#'
#'  \subsection{cat_col AND num_col AND id_col}{
#'    \enumerate{
#'      \item Values in \code{num_col} are aggregated for each ID, using \code{id_aggregation_fn}.
#'      \item IDs are subset by \code{cat_col}.
#'      \item The IDs for each subset are grouped,
#'      by using the aggregated values as "\code{num_col}".
#'      \item The group numbers for IDs are transferred to their rows.
#'    }
#'  }
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
                 remove_missing_starts = FALSE,
                 repeats=1, rm_identical_folds_cols = TRUE,
                 max_repeating_times=4){

  #
  # Takes:
  # .. dataframe
  # .. number of folds
  # .. a categorical variable to balance in folds
  # .... e.g. to predict between 2 diagnoses,
  # ..... you need both of them in the fold
  # .. an id variable for keeping a subject in the same fold
  # .. a numerical variable to balance in folds
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

  max_folds_col_number <- 0
  continue_repeating <- TRUE
  times_repeated <- 0

  while (isTRUE(continue_repeating)){

    # If num_col is not NULL
    if (!is.null(num_col)){
      for (r in 1:repeats){ # Replace with different loop fn
        data <- create_num_col_groups(data, n=k, num_col=num_col, cat_col=cat_col,
                                      id_col=id_col, col_name=ifelse(repeats > 1,
                                                                     paste0(".folds_", r + max_folds_col_number),
                                                                     ".folds"),
                                      id_aggregation_fn = id_aggregation_fn,
                                      method="n_fill",
                                      pre_randomize = TRUE) %>%
          dplyr::ungroup()

      }

    } else {

      # If cat_col is not NULL
      if (!is.null(cat_col)){

        # If id_col is not NULL
        if (!is.null(id_col)){

          # Group by cat_col
          # For each group:
          # .. create groups of the unique IDs (e.g. subjects)
          # .. add grouping factor to data
          # Group by new grouping factor '.folds'

          for (r in 1:repeats){ # Replace with different loop fn
            data <- data %>%
              group_by(!! as.name(cat_col)) %>%
              do(group_uniques_(., k, id_col, method,
                                col_name = ifelse(repeats > 1,
                                                paste0(".folds_", r + max_folds_col_number),
                                                ".folds"),
                                starts_col = starts_col,
                                remove_missing_starts = remove_missing_starts)) %>%
              dplyr::ungroup()
          }

        # If id_col is NULL
        } else {

          # Group by cat_col
          # Create groups from data
          # .. and add grouping factor to data

          for (r in 1:repeats){
            data <- data %>%
              group_by(!! as.name(cat_col)) %>%
              do(group(., k, method = method,
                       randomize = TRUE,
                       col_name = ifelse(repeats > 1,
                                         paste0(".folds_", r + max_folds_col_number),
                                         ".folds"),
                       starts_col = starts_col,
                       remove_missing_starts = remove_missing_starts)) %>%
              dplyr::ungroup()
          }
        }

      # If cat_col is NULL
      } else {

        # If id_col is not NULL
        if (!is.null(id_col)){

          # Create groups of unique IDs
          # .. and add grouping factor to data

          for (r in 1:repeats){
            data <- data %>%
              group_uniques_(k, id_col, method,
                             col_name = ifelse(repeats > 1,
                                               paste0(".folds_", r + max_folds_col_number),
                                               ".folds"),
                             starts_col = starts_col,
                             remove_missing_starts = remove_missing_starts) %>%
              dplyr::ungroup()

          }

        # If id_col is NULL
        } else {

          # Create groups from all the data points
          # .. and add grouping factor to data

          for (r in 1:repeats){
            data <- group(data, k,
                          method = method,
                          randomize = TRUE,
                          col_name = ifelse(repeats > 1,
                                            paste0(".folds_", r + max_folds_col_number),
                                            ".folds"),
                          starts_col = starts_col,
                          remove_missing_starts = remove_missing_starts) %>%
              dplyr::ungroup()
          }

        }
      }
    }

    # Add to repetition counter
    times_repeated = times_repeated + 1

    # Remove identical .folds columns or break out of while loop
    if (repeats>1 && isTRUE(rm_identical_folds_cols)){
      folds_colnames <- extract_folds_colnames(data)
      data <- remove_identical_cols(data, folds_colnames)
      folds_colnames <- extract_folds_colnames(data)

      # If we have generated too many unique folds cols
      # Remove some
      if (length(folds_colnames) > repeats){
        cols_to_remove <- folds_colnames[(repeats + 1) : length(folds_colnames)]
        data <- data %>% dplyr::select(-cols_to_remove)
        continue_repeating <- FALSE

      } else if (length(folds_colnames) == repeats || times_repeated == max_repeating_times){
        # If we have the right number of folds columns
        # or we have reached the max times
        # we wish to repeat the repeating
        # (TODO Find better vocabulary for this!)
        continue_repeating <- FALSE

      } else {
        # Find the max number in .folds_xx
        max_folds_col_number <- max(as.integer(substring(folds_colnames, 8)))
      }

    } else {
      continue_repeating <- FALSE
    }
  }



  # Group by .folds
  if (repeats == 1){
    data <- data %>%
      group_by(!! as.name('.folds'))

  } else {
    # Rename the folds columns to have consecutive number
    folds_colnames <- extract_folds_colnames(data)
    num_names_to_create <- length(folds_colnames)
    new_names <- paste0(".folds_", 1:num_names_to_create)

    data <- data %>%
      dplyr::rename_at(dplyr::vars(folds_colnames), ~ new_names)

  }

  # Return data
  return(data)


}

