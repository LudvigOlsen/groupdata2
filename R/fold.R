# R CMD check NOTE handling
if (getRversion() >= "2.15.1")
  utils::globalVariables(c("."))

## fold
#' @title Create balanced folds for cross-validation
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("stable")}
#'
#'  Divides data into groups by a range of methods.
#'  Balances a given categorical variable and/or numerical variable between folds and keeps (if possible)
#'  all data points with a shared ID (e.g. participant_id) in the same fold.
#'  Can create multiple unique fold columns for repeated cross-validation.
#' @details
#'  \subsection{cat_col}{
#'    \enumerate{
#'      \item \code{`data`} is subset by \code{`cat_col`}.
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
#'      \strong{Note} that this will only affect rows with the same value in \code{`num_col`}.
#'      \item Extreme pairing 1: Rows are ordered as \emph{smallest, largest, second smallest, second largest}, etc.
#'      Each pair get a group identifier.
#'      \item If \code{`extreme_pairing_levels` > 1}: The group identifiers are reordered as \emph{smallest,
#'      largest, second smallest, second largest}, etc., by the sum of \code{`num_col`} in the represented rows.
#'      These pairs (of pairs) get a new set of group identifiers, and the process is repeated
#'       \code{`extreme_pairing_levels`-2} times. Note that the group identifiers at the last level will represent
#'       \code{2^`extreme_pairing_levels`} rows, why you should be careful when choosing that setting.
#'      \item The final group identifiers are folded, and the fold identifiers are transferred to the rows.
#'    }
#'
#'  N.B. When doing extreme pairing of an unequal number of rows,
#'  the row with the smallest value is placed in a group by itself, and the order is instead:
#'  smallest, \emph{second smallest, largest, third smallest, second largest}, etc.
#'  }
#'
#'  \subsection{cat_col AND id_col}{
#'    \enumerate{
#'      \item \code{`data`} is subset by \code{`cat_col`}.
#'      \item Groups are created from unique IDs in each subset.
#'      \item Subsets are merged.
#'    }
#'  }
#'
#'  \subsection{cat_col AND num_col}{
#'    \enumerate{
#'      \item \code{`data`} is subset by \code{`cat_col`}.
#'      \item Subsets are grouped by \code{`num_col`}.
#'      \item Subsets are merged such that the largest group
#'      (by sum of \code{`num_col`}) from the first category
#'      is merged with the smallest group from the second category, etc.
#'    }
#'  }
#'
#'  \subsection{num_col AND id_col}{
#'    \enumerate{
#'      \item Values in \code{`num_col`} are aggregated for each ID, using \code{`id_aggregation_fn`}.
#'      \item The IDs are grouped, using the aggregated values as "\code{num_col}".
#'      \item The groups of the IDs are transferred to the rows.
#'    }
#'  }
#'
#'  \subsection{cat_col AND num_col AND id_col}{
#'    \enumerate{
#'      \item Values in \code{`num_col`} are aggregated for each ID, using \code{`id_aggregation_fn`}.
#'      \item IDs are subset by \code{`cat_col`}.
#'      \item The IDs in each subset are grouped,
#'      by using the aggregated values as "\code{num_col}".
#'      \item The subsets are merged such that the largest group
#'      (by sum of the aggregated values) from the first category
#'      is merged with the smallest group from the second category, etc.
#'      \item The groups of the IDs are transferred to the rows.
#'    }
#'  }
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param data \code{data.frame}. Can be \emph{grouped}, in which case
#'  the function is applied group-wise.
#' @param k \emph{Depends on \code{`method`}.}
#'
#'  Number of folds (default), fold size, with more (see \code{`method`}).
#'
#'  Given as whole number or percentage (\code{0 < `k` < 1}).
#' @param cat_col Name of categorical variable to balance between folds.
#'
#'  E.g. when predicting a binary variable (a or b), we usually want
#'  both classes represented in every fold.
#'
#'  N.B. If also passing an \code{`id_col`}, \code{`cat_col`} should be constant within each ID.
#' @param num_col Name of numerical variable to balance between folds.
#'
#'  N.B. When used with \code{`id_col`}, values for each ID are aggregated using
#'  \code{`id_aggregation_fn`} before being balanced.
#'
#'  N.B. When passing \code{`num_col`}, the \code{`method`} parameter is ignored.
#' @param id_col Name of factor with IDs.
#'  This will be used to keep all rows that share an ID in the same fold
#'  (if possible).
#'
#'  E.g. If we have measured a participant multiple times and want to see the
#'  effect of time, we want to have all observations of this participant in
#'  the same fold.
#'
#'  N.B. When \code{`data`} is a \emph{grouped} \code{data.frame}
#'  (see \code{\link[dplyr:group_by]{dplyr::group_by()}}), IDs that appear in multiple
#'  groupings might end up in different folds in those groupings.
#' @param id_aggregation_fn Function for aggregating values in \code{`num_col`}
#'  for each ID, before balancing \code{`num_col`}.
#'
#'  N.B. Only used when \code{`num_col`} and \code{`id_col`} are both specified.
#' @param extreme_pairing_levels How many levels of extreme pairing to do
#'  when balancing folds by a numerical column (i.e. \code{`num_col`} is specified).
#'
#'  \strong{Extreme pairing}: Rows/pairs are ordered as smallest, largest,
#'  second smallest, second largest, etc. If \code{extreme_pairing_levels > 1},
#'  this is done "recursively" on the extreme pairs. See \code{`Details/num_col`} for more.
#'
#'  N.B. Larger values work best with large datasets. If set too high,
#'  the result might not be stochastic. Always check if an increase
#'  actually makes the folds more balanced. See example.
#' @param num_fold_cols Number of fold columns to create.
#'  Useful for repeated cross-validation.
#'
#'  If \code{num_fold_cols > 1}, columns will be named
#'  \eqn{".folds_1"}, \eqn{".folds_2"}, etc.
#'  Otherwise simply \eqn{".folds"}.
#'
#'  N.B. If \code{`unique_fold_cols_only`} is \code{TRUE},
#'  we can end up with fewer columns than specified, see \code{`max_iters`}.
#'
#'  N.B. If \code{`data`} has existing fold columns, see \code{`handle_existing_fold_cols`}.
#' @param unique_fold_cols_only Check if fold columns are identical and
#'  keep only unique columns.
#'
#'  As the number of column comparisons can be time consuming,
#'  we can run this part in parallel. See \code{`parallel`}.
#'
#'  N.B. We can end up with fewer columns than specified in
#'  \code{`num_fold_cols`}, see \code{`max_iters`}.
#'
#'  N.B. Only used when \code{`num_fold_cols` > 1} or \code{`data`} has existing fold columns.
#' @param max_iters Maximum number of attempts at reaching
#'  \code{`num_fold_cols`} \emph{unique} fold columns.
#'
#'  When only keeping unique fold columns, we risk having fewer columns than expected.
#'  Hence, we repeatedly create the missing columns and remove those that are not unique.
#'  This is done until we have \code{`num_fold_cols`} unique fold columns
#'  or we have attempted \code{`max_iters`} times.
#'  In some cases, it is not possible to create \code{`num_fold_cols`}
#'  unique combinations of the dataset, e.g.
#'  when specifying \code{`cat_col`}, \code{`id_col`} and \code{`num_col`}.
#'  \code{`max_iters`} specifies when to stop trying.
#'  Note that we can end up with fewer columns than specified in \code{`num_fold_cols`}.
#'
#'  N.B. Only used \code{`num_fold_cols` > 1}.
#' @param handle_existing_fold_cols How to handle existing fold columns.
#'  Either \code{"keep_warn"}, \code{"keep"}, or \code{"remove"}.
#'
#'  To \strong{add} extra fold columns, use \code{"keep"} or \code{"keep_warn"}.
#'  Note that existing fold columns might be renamed.
#'
#'  To \strong{replace} the existing fold columns, use \code{"remove"}.
#' @param parallel Whether to parallelize the fold column comparisons,
#'  when \code{`unique_fold_cols_only`} is \code{TRUE}.
#'
#'  Requires a registered parallel backend.
#'  Like \code{doParallel::registerDoParallel}.
#' @param method \code{"n_dist"}, \code{"n_fill"}, \code{"n_last"},
#'  \code{"n_rand"}, \code{"greedy"}, or \code{"staircase"}.
#'
#'  \strong{Notice}: examples are sizes of the generated groups
#'  based on a vector with \code{57} elements.
#'
#'  \subsection{n_dist (default)}{Divides the data into a specified number of groups and
#'  distributes excess data points across groups
#'  \eqn{(e.g. 11, 11, 12, 11, 12)}.
#'
#'  \code{`k`} is number of groups}
#'
#'  \subsection{n_fill}{Divides the data into a specified number of groups and
#'  fills up groups with excess data points from the beginning
#'  \eqn{(e.g. 12, 12, 11, 11, 11)}.
#'
#'  \code{`k`} is number of groups}
#'
#'  \subsection{n_last}{Divides the data into a specified number of groups.
#'  It finds the most equal group sizes possible,
#'  using all data points. Only the last group is able to differ in size
#'  \eqn{(e.g. 11, 11, 11, 11, 13)}.
#'
#'  \code{`k`} is number of groups}
#'
#'  \subsection{n_rand}{Divides the data into a specified number of groups.
#'  Excess data points are placed randomly in groups (only 1 per group)
#'  \eqn{(e.g. 12, 11, 11, 11, 12)}.
#'
#'  \code{`k`} is number of groups}
#'
#'  \subsection{greedy}{Divides up the data greedily given a specified group size
#'  \eqn{(e.g. 10, 10, 10, 10, 10, 7)}.
#'
#'  \code{`k`} is group size}
#'
#'  \subsection{staircase}{Uses step size to divide up the data.
#'  Group size increases with 1 step for every group,
#'  until there is no more data
#'  \eqn{(e.g. 5, 10, 15, 20, 7)}.
#'
#'  \code{`k`} is step size}
#' @aliases create_balanced_groups
#' @family grouping functions
#' @return \code{data.frame} with grouping factor for subsetting in cross-validation.
#' @seealso \code{\link{partition}} for balanced partitions
#' @examples
#' # Attach packages
#' library(groupdata2)
#' library(dplyr)
#'
#' # Create data frame
#' df <- data.frame(
#'   "participant" = factor(rep(c("1", "2", "3", "4", "5", "6"), 3)),
#'   "age" = rep(sample(c(1:100), 6), 3),
#'   "diagnosis" = factor(rep(c("a", "b", "a", "a", "b", "b"), 3)),
#'   "score" = sample(c(1:100), 3 * 6)
#' )
#' df <- df %>% arrange(participant)
#' df$session <- rep(c("1", "2", "3"), 6)
#'
#' # Using fold()
#'
#' ## Without balancing
#' df_folded <- fold(df, 3, method = "n_dist")
#'
#' ## With cat_col
#' df_folded <- fold(df, 3,
#'   cat_col = "diagnosis",
#'   method = "n_dist"
#' )
#'
#' ## With id_col
#' df_folded <- fold(df, 3,
#'   id_col = "participant",
#'   method = "n_dist"
#' )
#'
#' ## With num_col
#' # Note: 'method' would not be used in this case
#' df_folded <- fold(df, 3, num_col = "score")
#'
#' # With cat_col and id_col
#' df_folded <- fold(df, 3,
#'   cat_col = "diagnosis",
#'   id_col = "participant", method = "n_dist"
#' )
#'
#' ## With cat_col, id_col and num_col
#' df_folded <- fold(df, 3,
#'   cat_col = "diagnosis",
#'   id_col = "participant", num_col = "score"
#' )
#'
#' # Order by folds
#' df_folded <- df_folded %>% arrange(.folds)
#'
#' ## Multiple fold columns
#' # Useful for repeated cross-validation
#' # Note: Consider running in parallel
#' df_folded <- fold(df, 3,
#'   cat_col = "diagnosis",
#'   id_col = "participant", num_fold_cols = 5,
#'   unique_fold_cols_only = TRUE,
#'   max_iters = 4
#' )
#'
#' ## Check if additional extreme_pairing_levels
#' ## improve the numerical balance
#' set.seed(2) # try with seed 1 as well
#' df_folded_1 <- fold(df, 3,
#'   num_col = "score",
#'   extreme_pairing_levels = 1
#' )
#' df_folded_1 %>%
#'   dplyr::group_by(.folds) %>%
#'   dplyr::summarise(
#'     sum_score = sum(score),
#'     mean_score = mean(score)
#'   )
#'
#' set.seed(2) # try with seed 1 as well
#' df_folded_2 <- fold(df, 3,
#'   num_col = "score",
#'   extreme_pairing_levels = 2
#' )
#' df_folded_2 %>%
#'   dplyr::group_by(.folds) %>%
#'   dplyr::summarise(
#'     sum_score = sum(score),
#'     mean_score = mean(score)
#'   )
#' @importFrom dplyr %>%
#' @importFrom utils combn
#' @importFrom rlang .data
#' @importFrom stats runif
fold <- function(data,
                 k = 5,
                 cat_col = NULL,
                 num_col = NULL,
                 id_col = NULL,
                 method = "n_dist",
                 id_aggregation_fn = sum,
                 extreme_pairing_levels = 1,
                 num_fold_cols = 1,
                 unique_fold_cols_only = TRUE,
                 max_iters = 5,
                 handle_existing_fold_cols = "keep_warn",
                 parallel = FALSE) {

  # Check inputs
  check_fold_once(
    data = data,
    k = k,
    cat_col = cat_col,
    num_col = num_col,
    id_col = id_col,
    method = method,
    id_aggregation_fn = id_aggregation_fn,
    extreme_pairing_levels = extreme_pairing_levels,
    num_fold_cols = num_fold_cols,
    unique_fold_cols_only = unique_fold_cols_only,
    max_iters = max_iters,
    handle_existing_fold_cols = handle_existing_fold_cols,
    parallel = parallel
  )

  if (dplyr::is_grouped_df(data)) {
    message_once_about_group_by("fold")
  }

  # Apply by group (recursion)
  run_by_group_df(
    data = data,
    .fn = run_fold_,
    k = k,
    cat_col = cat_col,
    num_col = num_col,
    id_col = id_col,
    method = method,
    id_aggregation_fn = id_aggregation_fn,
    extreme_pairing_levels = extreme_pairing_levels,
    num_fold_cols = num_fold_cols,
    unique_fold_cols_only = unique_fold_cols_only,
    max_iters = max_iters,
    handle_existing_fold_cols = handle_existing_fold_cols,
    parallel = parallel
  )

}

run_fold_ <- function(data,
                      k,
                      cat_col,
                      num_col,
                      id_col,
                      method,
                      id_aggregation_fn,
                      extreme_pairing_levels,
                      num_fold_cols,
                      unique_fold_cols_only,
                      max_iters,
                      handle_existing_fold_cols,
                      parallel) {
  checks <-
    fold_check_convert_k(
      data = data,
      k = k,
      method = method,
      cat_col = cat_col,
      id_col = id_col
    )

  k <- checks[["k"]]

  # Check for existing fold columns
  existing_fold_colnames <- extract_fold_colnames(data)
  num_existing_fold_colnames <- length(existing_fold_colnames)
  if (num_existing_fold_colnames > 0) {
    # Handle existing fold cols as specified
    if (handle_existing_fold_cols == "remove") {
      # We need to ungroup the dataset, or it will be automatically included again.
      data <- data %>%
        dplyr::ungroup() %>%
        base_deselect(cols = existing_fold_colnames)
      existing_fold_colnames <- character()
      num_existing_fold_colnames <- 0
    } else if (handle_existing_fold_cols == "keep_warn") {
      if (num_existing_fold_colnames == 1) {
        warn_terms__ <- c("column", "It", "it")
      }
      else {
        warn_terms__ <- c("columns", "These", "them")
      }
      warning(
        paste0(
          "Found ",
          num_existing_fold_colnames,
          " existing fold ",
          warn_terms__[1],
          ". ",
          warn_terms__[2],
          " will NOT be replaced. ",
          "Change 'handle_existing_fold_cols' to 'remove' if you want to replace ",
          warn_terms__[3],
          "."
        )
      )
    }
  }

  fold_cols_to_generate <- num_fold_cols
  expected_total_num_fold_cols <-
    num_existing_fold_colnames + num_fold_cols
  max_fold_cols_number <- ifelse(
    num_existing_fold_colnames > 0,
    extract_max_fold_cols_number(existing_fold_colnames),
    0
  )
  continue_repeating <- TRUE
  times_repeated <- 0
  completed_comparisons <- data.frame(
    "V1" = character(),
    "V2" = character(),
    "identical" = logical(),
    stringsAsFactors = FALSE
  )

  while (isTRUE(continue_repeating)) {
    # If num_col is not NULL
    if (!is.null(num_col)) {
      plyr::l_ply(1:fold_cols_to_generate, function(r) {
        data <<- create_num_col_groups(
          data,
          n = k,
          num_col = num_col,
          cat_col = cat_col,
          id_col = id_col,
          col_name = name_new_fold_col(
            num_to_create = num_fold_cols,
            num_existing = num_existing_fold_colnames,
            max_existing_number = max_fold_cols_number,
            current = r
          ),
          id_aggregation_fn = id_aggregation_fn,
          extreme_pairing_levels = extreme_pairing_levels,
          method = "n_fill",
          pre_randomize = TRUE
        ) %>%
          dplyr::ungroup()
      })
    } else {
      # If cat_col is not NULL
      if (!is.null(cat_col)) {
        # If id_col is not NULL
        if (!is.null(id_col)) {
          # Group by cat_col
          # For each group:
          # .. create groups of the unique IDs (e.g. subjects)
          # .. add grouping factor to data
          # Group by new grouping factor '.folds'

          # for (r in 1:fold_cols_to_generate){ # Replace with different loop fn
          plyr::l_ply(1:fold_cols_to_generate, function(r) {
            data <<- data %>%
              dplyr::group_by(!!as.name(cat_col)) %>%
              dplyr::do(group_uniques_(
                .,
                k,
                id_col,
                method,
                col_name = name_new_fold_col(
                  num_to_create = num_fold_cols,
                  num_existing = num_existing_fold_colnames,
                  max_existing_number = max_fold_cols_number,
                  current = r
                )
              )) %>%
              dplyr::ungroup()
          })

          # If id_col is NULL
        } else {
          # Group by cat_col
          # Create groups from data
          # .. and add grouping factor to data

          # for (r in 1:fold_cols_to_generate){
          plyr::l_ply(1:fold_cols_to_generate, function(r) {
            data <<- data %>%
              dplyr::group_by(!!as.name(cat_col)) %>%
              dplyr::do(
                group(
                  .,
                  k,
                  method = method,
                  randomize = TRUE,
                  col_name = name_new_fold_col(
                    num_to_create = num_fold_cols,
                    num_existing = num_existing_fold_colnames,
                    max_existing_number = max_fold_cols_number,
                    current = r
                  )
                )
              ) %>%
              dplyr::ungroup()
          })
        }

        # If cat_col is NULL
      } else {
        # If id_col is not NULL
        if (!is.null(id_col)) {
          # Create groups of unique IDs
          # .. and add grouping factor to data

          # for (r in 1:fold_cols_to_generate){
          plyr::l_ply(1:fold_cols_to_generate, function(r) {
            data <<- data %>%
              group_uniques_(
                n = k,
                id_col = id_col,
                method = method,
                col_name = name_new_fold_col(
                  num_to_create = num_fold_cols,
                  num_existing = num_existing_fold_colnames,
                  max_existing_number = max_fold_cols_number,
                  current = r
                )
              ) %>%
              dplyr::ungroup()
          })

          # If id_col is NULL
        } else {
          # Create groups from all the data points
          # .. and add grouping factor to data

          plyr::l_ply(1:fold_cols_to_generate, function(r) {
            # for (r in 1:fold_cols_to_generate){
            data <<- group(
              data = data,
              n = k,
              method = method,
              randomize = TRUE,
              col_name = name_new_fold_col(
                num_to_create = num_fold_cols,
                num_existing = num_existing_fold_colnames,
                max_existing_number = max_fold_cols_number,
                current = r
              )
            ) %>%
              dplyr::ungroup()
          })
        }
      }
    }

    # Add to repetition counter
    times_repeated <- times_repeated + 1

    # Remove identical .folds columns or break out of while loop
    if (expected_total_num_fold_cols > 1 &&
        isTRUE(unique_fold_cols_only)) {
      folds_colnames <- extract_fold_colnames(data)

      data_and_comparisons <-
        remove_identical_cols(
          data,
          folds_colnames,
          exclude_comparisons = completed_comparisons,
          return_all_comparisons = TRUE,
          group_wise = TRUE,
          parallel = parallel
        )

      data <- data_and_comparisons[["updated_data"]]
      removed_cols <- data_and_comparisons[["removed_cols"]]
      completed_comparisons <- completed_comparisons %>%
        dplyr::bind_rows(
          data_and_comparisons[["comparisons"]] %>%
            # If they were identical,
            # we removed one and the comparison isn't useful to save
            dplyr::filter(!identical, .data$V2 %ni% removed_cols)
          )

      folds_colnames <- extract_fold_colnames(data)

      if (length(folds_colnames) < expected_total_num_fold_cols) {
        fold_cols_to_generate <-
          expected_total_num_fold_cols - length(folds_colnames)
      }

      # If we have generated too many unique folds cols
      # NOTE: This should not happen anymore
      # Remove some
      if (length(folds_colnames) > expected_total_num_fold_cols) {
        cols_to_remove <-
          folds_colnames[(expected_total_num_fold_cols + 1):length(folds_colnames)]
        data <- data %>% base_deselect(cols = cols_to_remove)
        continue_repeating <- FALSE
      } else if (length(folds_colnames) == expected_total_num_fold_cols ||
                 times_repeated == max_iters) {
        # If we have the right number of fold columns
        # or we have reached the max times
        # we wish to repeat the repeating
        # (TODO Find better vocabulary for this!)
        continue_repeating <- FALSE
      } else {
        # Find the max number in .folds_xx
        max_fold_cols_number <-
          extract_max_fold_cols_number(folds_colnames)
      }
    } else {
      continue_repeating <- FALSE
    }
  }

  if (num_fold_cols == 1 && expected_total_num_fold_cols == 1) {
    # Group by .folds
    data <- data %>%
      dplyr::group_by(!!as.name(".folds"))
  } else {
    # Rename the fold columns to have consecutive number
    folds_colnames <- extract_fold_colnames(data)
    data <-
      rename_with_consecutive_numbering(data, folds_colnames, ".folds_")
  }

  # Return data
  data
}

fold_check_convert_k <- function(data, k, method, cat_col, id_col) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()

  if (!is.null(id_col) &&
      !is.null(cat_col)) {
    # Check that cat_col is constant within each ID
    # Note: I tested and count() is faster than group_keys()
    counts <- dplyr::count(data, !!as.name(id_col), !!as.name(cat_col))
    if (nrow(counts) != length(unique(counts[[id_col]]))) {
      assert_collection$push("The value in 'data[[cat_col]]' must be constant within each ID.")
    }
  }

  # k

  checkmate::assert_number(
    x = k,
    lower = 0,
    upper = nrow(data),
    finite = TRUE,
    add = assert_collection
  )

  # Convert k to wholenumber if given as percentage
  if (!arg_is_wholenumber_(k) && is_between_(k, 0, 1)) {
    rows_per_fold <- convert_percentage_(k, data)
    k <- ceiling(nrow(data) / rows_per_fold)
  }
  checkmate::assert_count(x = k,
                          positive = TRUE,
                          add = assert_collection)

  # If method is either greedy or staircase and cat_col is not NULL
  # we don't want k elements per level in cat_col
  # so we divide k by the number of levels in cat_col
  if (method %in% c("greedy", "staircase") && !is.null(cat_col)) {
    n_levels_cat_col <- length(unique(data[[cat_col]]))
    k <- ceiling(k / n_levels_cat_col)
  }

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  list("k" = k)

}

check_fold_once <- function(data,
                            k,
                            cat_col,
                            num_col,
                            id_col,
                            method,
                            id_aggregation_fn,
                            extreme_pairing_levels,
                            num_fold_cols,
                            unique_fold_cols_only,
                            max_iters,
                            handle_existing_fold_cols,
                            parallel) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data,
                               min.rows = 1,
                               add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_number(
    x = k,
    lower = 0,
    finite = TRUE,
    add = assert_collection
  )
  checkmate::assert_count(x = extreme_pairing_levels,
                          positive = TRUE,
                          add = assert_collection)
  checkmate::assert_count(x = num_fold_cols,
                          positive = TRUE,
                          add = assert_collection)
  checkmate::assert_count(x = max_iters,
                          positive = TRUE,
                          add = assert_collection)
  checkmate::assert_flag(x = unique_fold_cols_only, add = assert_collection)
  checkmate::assert_flag(x = parallel, add = assert_collection)
  checkmate::assert_character(
    x = cat_col,
    min.len = 1,
    any.missing = FALSE,
    null.ok = TRUE,
    unique = TRUE,
    names = "unnamed",
    add = assert_collection
  )
  checkmate::assert_string(
    x = num_col,
    na.ok = FALSE,
    min.chars = 1,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_string(
    x = id_col,
    na.ok = FALSE,
    min.chars = 1,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_string(x = method,
                           min.chars = 1,
                           add = assert_collection)
  checkmate::assert_function(x = id_aggregation_fn, add = assert_collection)
  checkmate::assert_string(x = handle_existing_fold_cols,
                           add = assert_collection)
  checkmate::reportAssertions(assert_collection)


  # TODO Could have a helper for adding this kind of message:
  if (!is.null(cat_col) &&
      length(setdiff(cat_col, colnames(data))) != 0) {
    assert_collection$push(paste0(
      "'cat_col' column(s), '",
      paste0(setdiff(cat_col, colnames(data)), collapse = ", "),
      "', not found in 'data'."
    ))
  }
  if (!is.null(num_col)) {
    if (num_col %ni% colnames(data)) {
      assert_collection$push(paste0("'num_col' column, '", num_col, "', not found in 'data'."))
    }
    checkmate::reportAssertions(assert_collection)
    if (!checkmate::test_numeric(data[[num_col]])) {
      assert_collection$push(paste0("'num_col' column must be numeric."))
    }
  }
  if (!is.null(id_col) && id_col %ni% colnames(data)) {
    assert_collection$push(paste0("'id_col' column, '", id_col, "', not found in 'data'."))
  }
  checkmate::assert_names(
    x = method,
    subset.of = c("n_dist", "n_fill", "n_last", "n_rand", "greedy", "staircase"),
    add = assert_collection
  )
  checkmate::assert_names(
    x = handle_existing_fold_cols,
    subset.of = c("keep_warn", "keep", "remove"),
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  if (!is.null(id_col)) {
    checkmate::assert_factor(x = data[[id_col]], add = assert_collection)
    if (!is.null(cat_col)) {
      if (id_col %in% cat_col) {
        assert_collection$push("'id_col' and 'cat_col' cannot contain the same column name.")
      }
    }
  }

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # If num_col is specified, warn that method is ignored
  if (!is.null(num_col) & method != "n_dist") {
    warning(
      paste0(
        "'method' is ignored when 'num_col' is not 'NULL'. ",
        "This warning occurs, because 'method' is not the default value."
      )
    )
  }

}
