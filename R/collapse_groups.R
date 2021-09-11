

#   __________________ #< d0ef3dcf6f36ff79828d86bb092f9b67 ># __________________
#   Collapse groups                                                         ####

# TODO auto-tune does not use cat_levels when ranking

# TODO Consider adding fold style handling of existing .coll_group* columns?

# TODO Allow group_aggregation_fn to be a list of functions to combine results
# of, e.g. to balance both mean and sum - e.g. with a weight as well?
# Also - a list with different functions per column in num_cols?

# TODO cat_levels: provide .majority/.minority in the list?
# TODO cat_levels: What happens if a column is not in the list? (should use all for that column)

#' @title Collapse groups with categorical, numerical, ID, and size balancing
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Collapses a set of groups into a smaller set of groups.
#'
#'  Balance the new groups by numerical columns,
#'  categorical columns,
#'  level counts in ID columns,
#'  and/or the number of rows (size).
#'
#'  \strong{Note}: The more of these you balance at a time,
#'  the less balanced each of them may become. While on average,
#'  the balancing work better than without, this is
#'  not guaranteed on every run. Enabling \code{`auto_tune`} should yield a
#'  much better overall balance than without in most contexts.
#'  This generates a larger set of group columns using all combinations of the
#'  balancing columns and selects the most balanced group column(s).
#'  This is slower and we recommend enabling parallelization (see \code{`parallel`}).
#'
#'  \strong{Tip}: Check the balances of the new groups with
#'  \code{\link[groupdata2:summarize_balances]{summarize_balances()}}.
#' @details
#'  The goal of \code{collapse_groups()} is to combine existing groups
#'  to a lower number of groups while (optionally) balancing one or more of
#'  four "balancing dimensions" (\emph{size}, \emph{numeric}, \emph{categorical},
#'  and/or \emph{ID}). From each of these dimensions, we calculate
#'  a normalized, numeric \emph{"balancing column"} that when balanced
#'  between the groups lead to its dimension being balanced as well.
#'
#'  To balance multiple dimensions at once, we combine their balancing columns with
#'  with weighted averaging (see \code{`combine_method`} and \code{`weights`}).
#'
#'  Finally, we create groups where this combined balancing column is balanced using the
#'  numerical balancing in \code{\link[groupdata2:fold]{fold()}}.
#'
#'  \subsection{Auto-tuning}{
#'
#'  This strategy is not guaranteed to produce balanced groups in all contexts,
#'  e.g. when the balancing columns cancel out. To increase the probability of
#'  balanced groups, we can produce multiple group columns with all combinations
#'  of the balancing dimensions and select the overall most balanced group column(s).
#'  We refer to this as auto-tuning (see \code{`auto_tune`}).
#'
#'  We find the most balanced group column from the rankings of across-group
#'  standard deviations for each of the balancing dimensions, using the summaries produced by
#'  \code{\link[groupdata2:summarize_balances]{summarize_balances()}}. E.g. if for
#'  a group column the groups have the following average \emph{ages} \code{`c(16, 18, 25, 21)`},
#'  the standard deviation thereof (\code{3.92}) is our measure for how balanced the \emph{age}
#'  column is. Another group column can thus have a lower/higher standard deviation.
#'  We find the rankings of these standard deviations for all the balancing dimensions
#'  and average them (again weighted by \code{`weights`}). We select the, on average, highest ranking
#'  (i.e. lowest standard deviations) group column(s).
#'
#'  }\subsection{Checking balances}{
#'
#'  We highly recommend using
#'  \code{\link[groupdata2:summarize_balances]{summarize_balances()}} to
#'  check how balanced the created groups are on the various dimensions.
#'  By applying \code{\link[groupdata2:ranked_balances]{ranked_balances()}}
#'  to the summaries, we get a \code{data.frame} with the standard deviations
#'  for each balancing dimension, ordered by the average rank.
#'
#'  }\subsection{Balancing columns}{
#'
#'  The following describes the creation of the balancing columns
#'  for each of the balancing dimensions:
#'
#'  \subsection{cat_cols}{
#'  For each column in \code{`cat_cols`}:
#'
#'   * \strong{Count each level} in each group. This creates a \code{data.frame} with
#'   one count column per level, with one row per group.
#'   * \strong{Standardize} the count columns.
#'   * \strong{Sum} the standardized counts rowwise to create one combined column representing
#'   the balance of the levels for each group.
#'
#'   \strong{Example}: Consider a factor column with the levels \code{c("A", "B", "C")}.
#'   We count each level per group, normalize the counts and combine them with weighted averaging:
#'
#'   \tabular{rrrrrrrrrr}{
#'   \strong{Group} \tab \strong{A} \tab
#'   \strong{B} \tab \strong{C} \tab
#'   \strong{ -> } \tab \strong{nA} \tab
#'   \strong{nB} \tab \strong{nC} \tab
#'   \strong{ -> } \tab \strong{Combined}\cr
#'   1 \tab 5 \tab 57 \tab 1 \tab | \tab 0.24 \tab 0.55 \tab -0.77 \tab | \tab 0.007  \cr
#'   2 \tab 7 \tab 69 \tab 2 \tab | \tab 0.93 \tab 0.64 \tab -0.77 \tab | \tab 0.267 \cr
#'   3 \tab 2 \tab 34 \tab 14\tab | \tab -1.42\tab 0.29 \tab 1.34  \tab | \tab 0.07 \cr
#'   4 \tab 5 \tab 0 \tab 4  \tab | \tab 0.24 \tab -1.48\tab 0.19  \tab | \tab -0.35 \cr
#'   ... \tab ... \tab ... \tab ... \tab | \tab ... \tab ... \tab ... \tab | \tab ... }
#'
#'  }
#'
#'  \subsection{id_cols}{
#'  For each column in \code{`id_cols`}:
#'
#'   * \strong{Count} the unique IDs (levels) within each group.
#'   (Note: The same ID can be counted in multiple groups.)
#'  }
#'
#'  \subsection{num_cols}{
#'  For each column in \code{`num_cols`}:
#'
#'   * \strong{Aggregate} the numeric columns by group using the \code{`group_aggregation_fn`}.
#'  }
#'
#'  \subsection{size}{
#'   * \strong{Count} the number of rows per group.
#'  }
#'
#'  \subsection{Combining balancing columns}{
#'   * Apply standardization or MinMax scaling to each of the balancing columns (see \code{`combine_method`}).
#'   * Perform weighted averaging to get a single balancing column (see \code{`weights`}).
#'
#'   \strong{Example}: We apply standardization and perform weighted averaging:
#'
#'   \tabular{rrrrrrrrrrrr}{
#'   \strong{Group} \tab \strong{Size} \tab
#'   \strong{Num} \tab \strong{Cat} \tab
#'   \strong{ID} \tab \strong{->} \tab
#'   \strong{nSize} \tab \strong{nNum}
#'   \tab \strong{nCat} \tab \strong{nID} \tab
#'   \strong{->} \tab \strong{Combined}\cr
#'   1 \tab 34 \tab 1.3 \tab 0.007 \tab 3 \tab | \tab -0.33 \tab -0.82 \tab 0.03 \tab -0.46\tab | \tab -0.395  \cr
#'   2 \tab 23 \tab 4.6 \tab 0.267 \tab 4 \tab | \tab -1.12 \tab 0.34  \tab 1.04 \tab 0.0  \tab | \tab 0.065 \cr
#'   3 \tab 56 \tab 7.2 \tab 0.07  \tab 7 \tab | \tab 1.27  \tab 1.26  \tab 0.28 \tab 1.39 \tab | \tab 1.05 \cr
#'   4 \tab 41 \tab 1.4 \tab -0.35 \tab 2 \tab | \tab 0.18  \tab -0.79 \tab -1.35\tab -0.93\tab | \tab -0.723 \cr
#'   ... \tab ... \tab ... \tab ... \tab ... \tab |
#'   \tab ... \tab ... \tab ... \tab ... \tab | \tab ... }
#'
#'  }
#'
#'  }\subsection{Creating the groups}{
#'
#'   Finally, we get to the group creation. There are three methods for creating groups based on the
#'   combined balancing column: "balance", "ascending", and "descending".
#'
#'   \subsection{`method` is "balance"}{
#'   To create groups that are balanced by the combined balancing column, we use the numerical balancing
#'   in \code{\link[groupdata2:fold]{fold()}}.
#'
#'   The following describes the numerical balancing in broad terms:
#'
#'   \enumerate{
#'      \item Rows are shuffled.
#'      \strong{Note} that this will only affect rows with the same value in the combined balancing column.
#'      \item Extreme pairing 1: Rows are ordered as \emph{smallest, largest, second smallest, second largest}, etc.
#'      Each pair get a group identifier. (See \code{\link[rearrr:pair_extremes]{rearrr::pair_extremes()}})
#'      \item If \code{`extreme_pairing_levels` > 1}: These group identifiers are reordered as \emph{smallest,
#'      largest, second smallest, second largest}, etc., by the sum of the combined balancing column in the represented rows.
#'      These pairs (of pairs) get a new set of group identifiers, and the process is repeated
#'       \code{`extreme_pairing_levels`-2} times. Note that the group identifiers at the last level will represent
#'       \code{2^`extreme_pairing_levels`} rows, why you should be careful when choosing that setting.
#'      \item The group identifiers from the last pairing are randomly divided into the final groups,
#'      and these final identifiers are transferred to the original rows.
#'   }
#'
#'   \strong{N.B.} When doing extreme pairing of an unequal number of rows,
#'   the row with the smallest value is placed in a group by itself, and the order is instead:
#'   smallest, \emph{second smallest, largest, third smallest, second largest}, etc.
#'
#'   \strong{Example}: We order the \code{data.frame} by smallest \emph{"Num"} value,
#'   largest \emph{"Num"} value, second smallest, and so on.
#'   We could further (when \code{`extreme_pairing_levels` > 1})
#'   find the sum of \emph{"Num"} for each pair and perform extreme pairing on the pairs.
#'   Finally, we group the \code{data.frame}:
#'
#'   \tabular{rrrrrrrrrrrr}{
#'     \strong{Group} \tab \strong{Num} \tab
#'     \strong{->} \tab
#'     \strong{Group} \tab \strong{Num} \tab
#'     \strong{Pair} \tab \strong{->} \tab
#'     \strong{New group}\cr
#'     1 \tab -0.395\tab | \tab 5 \tab -1.23 \tab 1 \tab | \tab 3 \cr
#'     2 \tab 0.065 \tab | \tab 3 \tab 1.05  \tab 1 \tab | \tab 3 \cr
#'     3 \tab 1.05  \tab | \tab 4 \tab -0.723\tab 2 \tab | \tab 1 \cr
#'     4 \tab -0.723\tab | \tab 2 \tab 0.065 \tab 2 \tab | \tab 1 \cr
#'     5 \tab -1.23 \tab | \tab 1 \tab -0.395\tab 3 \tab | \tab 2 \cr
#'     6 \tab -0.15 \tab | \tab 6 \tab -0.15 \tab 3 \tab | \tab 2 \cr
#'     ... \tab ... \tab | \tab ... \tab ... \tab ... \tab | \tab ... }
#'
#'   }
#'
#'   \subsection{`method` is "ascending" or "descending"}{
#'   These methods order the data by the combined balancing column and
#'   creates groups such that the sums get increasingly larger (\code{`ascending`})
#'   or smaller (\code{`descending`}). This will in turn lead to a \emph{pattern} of
#'   increasing/decreasing sums in the balancing columns (e.g. increasing/decreasing counts
#'   of the categorical levels, counts of IDs, number of rows and sums of numeric columns).
#'   }
#'
#' }
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param data \code{data.frame}. Can be \emph{grouped}, in which case
#'  the function is applied group-wise.
#' @param n Number of new groups.
#'
#'  When \code{`num_new_group_cols` > 1}, \code{`n`} can also be a vector
#'  with one \code{`n`} per new group column. This allows trying multiple \code{`n`}
#'  settings at a time. Note that the generated group columns are not guaranteed
#'  to be in the order of \code{`n`}.
#' @param balance_size Whether to balance the size of the collapsed groups. (logical)
#' @param cat_cols Names of categorical columns to balance the average frequency
#'  of one or more levels of.
#' @param cat_levels Names of the levels in the \code{`cat_cols`} columns to balance the average frequencies
#'  of. When \code{`NULL`} (default), all levels are balanced.
#'  Can be weights indicating the balancing importance of each level (within each column).
#'
#'  The weights are automatically scaled to sum to \code{1}.
#'
#'  Can be \code{".minority"} or \code{".majority"}, in which case the minority/majority level
#'  are found and used.
#'
#'  \subsection{When \code{`cat_cols`} has single column name:}{
#'
#'  Either a \code{vector} with level names or a named \code{numeric vector} with weights:
#'
#'  E.g. \code{c("dog", "pidgeon", "mouse")} or \code{c("dog" = 5, "pidgeon" = 1, "mouse" = 3)}
#'  }
#'
#'  \subsection{When \code{`cat_cols`} has multiple column names:}{
#'
#'  A named \code{list} with \code{vector}s for each column name in \code{`cat_cols`}.
#'  When not providing a \code{vector} for a \code{`cat_cols`}
#'  column, all levels are balanced in that column.
#'
#'  E.g. \code{list("col1" = c("dog" = 5, "pidgeon" = 1, "mouse" = 3),
#'  "col2" = c("hydrated", "dehydrated"))}.
#'  }
#' @param num_cols Names of numerical columns to balance between groups.
#' @param id_cols Names of factor columns with IDs to balance the counts of between groups.
#' @param group_cols Names of factors in \code{`data`} for identifying the \emph{existing} groups
#'  that should be collapsed.
#'
#'  Multiple names are treated as in \code{\link[dplyr:group_by]{dplyr::group_by()}}
#'  (i.e., a hierarchy of groups), where each leaf group within each parent group is
#'  considered a unique group to be collapsed.
#'  Parent groups are not considered during collapsing, why leaf groups from different
#'  parent groups can be collapsed together.
#'
#'  \strong{Note}: Do not confuse these group columns with potential columns that \code{`data`} is grouped by.
#'  \code{`group_cols`} identifies the groups to be collapsed. When \code{`data`} is
#'  grouped with \code{\link[dplyr:group_by]{dplyr::group_by()}}, the function is
#'  applied separately to each of those subsets.
#' @param group_aggregation_fn Function for aggregating values in the \code{`num_cols`} columns
#'  for each group in \code{`group_cols`}.
#'
#'  Default is \code{mean()}, where the average value(s) are balanced across the new groups.
#'
#'  When using \code{sum()}, the groups will have similar sums across the new groups.
#'
#'  \strong{N.B.} Only used when \code{`num_cols`} is specified.
#' @param auto_tune Whether to create a larger set of collapsed group columns
#'  from all combinations of the balancing dimensions and select the
#'  overall most balanced group column(s).
#'
#'  This tends to create much more balanced collapsed group columns.
#'
#'  Can be slow, why we recommend enabling parallelization (see \code{`parallel`}).
#' @param method \code{"balance"}, \code{"ascending"}, or \code{"descending"}:
#'
#'  After calculating a \emph{combined balancing column} from each of the balancing columns (see \code{Details >> Balancing columns}):
#'
#'  * \code{"balance"} balances the combined balancing column between the groups.
#'  * \code{"ascending"} orders the combined balancing column and groups from the lowest to highest value.
#'  * \code{"descending"} orders the combined balancing column and groups from the highest to lowest value.
#' @param extreme_pairing_levels How many levels of extreme pairing to do
#'  when balancing the groups by the combined balancing column (see \code{Details}).
#'
#'  \strong{Extreme pairing}: Rows/pairs are ordered as smallest, largest,
#'  second smallest, second largest, etc. If \code{extreme_pairing_levels > 1},
#'  this is done "recursively" on the extreme pairs.
#'
#'  \strong{N.B.} Larger values work best with large datasets. If set too high,
#'  the result might not be stochastic. Always check if an increase
#'  actually makes the groups more balanced.
#' @param num_new_group_cols Number of group columns to create.
#'
#'  When \code{`num_new_group_cols` > 1}, columns are named
#'  with a combination of \code{`col_name`} and \code{"_1"}, \code{"_2"}, etc.
#'  E.g. \eqn{".coll_groups_1"}, \eqn{".coll_groups_2"}, ...
#'
#'  \strong{N.B.} When \code{`unique_new_group_cols_only`} is \code{`TRUE`},
#'  we may end up with fewer columns than specified, see \code{`max_iters`}.
#' @param unique_new_group_cols_only Whether to only return unique new group columns.
#'
#'  As the number of column comparisons can be quite time consuming,
#'  we recommend enabling parallelization. See \code{`parallel`}.
#'
#'  \strong{N.B.} We can end up with fewer columns than specified in
#'  \code{`num_new_group_cols`}, see \code{`max_iters`}.
#'
#'  \strong{N.B.} Only used when \code{`num_new_group_cols` > 1}.
#' @param max_iters Maximum number of attempts at reaching
#'  \code{`num_new_group_cols`} \emph{unique} new group columns.
#'
#'  When only keeping unique new group columns, we risk having fewer columns than expected.
#'  Hence, we repeatedly create the missing columns and remove those that are not unique.
#'  This is done until we have \code{`num_new_group_cols`} unique group columns
#'  or we have attempted \code{`max_iters`} times.
#'
#'  In some cases, it is not possible to create \code{`num_new_group_cols`}
#'  unique combinations of the dataset.
#'  \code{`max_iters`} specifies when to stop trying.
#'  Note that we can end up with fewer columns than specified in \code{`num_new_group_cols`}.
#'
#'  \strong{N.B.} Only used when \code{`num_new_group_cols` > 1}.
#' @param combine_method Method to combine the balancing columns by.
#'  One of \code{"avg_standardized"} or \code{"avg_min_max_scaled"}.
#'
#'  For each balancing column (all columns in \emph{`num_cols`}, \emph{`cat_cols`},
#'  and \emph{`id_cols`}, plus \emph{size}), we calculate a normalized, numeric group summary column, which indicates the
#'  "size" of each group in that dimension. These are then combined to a single
#'  \emph{combined balancing column}.
#'
#'  The three steps are:
#'
#'  1) Calculate a numeric representation of the balance for each column.
#'  E.g. the number of unique levels within each group of an ID column
#'  (see \code{Details > Balancing columns} for more on this).
#'
#'  2) Normalize each column separately with standardization (\code{"avg_standardized"}; Default) or MinMax scaling
#'  to the \[0, 1\] range (\code{"avg_min_max_scaled"}).
#'
#'  3) Average the columns \emph{rowwise} to get a single column with one value per group. The averaging
#'  is weighted by \code{`weights`}, which is useful when one of the dimensions is
#'  more important to get a good balance of.
#'
#'  \code{`combine_method`} chooses whether to use standardization or MinMax scaling in step 2.
#' @param weights Named \code{vector} with balancing importance weights for each of
#'  the balancing columns. Besides the columns in \code{`cat_cols`}, \code{`num_cols`}, and \code{`id_cols`},
#'  the \emph{size} balancing weight can be given as \code{"size"}.
#'
#'  The weights are automatically scaled to sum to \code{1}.
#'
#'  Dimensions that are \emph{not} given a weight is automatically given the weight \code{1}.
#'
#'  E.g. \code{c("size" = 1, "cat" = 1, "num1" = 4, "num2" = 7, "id" = 2)}.
#' @param parallel Whether to parallelize the group column comparisons,
#'  when \code{`unique_new_group_cols_only`} is \code{`TRUE`}.
#'
#'  Especially highly recommended when \code{`auto_tune`} is enabled.
#'
#'  Requires a registered parallel backend.
#'  Like \code{doParallel::registerDoParallel}.
#' @family grouping functions
#' @return \code{data.frame} with one or more new grouping factors.
#' @seealso
#'  \code{\link[groupdata2:fold]{fold()}} for creating balanced folds/groups.
#'
#'  \code{\link[groupdata2:partition]{partition()}} for creating balanced partitions.
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
#'
#'
#'
#'
collapse_groups <- function(
  data,
  n,
  group_cols,
  cat_cols = NULL,
  cat_levels = NULL,
  num_cols = NULL,
  id_cols = NULL,
  balance_size = TRUE,
  auto_tune = FALSE,
  weights = NULL,
  method = "balance",
  group_aggregation_fn = mean, # TODO How should this affect autotuning?
  num_new_group_cols = 1,
  unique_new_group_cols_only = TRUE,
  max_iters = 5,
  extreme_pairing_levels = 1,
  combine_method = "avg_standardized",
  col_name = ".coll_groups",
  parallel = FALSE) {

  # Check arguments
  # Some arguments go directly to fold()
  # so they will be checked there
  check_collapse_groups_(
    data = data,
    n = n,
    group_cols = group_cols,
    balance_size = balance_size,
    cat_cols = cat_cols,
    cat_levels = cat_levels,
    num_cols = num_cols,
    group_aggregation_fn = group_aggregation_fn,
    id_cols = id_cols,
    auto_tune = auto_tune,
    method = method,
    num_new_group_cols = num_new_group_cols,
    combine_method = combine_method,
    weights = weights,
    col_name = col_name
  )

  #### Prepare data and names ####

  # Prepare for collapsing
  # Includes renaming columns with ".folds" or
  # other forbidden patterns in their name
  # and checking `data` isn't grouped by any `group_cols`
  prepped <- prepare_collapse_groups_run_(
    data = data,
    group_cols = group_cols,
    cat_cols = cat_cols,
    num_cols = num_cols,
    id_cols = id_cols,
    weights = weights,
    balance_size = balance_size
  )
  data <- prepped[["data"]]
  data_group_cols <- prepped[["data_group_cols"]]
  group_cols <- prepped[["group_cols"]]
  cat_cols <- prepped[["cat_cols"]]
  num_cols <- prepped[["num_cols"]]
  id_cols <- prepped[["id_cols"]]
  weights <- prepped[["weights"]]

  # Collapse groups within each group subset in `data`
  # NOTE: The `data_group_cols` groups, not the `group_cols` groups
  data <- run_by_group_df(
    data = data,
    .fn = run_collapse_groups_,
    n = n,
    group_cols = group_cols,
    balance_size = balance_size,
    cat_cols = cat_cols,
    cat_levels = cat_levels,
    num_cols = num_cols,
    group_aggregation_fn = group_aggregation_fn,
    id_cols = id_cols,
    auto_tune = auto_tune,
    method = method,
    num_new_group_cols = num_new_group_cols,
    unique_new_group_cols_only = unique_new_group_cols_only,
    max_iters = max_iters,
    extreme_pairing_levels = extreme_pairing_levels,
    combine_method = combine_method,
    weights = weights,
    col_name = col_name,
    parallel = parallel
  )

  # Prepare data for return
  data <- prepare_collapse_groups_output_(
    data = data,
    data_group_cols = data_group_cols,
    group_cols = group_cols,
    col_name = col_name,
    num_new_group_cols = num_new_group_cols
  )

  data

}

prepare_collapse_groups_run_ <- function(
  data,
  group_cols,
  cat_cols = NULL,
  num_cols = NULL,
  id_cols = NULL,
  weights = NULL,
  balance_size = FALSE) {

  # Check grouping of `data`
  data_group_cols <- character()
  if (dplyr::is_grouped_df(data)){
    data_group_cols <- dplyr::group_vars(data)
    if (length(intersect(data_group_cols, group_cols))>0) {
      stop("`data` was grouped by a column from `group_cols`.")
    }
  }

  # If `data` contains a fold column
  # We need to rename it temporarily
  updated <- replace_forbidden_names_(
    data = data,
    data_group_cols = data_group_cols,
    group_cols = group_cols,
    cat_cols = cat_cols,
    num_cols = num_cols,
    id_cols = id_cols,
    weights = weights
  )
  data <- updated[["data"]]
  data_group_cols <- updated[["data_group_cols"]]
  group_cols <- updated[["group_cols"]]
  cat_cols <- updated[["cat_cols"]]
  num_cols <- updated[["num_cols"]]
  id_cols <- updated[["id_cols"]]
  weights <- updated[["weights"]]

  # Make sure weights include all cols

  # Whether to include "size" column
  size_col <- NULL
  if (isTRUE(balance_size)){
    size_col <- "size"
  }

  # Add missing weights
  all_balance_cols <- c(cat_cols, num_cols, id_cols, size_col)
  if (is.null(weights) || length(weights) < length(all_balance_cols)){
    to_add <- setdiff(all_balance_cols, names(weights))
    new_weights <- rep(1, times = length(to_add)) %>%
      setNames(to_add)
    weights <- c(weights, new_weights)
  }
  if (length(weights) > 0){
    weights <- weights[order(names(weights))]
  }

  list(
    "data" = data,
    "data_group_cols" = data_group_cols,
    "group_cols" = group_cols,
    "cat_cols" = cat_cols,
    "num_cols" = num_cols,
    "id_cols" = id_cols,
    "weights" = weights
  )
}

prepare_collapse_groups_output_ <- function(
  data,
  data_group_cols,
  group_cols,
  col_name,
  num_new_group_cols,
  replaced_fold_name) {

  # If `data` contained a fold column
  # or other forbidden column name,
  # We need to invert the renaming
  updated <- replace_forbidden_names_(
    data = data,
    data_group_cols = data_group_cols,
    group_cols = group_cols,
    invert = TRUE
  )
  data <- updated[["data"]]

  data
}

run_collapse_groups_ <- function(
  data,
  n,
  group_cols,
  balance_size,
  cat_cols,
  cat_levels,
  num_cols,
  group_aggregation_fn,
  id_cols,
  auto_tune,
  method,
  num_new_group_cols,
  unique_new_group_cols_only,
  max_iters,
  extreme_pairing_levels,
  combine_method,
  weights,
  col_name,
  parallel
) {

  # Create unique old groups factor
  tmp_old_group_var <- create_tmp_var(data = data, tmp_var = ".old_group")
  data <- data %>%
    dplyr::group_by(!!!rlang::syms(group_cols)) %>%
    dplyr::mutate(!!tmp_old_group_var := dplyr::cur_group_id()) %>%
    dplyr::ungroup()

  if (any(max(data[[tmp_old_group_var]]) < n)){
    stop(
      paste0(
        "`data` subset had fewer `group_cols` groups (",
        max(data[[tmp_old_group_var]]),
        ") than `n` (",
        min(n),
        "). ",
        "If `data` was originally grouped, the `group_cols` within each of those subsets must ",
        "contain `>= n` groups to collapse."
      )
    )
  } else if (all(max(data[[tmp_old_group_var]]) == n) &&
             method == "balance"){
    # If `n` is such that each group becomes its own group
    # we simply fold it randomly without anything else
    # This is supported in case it is used programmatically
    # and, say, a sub group in the original `data`
    # has this number of groups
    new_groups <- dplyr::tibble(
      !!tmp_old_group_var := unique(data[[tmp_old_group_var]])
    ) %>%
      fold(
        k = n,
        num_fold_cols = num_new_group_cols,
        unique_fold_cols_only = unique_new_group_cols_only,
        max_iters = max_iters,
        parallel = parallel
      )

    # Replace .folds with the col_name
    # By doing it this way, it works with multiple fold columns
    colnames(new_groups) <- gsub(pattern = ".folds",
                                 replacement = col_name,
                                 x = colnames(new_groups))

    # Add the groups to `data` and remove tmp column
    data <- add_new_groups_(
      data = data,
      new_groups = new_groups,
      tmp_old_group_var = tmp_old_group_var,
      col_name = col_name
    )

    return(data)
  }

  #### Calculate summary info ####

  cat_summary <- NULL
  if (!is.null(cat_cols)){
    cat_summary <- purrr::map(.x = cat_cols, .f = ~ {
      create_combined_cat_summary_(
        data = data,
        group_cols = tmp_old_group_var,
        cat_col = .x,
        cat_levels = cat_levels
      )
    }) %>%
      purrr::reduce(dplyr::full_join, by = tmp_old_group_var)
  }

  num_summary <- NULL
  if (!is.null(num_cols)){

    num_summary <- data %>%
      dplyr::group_by(!!as.name(tmp_old_group_var)) %>%
      dplyr::summarise(
        dplyr::across(dplyr::one_of(num_cols), group_aggregation_fn),
        .groups = "drop"
      )

    num_summ_cols <- colnames(num_summary)[colnames(num_summary) != tmp_old_group_var]
  }

  size_summary <- NULL
  if (isTRUE(balance_size)){
    size_summary <- data %>%
      dplyr::group_by(!!as.name(tmp_old_group_var)) %>%
      dplyr::summarise(size = dplyr::n(), .groups = "drop")
  }

  id_summary <- NULL
  if (!is.null(id_cols)){
    id_summary <- data %>%
      dplyr::group_by(!!as.name(tmp_old_group_var)) %>%
      dplyr::summarize(
        dplyr::across(dplyr::one_of(id_cols),
                      function(x) {
                        length(unique(x))
                      }), .groups = "drop")
  }

  # Prepare summaries tibble
  summaries <- dplyr::tibble(
    !!tmp_old_group_var := unique(data[[tmp_old_group_var]])
  )

  if (!is.null(cat_summary)){
    summaries <- summaries %>%
      dplyr::left_join(cat_summary, by = tmp_old_group_var)

    # In case of NAs, set them to 0
    summaries[, cat_cols][is.na(summaries[, cat_cols])] <- 0
  }
  if (!is.null(num_summary)){
    summaries <- summaries %>%
      dplyr::left_join(num_summary, by = tmp_old_group_var)
  }
  if (!is.null(size_summary)){
    summaries <- summaries %>%
      dplyr::left_join(size_summary, by = tmp_old_group_var)
  }
  if (!is.null(id_summary)){
    summaries <- summaries %>%
      dplyr::left_join(id_summary, by = tmp_old_group_var)
  }

  # Weighted combination of scaled summary columns

  # Assign scaling function
  scale_fn <- list(
    "avg_standardized" = standardize_,
    "avg_min_max_scaled" = function(x) {
      rearrr::min_max_scale(x = x, new_min = 0, new_max = 1)
    }
  )[[combine_method]]

  if (isTRUE(auto_tune)){
    data <- auto_tune_collapsings(
      data = data,
      summaries = summaries,
      n = n,
      tmp_old_group_var = tmp_old_group_var,
      num_cols = num_cols,
      cat_cols = cat_cols,
      id_cols = id_cols,
      balance_size = balance_size,
      weights = weights,
      scale_fn = scale_fn,
      extreme_pairing_levels = extreme_pairing_levels,
      num_new_group_cols = num_new_group_cols,
      unique_new_group_cols_only = unique_new_group_cols_only,
      max_iters = max_iters,
      col_name = col_name,
      parallel = parallel
    )

  } else {

    # Scale, weight and combine
    summaries <- combine_scaled_cols_(
      summaries = summaries,
      weights = weights,
      group_cols = tmp_old_group_var,
      scale_fn = scale_fn
    ) %>%
      dplyr::select(dplyr::one_of(tmp_old_group_var, "combined"))

    if (method == "balance"){
      # Fold the summary
      new_groups <- summaries %>%
        fold(
          k = n,
          num_col = "combined",
          extreme_pairing_levels = extreme_pairing_levels,
          num_fold_cols = num_new_group_cols,
          unique_fold_cols_only =  unique_new_group_cols_only,
          max_iters = max_iters,
          parallel = parallel
        )

      # Replace .folds with the col_name
      # By doing it this way, it works with multiple fold columns
      colnames(new_groups) <- gsub(pattern = ".folds",
                                   replacement = col_name,
                                   x = colnames(new_groups))

      # Add the groups to `data` and remove tmp column
      data <- add_new_groups_(
        data = data,
        new_groups = new_groups,
        tmp_old_group_var = tmp_old_group_var,
        col_name = col_name
      )

    } else {
      data <- add_ordered_summary_groups_(
        data = data,
        summary = summaries,
        n = n,
        group_cols = tmp_old_group_var,
        num_col = "combined",
        method = method,
        col_name = col_name
      ) %>%
        dplyr::select(-dplyr::one_of(tmp_old_group_var))
    }

  }

  data
}

add_new_groups_ <- function(data, new_groups, tmp_old_group_var, col_name){
  # Select the relevant columns
  new_groups <- new_groups %>%
    dplyr::select(
      dplyr::one_of(tmp_old_group_var),
      dplyr::starts_with(col_name),
      # May be called "num" or something and return a wrong column with `starts_with`
      -dplyr::any_of(c("combined", "size"))
    )

  # Add new groups
  data <- data %>%
    dplyr::left_join(new_groups, by = tmp_old_group_var)

  # Remove tmp column
  data[[tmp_old_group_var]] <- NULL

  data
}

# Replace ".fold" with ".____fold" in names
# before running the collapsing
# to avoid overwriting columns
replace_forbidden_names_ <- function(
  data,
  data_group_cols,
  group_cols,
  cat_cols = NULL,
  num_cols = NULL,
  id_cols = NULL,
  weights = NULL,
  invert = FALSE) {

  forbidden_name_patterns <- c(".fold")
  forbidden_names <- c("n", "combined", "size")

  new_name_patterns <- paste0(".____", forbidden_name_patterns)
  new_names <- paste0(".____", forbidden_names)

  replace_forbidden_ <- function(nms){

    if (is.null(nms)){
      return(NULL)
    }

    for (i in seq_along(forbidden_name_patterns)){
      if (isTRUE(invert)){
        pattern <- new_name_patterns[[i]]
        replacement <- forbidden_name_patterns[[i]]
      } else {
        pattern <- forbidden_name_patterns[[i]]
        replacement <- new_name_patterns[[i]]
      }
      nms <- gsub(pattern = pattern,
                  replacement = replacement,
                  x = nms)
    }
    for (i in seq_along(forbidden_names)){
      if (isTRUE(invert)){
        pattern <- paste0("^", new_names[[i]], "$")
        replacement <- forbidden_names[[i]]
      } else {
        pattern <- paste0("^", forbidden_names[[i]], "$")
        replacement <- new_names[[i]]
      }
      nms <- gsub(pattern = pattern,
                  replacement = replacement,
                  x = nms)
    }
    nms
  }

  # Apply replacement
  colnames(data) <- replace_forbidden_(colnames(data))
  data_group_cols <- replace_forbidden_(data_group_cols)
  group_cols <- replace_forbidden_(group_cols)
  cat_cols <- replace_forbidden_(cat_cols)
  num_cols <- replace_forbidden_(num_cols)
  id_cols <- replace_forbidden_(id_cols)
  names(weights) <- replace_forbidden_(names(weights))

  list(
    data = data,
    data_group_cols = data_group_cols,
    group_cols = group_cols,
    cat_cols = cat_cols,
    num_cols = num_cols,
    id_cols = id_cols,
    weights = weights
  )
}

create_combined_cat_summary_ <- function(data, group_cols, cat_col, cat_levels){
  if (is.list(cat_levels)){
    cat_levels <- cat_levels[[cat_col]]
  }

  cat_summary <- NULL
  if (!is.null(cat_col)){
    cat_summary <- data %>%
      dplyr::count(!!!rlang::syms(c(group_cols, cat_col)))

    if (is.null(cat_levels)){
      cat_levels <- levels(data[[cat_col]])
    }

    if (length(cat_levels) == 1 && cat_levels %in% c(".majority", ".minority")){
      slice_fn <- ifelse(cat_levels == ".majority", which.max, which.min)
      cat_levels <- data %>%
        # TODO time whether summarize with the existing counts is faster
        dplyr::count(!!as.name(cat_col)) %>%
        dplyr::slice(slice_fn(n)) %>%
        dplyr::pull(!!as.name(cat_col))
    }

    # Convert to always be a named numeric vector (weights)
    if (!is.numeric(cat_levels)) {
      cat_levels <- rep(1, times = length(cat_levels)) %>%
        setNames(nm = cat_levels)
    }

    cat_summary <- cat_summary %>%
      dplyr::filter(!!as.name(cat_col) %in% names(cat_levels)) %>%
      tidyr::spread(key = !!as.name(cat_col),
                    value = .data$n,
                    fill = 0)

    # Scale, weight and combine the `cat_levels` columns
    cat_summary <- scale_combine_cols_(
      summary = cat_summary,
      weights = cat_levels,
      scale_fn = standardize_,
      col_name = cat_col
    ) %>%
      dplyr::select(dplyr::one_of(group_cols, cat_col))

    if (sd(cat_summary[[cat_col]]) == 0){
      warning(simpleWarning(
        paste0(
          "Combining the standardized level counts",
          " for the `cat_cols` column '",
          cat_col,
          "' led to a zero-variance vector. ",
          "Consider not balancing this column or change the included `cat_levels`."
        ),
        call = if (p <- sys.parent(4 + 1))
          sys.call(p)
      ))
    }
  }

  cat_summary
}

scale_combine_cols_ <- function(summary, weights, scale_fn, col_name){

  # Order weights by their names
  weights <- weights[order(names(weights))]
  # Scale weights to sum to 1
  weights <- weights / sum(weights)
  cat_level_cols <- summary %>%
    # Select the cat_levels columns in the same order as weights
    base_select(cols = names(weights)) %>%
    # Scale all selected columns
    dplyr::mutate(dplyr::across(dplyr::everything(), scale_fn))

  # Multiply by weight
  cat_level_cols <- sweep(
    x = cat_level_cols,
    MARGIN = 2,
    STATS = weights,
    FUN = "*"
  ) %>%
    dplyr::as_tibble()

  # Combine with row sums
  summary[[col_name]] <- rowSums(cat_level_cols, na.rm = TRUE)

  summary
}

# Standardize/normalize, weight and combine summary columns
combine_scaled_cols_ <- function(summaries, weights, group_cols, scale_fn = standardize_){

  # When there are no balancing columns
  # we can't combine them :)
  if (ncol(summaries) - length(group_cols) == 0){
    summaries[["combined"]] <- 1
    return(summaries)
  }

  # The column names of interest
  cols <- colnames(summaries)[colnames(summaries) %ni% group_cols]

  # Create weights if not specified
  if (is.null(weights)){
    weights <- rep(1, times = length(cols)) %>%
      setNames(nm = cols)
  }

  # Calculate combined column
  summaries[["combined"]] <- summaries %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), scale_fn)) %>%
    purrr::pmap_dbl(.f = weighted_mean_, weights = weights)

  summaries
}


# When method is ascending or descending
add_ordered_summary_groups_ <- function(data, summary, n, group_cols, num_col, method, col_name){
  order_fn <- identity
  if (method == "descending"){
    order_fn <- dplyr::desc
  }

  # Order and group
  new_groups <- summary %>%
    dplyr::arrange(order_fn(!!as.name(num_col))) %>%
    group(
      n = n,
      method = "n_fill",
      col_name = col_name,
      descending = method == "ascending"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::one_of(num_col))

  # Add new groups
  data <- data %>%
    dplyr::left_join(new_groups, by = group_cols)

  data
}


### . . . . . . . . .. #< d389058924b6716ba075731944149594 ># . . . . . . . . ..
### Check arguments                                                         ####


check_collapse_groups_ <- function(
  data,
  n,
  group_cols,
  balance_size,
  cat_cols,
  cat_levels,
  num_cols,
  group_aggregation_fn,
  id_cols,
  auto_tune,
  method,
  num_new_group_cols, # Also checked in fold()
  combine_method,
  weights,
  col_name
){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data,
                               min.rows = 1,
                               add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_numeric(
    x = n,
    lower = 1,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_numeric(
    x = weights,
    finite = TRUE,
    any.missing = FALSE,
    lower = 0,
    max.len = length(c(cat_cols, num_cols, id_cols)) + 1, # + size
    names = "unique",
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_number(
    x = num_new_group_cols,
    finite = TRUE,
    lower = 1,
    add = assert_collection
  )
  checkmate::assert_flag(x = balance_size,
                         add = assert_collection)
  checkmate::assert_character(
    x = group_cols,
    min.len = 1,
    min.chars = 1,
    any.missing = FALSE,
    unique = TRUE,
    names = "unnamed",
    add = assert_collection
  )
  checkmate::assert_character(
    x = cat_cols,
    min.chars = 1,
    null.ok = TRUE,
    any.missing = FALSE,
    unique = TRUE,
    add = assert_collection
  )
  checkmate::assert_string(
    x = method,
    min.chars = 1,
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # Either the name of the levels
  # or a named vector with weights for each given level
  checkmate::assert(
    checkmate::check_character(
      x = cat_levels,
      min.chars = 1,
      any.missing = FALSE,
      unique = TRUE,
      null.ok = TRUE,
      names = "unnamed"
    ),
    checkmate::check_numeric(
      x = cat_levels,
      names = "unique",
      lower = 0,
      finite = TRUE,
      any.missing = FALSE,
      null.ok = TRUE
    ),
    checkmate::check_list(
      x = cat_levels,
      names = "unique",
      types = c("character", "numeric"),
      any.missing = FALSE,
      min.len = 1,
      max.len = length(cat_cols),
      null.ok = TRUE
    ),
    .var.name = "cat_levels"
  )

  checkmate::assert_character(
    x = num_cols,
    any.missing = FALSE,
    unique = TRUE,
    min.chars = 1,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_character(
    x = id_cols,
    any.missing = FALSE,
    min.chars = 1,
    unique = TRUE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_flag(x = auto_tune,
                         add = assert_collection)
  checkmate::assert_string(x = combine_method,
                           min.chars = 1,
                           add = assert_collection)
  checkmate::assert_string(x = col_name,
                           min.chars = 1,
                           add = assert_collection)
  checkmate::assert_function(x = group_aggregation_fn, add = assert_collection)

  checkmate::reportAssertions(assert_collection)

  # Handle existing group columns with same name
  if (col_name %in% colnames(data)){
    assert_collection$push("`col_name` is already a column in `data`.")
  } else if (any(grepl(col_name, colnames(data), fixed = TRUE))){
    assert_collection$push("`data` already contains a column including `col_name`.")
  }

  checkmate::assert_names(
    x = colnames(data),
    must.include = c(group_cols, cat_cols, num_cols, id_cols),
    type = "unique",
    add = assert_collection
  )
  checkmate::assert_names(
    x = method,
    subset.of = c("balance", "ascending", "descending"),
    add = assert_collection
  )
  checkmate::assert_names(
    x = combine_method,
    subset.of = c("avg_standardized", "avg_min_max_scaled"),
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  if (method != "balance"){
    if (isTRUE(auto_tune)){
      assert_collection$push("when `method` != 'balance', `auto_tune` must be disabled.")
    }
    if (num_new_group_cols > 1){
      assert_collection$push("when `method` != 'balance', `num_new_group_cols` must be `1`.")
    }
    checkmate::reportAssertions(assert_collection)
  }
  if (!is.null(weights)){
    checkmate::assert_names(
      x = names(weights),
      subset.of = c("size", cat_cols, num_cols, id_cols),
      add = assert_collection
    )
  }

  if (!is.null(cat_cols)){
    for (cat_col in cat_cols) {
      checkmate::assert_factor(
        x = data[[cat_col]],
        any.missing = FALSE,
        min.levels = 2,
        .var.name = paste0("data[['", cat_col, "']]"),
        add = assert_collection
      )
    }
    checkmate::reportAssertions(assert_collection)

    if (!is.null(cat_levels)){
      check_cat_levels_(data = data,
                        cat_cols = cat_cols,
                        cat_levels = cat_levels)
    }
  }

  if (!is.null(num_cols)) {
    for (num_col in num_cols) {
      checkmate::assert_numeric(
        x = data[[num_col]],
        any.missing = FALSE,
        finite = TRUE,
        .var.name = paste0("data[['", num_col, "']]"),
        add = assert_collection
      )
    }
  }
  if (!is.null(id_cols)){
    for (id_col in id_cols) {
      checkmate::assert_factor(
        x = data[[id_col]],
        any.missing = FALSE,
        .var.name = paste0("data[['", id_col, "']]"),
        add = assert_collection
      )
    }
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####
}

check_cat_levels_ <- function(data, cat_cols, cat_levels) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  # checkmate::assert_ , add = assert_collection)

  if (is.list(cat_levels)) {
    if (!setequal(names(cat_levels), cat_cols)) {
      assert_collection$push("when `cat_levels` is a list, its names must be equal to `cat_cols`.")
    }
    for (cat_col in cat_cols) {
      check_single_cat_levels_(data = data,
                               cat_col = cat_col,
                               cat_levels = cat_levels[[cat_col]])
    }
  } else {
    # When not a list, the same cat_levels are used for all cat_cols
    # E.g. a character with ".minority"
    for (cat_col in cat_cols) {
      check_single_cat_levels_(data = data,
                               cat_col = cat_col,
                               cat_levels = cat_levels)
    }
    checkmate::reportAssertions(assert_collection)
    # End of argument checks ####
  }
}

check_single_cat_levels_ <- function(data, cat_col, cat_levels) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  if (is.list(cat_levels)) {
    assert_collection$push(
      paste0(
        "when `cat_levels` is a list, elements must have type",
        " `character` or `numeric`."
      )
    )
  }
  if (!is.character(cat_levels)) {
    cat_levels_names <- names(cat_levels)
  } else {
    cat_levels_names <- cat_levels
  }
  checkmate::assert_names(
    x = cat_levels_names,
    subset.of = c(".minority",
                  ".majority",
                  levels(data[[cat_col]])),
    type = "unique",
    add = assert_collection,
    .var.name = paste0("`cat_levels` for ", cat_col)
  )
  if ((".minority" %in% cat_levels_names ||
       ".majority" %in% cat_levels_names) &&
      (length(cat_levels_names) > 1 ||
       !is.character(cat_levels))) {
    assert_collection$push(
      paste0(
        "when '.minority' or '.majority' is in `cat_levels` (for ",
        cat_col,
        "), ",
        "it must be the only level."
      )
    )
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####
}
