
## Collapse groups

# TODO could you standardize freqs of each class and combine them
# and get *some* balancing of all classes? Seems the class columns
# should be able to cancel each other out and so there are
# at least some scenarios where it wouldn't work?

# TODO handle existing .coll_group* columns!

# TODO When there's any balancing we can often only create 1 fold column
# so is it worth having the num_***_cols arg? Of course people
# might not always want balancing? And with different n's, they will
# be different!

#' @title Collapse groups with categorical, numerical, and size balancing
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Collapses a set of groups into a smaller set of groups.
#'
#'  Balance the new groups by a numerical column,
#'  one or more levels of a categorical column,
#'  and/or the number of rows (size).
#'  Note: The more of these you balance at a time,
#'  the less balanced each of them may become.
#'
#'  Can create multiple unique collapsed group columns.
#' @details
#'  \subsection{cat_col}{
#'    \enumerate{
#'      \item ...
#'    }
#'  }
#'
#'  \subsection{num_col}{
#'    \enumerate{
#'      \item ...
#'    }
#'  }
#'
#'  \subsection{size}{
#'    \enumerate{
#'      \item ...
#'    }
#'  }
#'
#'  \subsection{combined}{
#'    \enumerate{
#'      \item ...
#'    }
#'  }
#'
#'  \subsection{numerical balancing of combined column}{
#'    TODO THIS ONE!
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
#'    N.B. When doing extreme pairing of an unequal number of rows,
#'    the row with the smallest value is placed in a group by itself, and the order is instead:
#'    smallest, \emph{second smallest, largest, third smallest, second largest}, etc.
#'  }
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
#' @param cat_col Name of categorical column to balance the average frequency
#'  of a chosen level of between groups.
#'
#'  Note: I have not yet identified a way to quickly and robustly balance frequencies of
#'  multiple levels at once. This may change in the future.
#' @param cat_levels Names of the levels in the \code{`cat_col`} column to balance the average frequency
#'  of. When \code{NULL}, all levels are balanced. The balancing is likely to work best with a few levels.
#'
#'  Can be a named numeric vector, where the names are the levels and the values are weights
#'  of importance in the balancing.
#'
#'  Can be \code{".minority"} or \code{".majority"}, in which cases the minority/majority level
#'  are found and used.
#' @param num_col Name of numerical column to balance between groups.
#'
#'  Aggregated for each group using \code{`group_aggregation_fn`} before balancing.
#' @param group_cols Name(s) of factor(s) in \code{`data`} for identifying the existing groups
#'  that should be collapsed.
#'
#'  Multiple names are treated as in \code{\link[dplyr:group_by]{dplyr::group_by()}}
#'  (i.e., a hierarchy of groups), where each leaf group within each parent group is
#'  considered a unique group to be collapsed.
#'  Parent groups are not considered during collapsing, why leaf groups from different
#'  parent groups can be collapsed together.
#'
#'  N.B. Do not confuse these group columns with potential columns that \code{`data`} is grouped by.
#'  \code{`group_cols`} identifies the groups to be collapsed. When \code{`data`} is
#'  grouped with \code{\link[dplyr:group_by]{dplyr::group_by()}}, the function is
#'  applied separately to each subset.
#'      TODO Improve this part!!!!
#' @param group_aggregation_fn Function for aggregating values in \code{`num_col`}
#'  for each group in \code{`group_cols`}, before balancing \code{`num_col`}.
#'
#'  N.B. Only used when \code{`num_col`} is specified.
#' @param extreme_pairing_levels How many levels of extreme pairing to do
#'  when balancing the groups by the combined balancing column. (TODO improve)
#'
#'  \strong{Extreme pairing}: Rows/pairs are ordered as smallest, largest,
#'  second smallest, second largest, etc. If \code{extreme_pairing_levels > 1},
#'  this is done "recursively" on the extreme pairs. See \code{`Details/TODODODODO`} for more.
#'
#'  N.B. Larger values work best with large datasets. If set too high,
#'  the result might not be stochastic. Always check if an increase
#'  actually makes the groups more balanced. See example.
#' @param num_new_group_cols Number of group columns to create.
#'
#'  If \code{num_new_group_cols > 1}, columns will be named
#'  with a combination of \code{`col_name`} and \code{"_1"}, \code{"_2"}, etc.
#'  E.g. \eqn{".coll_groups_1"}, \eqn{".coll_groups_2"}, etc.
#'
#'  N.B. If \code{`unique_new_group_cols_only`} is \code{TRUE},
#'  we can end up with fewer columns than specified, see \code{`max_iters`}.
#'  E.g. when using balancing, the max. possible number of unique
#'  collapsings is often \code{1}.
#' @param unique_new_group_cols_only Check if fold columns are identical and
#'  keep only unique columns.
#'
#'  As the number of column comparisons can be time consuming,
#'  we can run this part in parallel. See \code{`parallel`}.
#'
#'  N.B. We can end up with fewer columns than specified in
#'  \code{`num_new_group_cols`}, see \code{`max_iters`}.
#'
#'  N.B. Only used when \code{`num_new_group_cols` > 1} or (TODODODO: \code{`data`} has existing group columns.)
#' @param max_iters Maximum number of attempts at reaching
#'  \code{`num_new_group_cols`} \emph{unique} new group columns.
#'
#'  When only keeping unique new group columns, we risk having fewer columns than expected.
#'  Hence, we repeatedly create the missing columns and remove those that are not unique.
#'  This is done until we have \code{`num_new_group_cols`} unique group columns
#'  or we have attempted \code{`max_iters`} times.
#'  In some cases, it is not possible to create \code{`num_new_group_cols`}
#'  unique combinations of the dataset.
#'  \code{`max_iters`} specifies when to stop trying.
#'  Note that we can end up with fewer columns than specified in \code{`num_new_group_cols`}.
#'
#'  N.B. Only used when \code{`num_new_group_cols` > 1}.
#' @param combine_method Method to combine the balancing dimensions by. TODO
#'
#'  \code{"avg_standardized"} or \code{"avg_min_max_scaled"}.
#'
#' @param combine_weights Named vector with weights for each of
#'  the balancing dimensions. Can be used to favor balancing of
#'  either size, categorical, or numerical balancing.
#' @param parallel Whether to parallelize the group column comparisons,
#'  when \code{`unique_new_group_cols_only`} is \code{TRUE}.
#'
#'  Requires a registered parallel backend.
#'  Like \code{doParallel::registerDoParallel}.
#'
#' @family grouping functions
#' @return \code{data.frame} with grouping factor for subsetting in cross-validation.
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
collapse_groups <- function(
  data,
  n,
  group_cols,
  balance_size = TRUE,
  cat_col = NULL,
  cat_levels = ".majority",
  num_col = NULL,
  group_aggregation_fn = mean,
  extreme_pairing_levels = 1,
  num_new_group_cols = 1,
  unique_new_group_cols_only = TRUE,
  max_iters = 5,
  combine_method = "avg_standardized",
  combine_weights = c("size" = 1, "cat" = 1, "num" = 1),
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
    cat_col = cat_col,
    cat_levels = cat_levels,
    num_col = num_col,
    group_aggregation_fn = group_aggregation_fn,
    combine_method = combine_method,
    combine_weights = combine_weights,
    col_name = col_name
  )

  #### Prepare data and names ####

  data_group_cols <- character()
  if (dplyr::is_grouped_df(data)){
    data_group_cols <- dplyr::group_vars(data)
    if (length(intersect(data_group_cols, group_cols))>0) {
      stop("`data` was grouped by a column from `group_cols`.")
    }
  }

  # If `data` contains a fold column
  # We need to rename it temporarily
  if (replaced_fold_name <- any(grepl(".fold", colnames(data)))) {
    updated <- replace_fold_in_names_(
      data = data,
      data_group_cols = data_group_cols,
      group_cols = group_cols
    )
    data <- updated[["data"]]
    data_group_cols <- updated[["data_group_cols"]]
    group_cols <- updated[["group_cols"]]
  }

  # Collapse groups within each group subset in `data`
  # NOTE: The `data_group_cols` groups, not the `group_cols` groups
  data <- run_by_group_df(
    data = data,
    .fn = run_collapse_groups_,
    n = n,
    group_cols = group_cols,
    balance_size = balance_size,
    cat_col = cat_col,
    cat_levels = cat_levels,
    num_col = num_col,
    group_aggregation_fn = group_aggregation_fn,
    extreme_pairing_levels = extreme_pairing_levels,
    num_new_group_cols = num_new_group_cols,
    unique_new_group_cols_only = unique_new_group_cols_only,
    max_iters = max_iters,
    combine_method = combine_method,
    combine_weights = combine_weights,
    col_name = col_name,
    parallel = parallel
  )

  # If `data` contained a fold column
  # We need to invert the renaming
  if (isTRUE(replaced_fold_name)) {
    updated <- replace_fold_in_names_(
      data = data,
      data_group_cols = data_group_cols,
      group_cols = group_cols,
      invert = TRUE
    )
    data <- updated[["data"]]
    data_group_cols <- updated[["data_group_cols"]]
    group_cols <- updated[["group_cols"]]
  }

  if (num_new_group_cols == 1){
    # Group by the new groups
    data <- data %>%
      dplyr::group_by(!!!rlang::syms(c(data_group_cols, col_name)))
  }

  data

}

run_collapse_groups_ <- function(
  data,
  n,
  group_cols,
  balance_size,
  cat_col,
  cat_levels,
  num_col,
  group_aggregation_fn,
  extreme_pairing_levels,
  num_new_group_cols,
  unique_new_group_cols_only,
  max_iters,
  combine_method,
  combine_weights,
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
  } else if (all(max(data[[tmp_old_group_var]]) == n)){
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

    return(data)
  }

  #### Calculate summary info ####

  cat_summary <- NULL
  if (!is.null(cat_col)){
    cat_summary <- data %>%
      dplyr::count(!!!rlang::syms(c(tmp_old_group_var, cat_col)))

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
    cat_summary <- combined_scaled_cat_level_cols(
      cat_summary = cat_summary,
      weights = cat_levels,
      scale_fn = standardize # TODO make specifiable?
    ) %>%
      dplyr::select(dplyr::one_of(tmp_old_group_var, "cat_levels_combined"))

    if (all(cat_summary[["cat_levels_combined"]] == 0)){
      stop(paste0(
        "The `cat_levels` '",
        names(cat_levels),
        ", was not found in any of the groups."
      ))
    }
  }

  num_summary <- NULL
  if (!is.null(num_col)){
    num_summary <- data %>%
      dplyr::group_by(!!as.name(tmp_old_group_var)) %>%
      dplyr::summarise(num_aggr = group_aggregation_fn(!!as.name(num_col)), .groups = "drop")
  }

  size_summary <- NULL
  if (isTRUE(balance_size)){
    size_summary <- data %>%
      dplyr::group_by(!!as.name(tmp_old_group_var)) %>%
      dplyr::summarise(size = dplyr::n(), .groups = "drop")
  }

  # Prepare summaries tibble
  summaries <- dplyr::tibble(
    !!tmp_old_group_var := unique(data[[tmp_old_group_var]])
  )

  if (!is.null(cat_summary)){
    summaries <- summaries %>%
      dplyr::left_join(cat_summary, by = tmp_old_group_var)

    # In case of NAs, set them to
    summaries[["cat_levels_combined"]][is.na(summaries[["cat_levels_combined"]])] <- 0
  }
  if (!is.null(num_summary)){
    summaries <- summaries %>%
      dplyr::left_join(num_summary, by = tmp_old_group_var)
  }
  if (!is.null(size_summary)){
    summaries <- summaries %>%
      dplyr::left_join(size_summary, by = tmp_old_group_var)
  }

  # Weighted combination of scaled summary columns

  # Assign scaling function
  scale_fn <- list(
    "avg_standardized" = standardize,
    "avg_min_max_scaled" = function(x) {
      rearrr::min_max_scale(x = x, new_min = 0, new_max = 1)
    }
  )[[combine_method]]

  # Scale, weight and combine
  summaries <- combine_scaled_cols_(
    summaries = summaries,
    combine_weights = combine_weights,
    include_flags = c("size" = isTRUE(balance_size),
                      "cat" = !is.null(cat_col),
                      "num" = !is.null(num_col)),
    scale_fn = scale_fn
  )

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

  data
}

add_new_groups_ <- function(data, new_groups, tmp_old_group_var, col_name){
  # Select the relevant columns
  new_groups <- new_groups %>%
    dplyr::select(dplyr::one_of(tmp_old_group_var),
                  dplyr::starts_with(col_name))

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
replace_fold_in_names_ <- function(data, data_group_cols, group_cols, invert = FALSE){

  if (isTRUE(invert)){
    pattern <- ".____fold"
    replacement <- ".fold"
  } else {
    pattern <- ".fold"
    replacement <- ".____fold"
  }

  replace_fold <- function(nms) {
    gsub(pattern = pattern,
         replacement = replacement,
         x = nms)
  }

  # Apply replacement
  colnames(data) <- replace_fold(colnames(data))
  data_group_cols <- replace_fold(data_group_cols)
  group_cols <- replace_fold(group_cols)

  list(data = data,
       data_group_cols = data_group_cols,
       group_cols = group_cols)
}

# Center and scale x
standardize <- function(x){
  std <- sd(x)
  if (std == 0) std <- 1
  (x - mean(x)) / std
}

combined_scaled_cat_level_cols <- function(cat_summary, weights, scale_fn = standardize){

  # Order weights by their names
  weights <- weights[order(names(weights))]
  # Scale weights to sum to 1
  weights <- weights / sum(weights)
  cat_level_cols <- cat_summary %>%
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
  cat_summary[["cat_levels_combined"]] <- rowSums(cat_level_cols, na.rm = TRUE)

  cat_summary
}

# Standardize/normalize, weight and combine summary columns
combine_scaled_cols_ <- function(summaries, combine_weights, include_flags, scale_fn = standardize){

  # Normalize weights

  # Reorder to allow masking
  combine_weights <- combine_weights[order(names(combine_weights))]
  include_flags <- include_flags[order(names(include_flags))]

  # Mask by whether the attribute is balanced
  combine_weights <- combine_weights * include_flags
  combine_weights <- combine_weights / sum(combine_weights)

  # Prepare combined column
  summaries[["combined"]] <- 0

  # Standardize each column, multiply with weight and add to combined

  if ("cat_levels_combined" %in% names(summaries)) {
    summaries[["combined"]] <- summaries[["combined"]] + (
      scale_fn(summaries[["cat_levels_combined"]]) * combine_weights[["cat"]]
    )
  }
  if ("num_aggr" %in% names(summaries)) {
    summaries[["combined"]] <- summaries[["combined"]] + (
      scale_fn(summaries[["num_aggr"]]) * combine_weights[["num"]]
    )
  }
  if ("size" %in% names(summaries)) {
    summaries[["combined"]] <- summaries[["combined"]] + (
      scale_fn(summaries[["size"]]) * combine_weights[["size"]]
    )
  }

  summaries
}

check_collapse_groups_ <- function(
  data,
  n,
  group_cols,
  balance_size,
  cat_col,
  cat_levels,
  num_col,
  group_aggregation_fn,
  combine_method,
  combine_weights,
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
    x = combine_weights,
    finite = TRUE,
    any.missing = FALSE,
    len = 3,
    names = "unique",
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
  checkmate::assert_string(
    x = cat_col,
    min.chars = 1,
    null.ok = TRUE,
    add = assert_collection
  )

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
      any.missing = FALSE,
      null.ok = TRUE
    ),
    .var.name = "cat_levels"
  )

  checkmate::assert_string(
    x = num_col,
    na.ok = FALSE,
    min.chars = 1,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_string(x = combine_method,
                           min.chars = 1,
                           add = assert_collection)
  checkmate::assert_string(x = col_name,
                           min.chars = 1,
                           add = assert_collection)
  checkmate::assert_function(x = group_aggregation_fn, add = assert_collection)

  checkmate::reportAssertions(assert_collection)

  if (col_name %in% colnames(data)){
    assert_collection$push("`col_name` is already a column in `data`.")
  }
  checkmate::assert_names(
    x = colnames(data),
    must.include = c(group_cols, cat_col, num_col),
    type = "unique",
    add = assert_collection
  )
  checkmate::assert_names(
    x = combine_method,
    subset.of = c("avg_standardized", "avg_min_max_scaled"),
    add = assert_collection
  )
  checkmate::assert_names(
    x = names(combine_weights),
    permutation.of = c("size", "num", "cat"),
    add = assert_collection
  )

  if (!is.null(cat_col)){
    checkmate::assert_factor(
      x = data[[cat_col]],
      any.missing = FALSE,
      min.levels = 2,
      add = assert_collection
    )
    checkmate::reportAssertions(assert_collection)
    if (!is.null(cat_levels)){
      if (is.numeric(cat_levels))
        cat_levels_names <- names(cat_levels)
      else
        cat_levels_names <- cat_levels
      checkmate::assert_names(
        x = cat_levels_names,
        subset.of = c(".minority",
                      ".majority",
                      levels(data[[cat_col]])),
        type = "unique",
        add = assert_collection,
        .var.name = "cat_levels"
      )
      if ((".minority" %in% cat_levels_names ||
          ".majority" %in% cat_levels_names) &&
          (length(cat_levels_names) > 1 ||
           !is.character(cat_levels))){
        assert_collection$push(
          paste0(
            "when '.minority' or '.majority' is in `cat_levels`, ",
            "it must be the only level."
          )
        )
      }
    }
  }

  if (!is.null(num_col)) {
    checkmate::assert_numeric(
      x = data[[num_col]],
      any.missing = FALSE,
      finite = TRUE,
      add = assert_collection
    )
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

}
