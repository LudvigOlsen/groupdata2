

#   __________________ #< 5ec83ba9168e5ec8da216e162db403c9 ># __________________
#   Collapse groups wrappers                                                ####


##  .................. #< 97b9cf970cbaebba7d4719c38ad7e0bf ># ..................
##  Collapse wrappers documentation                                         ####

#' @title Collapse groups balanced by a single attribute
#' @name collapse_groups_by
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Collapses a set of groups into a smaller set of groups.
#'
#'  Balance the new groups by:
#'
#'   * The \strong{number of rows} with \code{collapse_groups_by_size()}
#'
#'   * A \strong{numerical column} with \code{collapse_groups_by_numeric()}
#'
#'   * One or more levels of a \strong{categorical column} with \code{collapse_groups_by_levels()}
#'
#'   * Level counts in an \strong{ID column} with \code{collapse_groups_by_ids()}
#'
#'   * \strong{Any combination} of these with \code{collapse_groups()}
#'
#'  These functions wrap \code{\link[groupdata2:collapse_groups]{collapse_groups()}}. They
#'  simplify the given tasks and provide additional methods. To balance more than one of
#'  the attributes at a time, use \code{\link[groupdata2:collapse_groups]{collapse_groups()}}
#'  directly.
#'
#'  Tip: Check the balances of the new groups with
#'  \code{\link[groupdata2:summarize_balances]{summarize_balances()}}.
#' @inheritParams collapse_groups
#' @param n Number of new groups.
#' @param method \code{"balance"}, \code{"ascending"}, or \code{"descending"}.
#'
#'  * \code{"balance"} balances the attribute between the groups.
#'  * \code{"ascending"} orders the attribute and groups from the lowest to highest value.
#'  * \code{"descending"} orders the attribute and groups from the highest to lowest value.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @return \code{`data`} with a new grouping factor column,
#'  grouped by the existing grouping plus the new column.
#' @family grouping functions
#' @examples
#' # Attach packages
NULL


##  .................. #< 077f4d486ff8d1e493b6e0d8c6626776 ># ..................
##  Collapse by size                                                        ####


#' @rdname collapse_groups_by
#' @export
collapse_groups_by_size <- function(
  data,
  n,
  group_cols,
  method = "balance", # ascending/descending
  extreme_pairing_levels = 1, # only method==balance
  col_name = ".coll_groups") {

  if (method == "balance") {
    return(
      collapse_groups(
        data = data,
        n = n,
        group_cols = group_cols,
        extreme_pairing_levels = extreme_pairing_levels,
        num_new_group_cols = 1,
        col_name = col_name,
        balance_size = TRUE
      )
    )
  }

  # Check arguments ####
  # Some arguments go directly to fold()
  # so they will be checked there
  check_collapse_groups_by_(
    data = data,
    n = n,
    group_cols = group_cols,
    method = method,
    col_name = col_name
  )

  #### Prepare data and names ####

  # Prepare for collapsing
  # Includes renaming columns with ".folds" in their name
  # and checking `data` isn't grouped by any `group_cols`
  prepped <- prepare_collapse_groups_run_(data = data, group_cols = group_cols)
  data <- prepped[["data"]]
  data_group_cols <- prepped[["data_group_cols"]]
  group_cols <- prepped[["group_cols"]]

  # Collapse groups within each group subset in `data`
  # NOTE: The `data_group_cols` groups, not the `group_cols` groups
  data <- run_by_group_df(
    data = data,
    .fn = run_collapse_groups_by_size_,
    n = n,
    group_cols = group_cols,
    method = method,
    col_name = col_name
  )

  # Prepare data for return
  data <- prepare_collapse_groups_output_(
    data = data,
    data_group_cols = data_group_cols,
    group_cols = group_cols,
    col_name = col_name,
    num_new_group_cols = 1
  )

  data

}

run_collapse_groups_by_size_ <- function(data,
                                         n,
                                         group_cols,
                                         method,
                                         col_name){

  size_summary <- data %>%
    dplyr::group_by(!!!rlang::syms(group_cols)) %>%
    dplyr::summarise(size = dplyr::n(), .groups = "drop")

  # Order summary depending on method
  # Create groups
  # Transfer groups to `data`
  data <- add_ordered_summary_groups_(
    data = data,
    summary = size_summary,
    n = n,
    group_cols = group_cols,
    num_col = "size",
    method = method,
    col_name = col_name
  )

  data
}


##  .................. #< 0bd44ba51e9f181c74193f831f89210d ># ..................
##  Collapse by numeric                                                     ####


#' @rdname collapse_groups_by
#' @export
collapse_groups_by_numeric <- function(
  data,
  n,
  group_cols,
  num_cols,
  method = "balance", # ascending/descending
  group_aggregation_fn = mean,
  extreme_pairing_levels = 1,
  col_name = ".coll_groups") {

  if (method == "balance") {
    # TODO Some arguments are missing here
    return(
      collapse_groups(
        data = data,
        n = n,
        group_cols = group_cols,
        num_cols = num_cols,
        extreme_pairing_levels = extreme_pairing_levels,
        num_new_group_cols = 1,
        group_aggregation_fn = group_aggregation_fn,
        balance_size = FALSE,
        col_name = col_name
      )
    )
  }

  # Check arguments ####
  # Some arguments go directly to fold()
  # so they will be checked there
  check_collapse_groups_by_(
    data = data,
    n = n,
    group_cols = group_cols,
    method = method,
    col_name = col_name
  )
  checkmate::assert_character(
    num_cols,
    min.chars = 1,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE
  )
  checkmate::assert_names(
    colnames(data),
    must.include = num_cols
  )
  for (num_col in num_cols) {
    checkmate::assert_numeric(
      x = data[[num_col]],
      any.missing = FALSE,
      finite = TRUE,
      .var.name = paste0("data[[", num_col, "]]")
    )
  }
  # End of argument checks ####

  # Prepare for collapsing
  # Includes renaming columns with ".folds" in their name
  # and checking `data` isn't grouped by any `group_cols`
  prepped <- prepare_collapse_groups_run_(
    data = data,
    group_cols = group_cols,
    num_cols = num_cols
  )
  data <- prepped[["data"]]
  data_group_cols <- prepped[["data_group_cols"]]
  group_cols <- prepped[["group_cols"]]
  num_cols <- prepped[["num_cols"]]

  # Collapse groups within each group subset in `data`
  # NOTE: The `data_group_cols` groups, not the `group_cols` groups
  data <- run_by_group_df(
    data = data,
    .fn = run_collapse_groups_by_numeric_,
    n = n,
    group_cols = group_cols,
    num_cols = num_cols,
    group_aggregation_fn = group_aggregation_fn,
    method = method,
    col_name = col_name
  )

  # Prepare data for return
  data <- prepare_collapse_groups_output_(
    data = data,
    data_group_cols = data_group_cols,
    group_cols = group_cols,
    col_name = col_name,
    num_new_group_cols = 1
  )

  data

}

run_collapse_groups_by_numeric_ <- function(
  data,
  n,
  group_cols,
  num_cols,
  method,
  group_aggregation_fn,
  col_name) {

  num_summary <- data %>%
    dplyr::group_by(!!!rlang::syms(group_cols)) %>%
    dplyr::summarise(dplyr::across(dplyr::one_of(num_cols), group_aggregation_fn), .groups = "drop")

  # Combine `num_cols` columns
  num_summary[["num_aggr"]] <- num_summary %>%
    base_select(cols = num_cols) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), standardize)) %>%
    purrr::pmap_dbl(.f = function(...){mean(c(...))})

  # Select necessary columns
  num_summary <- num_summary %>%
    dplyr::select(dplyr::one_of(group_cols, "num_aggr"))

  # Order summary depending on method
  # Create groups
  # Transfer groups to `data`
  data <- add_ordered_summary_groups_(
    data = data,
    summary = num_summary,
    n = n,
    group_cols = group_cols,
    num_col = "num_aggr",
    method = method,
    col_name = col_name
  )

  data
}


##  .................. #< 06fd80b0d0291e8a4a1c2960e9e5c443 ># ..................
##  Collapse by factor                                                      ####


#' @rdname collapse_groups_by
#' @export
collapse_groups_by_levels <- function(
  data,
  n,
  group_cols,
  cat_col = NULL,
  cat_levels = NULL,
  method = "balance", # ascending/descending
  extreme_pairing_levels = 1,
  col_name = ".coll_groups") {

  if (method == "balance") {
    # TODO Some arguments are missing here
    return(
      collapse_groups(
        data = data,
        n = n,
        group_cols = group_cols,
        cat_col = cat_col,
        cat_levels = cat_levels,
        extreme_pairing_levels = extreme_pairing_levels,
        num_new_group_cols = 1,
        balance_size = FALSE,
        col_name = col_name
      )
    )
  }

  # Check arguments ####
  # Some arguments go directly to fold()
  # so they will be checked there
  check_collapse_groups_by_(
    data = data,
    n = n,
    group_cols = group_cols,
    method = method,
    col_name = col_name
  )
  checkmate::assert_string(
    cat_col,
    min.chars = 1
  )
  checkmate::assert_names(
    colnames(data),
    must.include = cat_col
  )
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_factor(
    x = data[[cat_col]],
    any.missing = FALSE,
    min.levels = 2,
    add = assert_collection
  )
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
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Prepare for collapsing
  # Includes renaming columns with ".folds" in their name
  # and checking `data` isn't grouped by any `group_cols`
  prepped <- prepare_collapse_groups_run_(
    data = data,
    group_cols = group_cols,
    cat_col = cat_col
  )
  data <- prepped[["data"]]
  data_group_cols <- prepped[["data_group_cols"]]
  group_cols <- prepped[["group_cols"]]
  cat_col <- prepped[["cat_col"]]

  # Collapse groups within each group subset in `data`
  # NOTE: The `data_group_cols` groups, not the `group_cols` groups
  data <- run_by_group_df(
    data = data,
    .fn = run_collapse_groups_by_levels_,
    n = n,
    group_cols = group_cols,
    cat_col = cat_col,
    cat_levels = cat_levels,
    method = method,
    col_name = col_name
  )

  # Prepare data for return
  data <- prepare_collapse_groups_output_(
    data = data,
    data_group_cols = data_group_cols,
    group_cols = group_cols,
    col_name = col_name,
    num_new_group_cols = 1
  )

  data

}

run_collapse_groups_by_levels_ <- function(
  data,
  n,
  group_cols,
  cat_col,
  cat_levels,
  method,
  col_name) {

  # Create summary with combined score
  cat_summary <- create_combined_cat_summary_(
    data = data,
    group_cols = group_cols,
    cat_col = cat_col,
    cat_levels = cat_levels
  )

  # Order summary depending on method
  # Create groups
  # Transfer groups to `data`
  data <- add_ordered_summary_groups_(
    data = data,
    summary = cat_summary,
    n = n,
    group_cols = group_cols,
    num_col = "cat_levels_combined",
    method = method,
    col_name = col_name
  )

  data
}


##  .................. #< 99d1063b7b96a07a92a8f94b2ecbf7c1 ># ..................
##  Collapse by ID                                                          ####

#' @rdname collapse_groups_by
#' @export
collapse_groups_by_ids <- function(
  data,
  n,
  group_cols,
  id_col,
  method = "balance", # ascending/descending
  extreme_pairing_levels = 1,
  col_name = ".coll_groups") {

  if (method == "balance") {
    # TODO Some arguments are missing here
    return(
      collapse_groups(
        data = data,
        n = n,
        group_cols = group_cols,
        id_col = id_col,
        extreme_pairing_levels = extreme_pairing_levels,
        balance_size = FALSE,
        col_name = col_name
      )
    )
  }

  # Check arguments ####
  # Some arguments go directly to fold()
  # so they will be checked there
  check_collapse_groups_by_(
    data = data,
    n = n,
    group_cols = group_cols,
    method = method,
    col_name = col_name
  )
  checkmate::assert_string(
    x = id_col,
    na.ok = FALSE,
    min.chars = 1,
    null.ok = TRUE
  )
  checkmate::assert_names(
    colnames(data),
    must.include = id_col
  )
  checkmate::assert_factor(
    x = data[[id_col]],
    any.missing = FALSE
  )
  # End argument checks ####

  # Prepare for collapsing
  # Includes renaming columns with ".folds" in their name
  # and checking `data` isn't grouped by any `group_cols`
  prepped <- prepare_collapse_groups_run_(
    data = data,
    group_cols = group_cols,
    id_col = id_col
  )
  data <- prepped[["data"]]
  data_group_cols <- prepped[["data_group_cols"]]
  group_cols <- prepped[["group_cols"]]
  id_col <- prepped[["id_col"]]

  # Collapse groups within each group subset in `data`
  # NOTE: The `data_group_cols` groups, not the `group_cols` groups
  data <- run_by_group_df(
    data = data,
    .fn = run_collapse_groups_by_ids_,
    n = n,
    group_cols = group_cols,
    id_col = id_col,
    method = method,
    col_name = col_name
  )

  # Prepare data for return
  data <- prepare_collapse_groups_output_(
    data = data,
    data_group_cols = data_group_cols,
    group_cols = group_cols,
    col_name = col_name,
    num_new_group_cols = 1
  )

  data

}

run_collapse_groups_by_ids_ <- function(
  data,
  n,
  group_cols,
  id_col,
  method,
  col_name) {

  # Create summary with combined score
  id_summary <- data %>%
    dplyr::group_by(!!!rlang::syms(group_cols)) %>%
    dplyr::summarize(n_ids = length(unique(!!as.name(id_col))), .groups = "drop")

  # Order summary depending on method
  # Create groups
  # Transfer groups to `data`
  data <- add_ordered_summary_groups_(
    data = data,
    summary = id_summary,
    n = n,
    group_cols = group_cols,
    num_col = "n_ids",
    method = method,
    col_name = col_name
  )

  data
}


##  .................. #< 614bf0d628035c9986e785b3e50c726b ># ..................
##  Helpers                                                                 ####


add_ordered_summary_groups_ <- function(data, summary, n, group_cols, num_col, method, col_name){
  order_fn <- identity
  if (method == "descending"){
    order_fn <- dplyr::desc
  }

  # Order and group
  new_groups <- summary %>%
    dplyr::arrange(order_fn(!!as.name(num_col))) %>%
    group(n = n, method = "n_dist", col_name = col_name) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::one_of(num_col))

  # Add new groups
  data <- data %>%
    dplyr::left_join(new_groups, by = group_cols)

  data
}


### . . . . . . . . .. #< 9b246b10eb0769ffa774eee6835009d2 ># . . . . . . . . ..
### Check args shared                                                       ####

check_collapse_groups_by_ <- function(
  data,
  n,
  group_cols,
  method = "balance", # ascending/descending
  col_name = ".coll_groups"
){
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data,
                               min.rows = 1,
                               add = assert_collection)
  checkmate::assert_numeric(
    x = n,
    lower = 1,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_character(
    x = group_cols,
    min.len = 1,
    min.chars = 1,
    any.missing = FALSE,
    unique = TRUE,
    names = "unnamed",
    add = assert_collection
  )
  checkmate::assert_string(x = col_name,
                           min.chars = 1,
                           add = assert_collection)
  checkmate::assert_string(x = method,
                           min.chars = 1,
                           add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(
    x = method,
    subset.of = c("balance", "ascending", "descending"),
    add = assert_collection
  )
  checkmate::assert_names(
    x = colnames(data),
    must.include = group_cols,
    type = "unique",
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####
}
