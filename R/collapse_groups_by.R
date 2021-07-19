
# These are possible simplified versions of collapse_groups
# that may also allow different methods for each case
# that cannot be handled by collapse_groups



##  .................. #< 077f4d486ff8d1e493b6e0d8c6626776 ># ..................
##  Collapse by size                                                        ####


collapse_groups_by_size <- function(
  data,
  n,
  group_cols,
  method = "balance", # ascending/descending
  extreme_pairing_levels = 1, # only method==balance
  col_name = ".coll_groups") {

  if (method == "balance") {
    # TODO Some arguments are missing here
    return(
      collapse_groups(
        data = data,
        n = n,
        group_cols = group_cols,
        extreme_pairing_levels = extreme_pairing_levels,
        col_name = col_name,
        balance_size = TRUE
      )
    )
  }

  # Check arguments
  # Some arguments go directly to fold()
  # so they will be checked there
  # check_collapse_groups_???(
  #   data = data,
  #   n = n,
  #   group_cols = group_cols,
  #   col_name = col_name
  # )

  #### Prepare data and names ####

  # Prepare for collapsing
  # Includes renaming columns with ".folds" in their name
  # and checking `data` isn't grouped by any `group_cols`
  prepped <- prepare_collapse_groups_run_(data = data, group_cols = group_cols)
  data <- prepped[["data"]]
  data_group_cols <- prepped[["data_group_cols"]]
  group_cols <- prepped[["group_cols"]]
  replaced_fold_name <- prepped[["replaced_fold_name"]]

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
    num_new_group_cols = 1,
    replaced_fold_name = replaced_fold_name
  )

  data

}

run_collapse_groups_by_size_ <- function(data,
                                         n,
                                         group_cols,
                                         method,
                                         col_name){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_names(method,
                          subset.of = c("ascending", "descending"),
                          add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

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


collapse_groups_by_numeric <- function(
  data,
  n,
  group_cols,
  num_col = NULL,
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
        num_col = num_col,
        extreme_pairing_levels = extreme_pairing_levels,
        group_aggregation_fn = group_aggregation_fn,
        balance_size = FALSE,
        col_name = col_name
      )
    )
  }

  # Check arguments
  # Some arguments go directly to fold()
  # so they will be checked there
  # check_collapse_groups_???(
  #   data = data,
  #   n = n,
  #   group_cols = group_cols,
  #   col_name = col_name
  # )

  # Prepare for collapsing
  # Includes renaming columns with ".folds" in their name
  # and checking `data` isn't grouped by any `group_cols`
  prepped <- prepare_collapse_groups_run_(data = data, group_cols = group_cols)
  data <- prepped[["data"]]
  data_group_cols <- prepped[["data_group_cols"]]
  group_cols <- prepped[["group_cols"]]
  replaced_fold_name <- prepped[["replaced_fold_name"]]

  # Collapse groups within each group subset in `data`
  # NOTE: The `data_group_cols` groups, not the `group_cols` groups
  data <- run_by_group_df(
    data = data,
    .fn = run_collapse_groups_by_numeric_,
    n = n,
    group_cols = group_cols,
    num_col = num_col,
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
    num_new_group_cols = 1,
    replaced_fold_name = replaced_fold_name
  )

  data

}

run_collapse_groups_by_numeric_ <- function(
  data,
  n,
  group_cols,
  num_col,
  method,
  group_aggregation_fn,
  col_name) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_names(method,
                          subset.of = c("ascending", "descending"),
                          add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  num_summary <- data %>%
    dplyr::group_by(!!!rlang::syms(group_cols)) %>%
    dplyr::summarise(num_aggr = group_aggregation_fn(!!as.name(num_col)), .groups = "drop")

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


collapse_groups_by_levels <- function(
  data,
  n,
  group_cols,
  cat_col = NULL,
  cat_levels = ".majority",
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
        balance_size = FALSE,
        col_name = col_name
      )
    )
  }

  # Check arguments
  # Some arguments go directly to fold()
  # so they will be checked there
  # check_collapse_groups_???(
  #   data = data,
  #   n = n,
  #   group_cols = group_cols,
  #   col_name = col_name
  # )

  # Prepare for collapsing
  # Includes renaming columns with ".folds" in their name
  # and checking `data` isn't grouped by any `group_cols`
  prepped <- prepare_collapse_groups_run_(data = data, group_cols = group_cols)
  data <- prepped[["data"]]
  data_group_cols <- prepped[["data_group_cols"]]
  group_cols <- prepped[["group_cols"]]
  replaced_fold_name <- prepped[["replaced_fold_name"]]

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
    num_new_group_cols = 1,
    replaced_fold_name = replaced_fold_name
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

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_names(method,
                          subset.of = c("ascending", "descending"),
                          add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

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

  # Check arguments
  # Some arguments go directly to fold()
  # so they will be checked there
  # check_collapse_groups_???(
  #   data = data,
  #   n = n,
  #   group_cols = group_cols,
  #   col_name = col_name
  # )

  # Prepare for collapsing
  # Includes renaming columns with ".folds" in their name
  # and checking `data` isn't grouped by any `group_cols`
  prepped <- prepare_collapse_groups_run_(data = data, group_cols = group_cols)
  data <- prepped[["data"]]
  data_group_cols <- prepped[["data_group_cols"]]
  group_cols <- prepped[["group_cols"]]
  replaced_fold_name <- prepped[["replaced_fold_name"]]

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
    num_new_group_cols = 1,
    replaced_fold_name = replaced_fold_name
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

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_names(method,
                          subset.of = c("ascending", "descending"),
                          add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

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
