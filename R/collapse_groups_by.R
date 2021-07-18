
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

  order_fn <- identity
  if (method == "descending"){
    order_fn <- dplyr::desc
  }

  new_groups <- size_summary %>%
    dplyr::arrange(order_fn(.data$size)) %>%
    group(n = n, method = "n_dist", col_name = col_name) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$size)

  # Add new groups
  data <- data %>%
    dplyr::left_join(new_groups, by = group_cols)

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

  size_summary <- data %>%
    dplyr::group_by(!!!rlang::syms(group_cols)) %>%
    dplyr::summarise(num_aggr = group_aggregation_fn(!!as.name(num_col)), .groups = "drop")

  order_fn <- identity
  if (method == "descending"){
    order_fn <- dplyr::desc
  }

  new_groups <- size_summary %>%
    dplyr::arrange(order_fn(.data$num_aggr)) %>%
    group(n = n, method = "n_dist", col_name = col_name) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$num_aggr)

  # Add new groups
  data <- data %>%
    dplyr::left_join(new_groups, by = group_cols)

  data
}

#
#   if (!is.null(num_col)){
#
#     data_num_summary <- data %>%
#       dplyr::group_by(!!!rlang::syms(c(data_group_cols, tmp_old_group_var))) %>%
#       # Must be grouped by the `data_group_cols` afterwards!
#       dplyr::summarise(num_aggr = group_aggregation_fn(!!as.name(num_col)), .groups = "drop_last")
#
#     if (num_method == "balance"){
#       new_groups <- fold(
#         data = data_num_summary,
#         k = n,
#         num_col = "num_aggr",
#         extreme_pairing_levels = extreme_pairing_levels
#       ) %>%
#         dplyr::rename(!!col_name := .data$.folds)
#
#     } else if (num_method %in% c("descending", "ascending")){
#       desc_fn <- dplyr::desc
#       if (num_method == "ascending") desc_fn <- identity
#
#       new_groups <- data_num_summary %>%
#         dplyr::arrange(desc_fn(.data$num_aggr)) %>%
#         group(n = n, method = "n_dist", col_name = col_name)
#     }
#   }
#
# }



##  .................. #< 06fd80b0d0291e8a4a1c2960e9e5c443 ># ..................
##  Collapse by factor                                                      ####


# collapse_groups_by_factor <- function(
#   data,
#   n,
#   group_cols,
#   cat_col = NULL,
#   method = "balance", # ascending/descending
#   cat_class = ".majority",
#   group_aggregation_fn = mean,
#   extreme_pairing_levels = 1,
#   col_name = ".coll_groups") {
#
#   tmp_group_class_var <- NULL
#   if (!is.null(cat_col)){
#     # TODO When all have the same .majority class, we can't really balance them?
#     # Check that the smallest chosen level has frequence >= `n`!
#     if (cat_class %in% c(".majority", ".minority")){
#       tmp_group_class_var <- create_tmp_var(data = data, tmp_var = ".group_cat_col")
#       group_cat_classes <- data %>%
#         dplyr::count(!!!rlang::syms(c(group_cols, cat_col))) %>%
#         dplyr::sample_frac() %>%
#         dplyr::group_by(!!!rlang::syms(c(data_group_cols, group_cols))) %>%
#         print()
#
#       if (cat_class == ".majority"){
#         group_cat_classes <- group_cat_classes %>%
#           dplyr::slice(which.max(n))
#       } else if (cat_class == ".minority"){
#         group_cat_classes <- group_cat_classes %>%
#           dplyr::slice(which.min(n))
#       }
#
#       enough_of_smallest_level <- group_cat_classes %>%
#         dplyr::group_by(!!!rlang::syms(data_group_cols)) %>%
#         dplyr::count(!!as.name(cat_col)) %>%
#         dplyr::pull(.data$n) %>%
#         min() >= n
#
#       if (!isTRUE(enough_of_smallest_level)){
#         stop("One of the class levels in `cat_col` is not selected frequently enough to balance.")
#       }
#
#       group_cat_classes <- group_cat_classes %>%
#         dplyr::select(-.data$n) %>%
#         dplyr::rename(!!tmp_group_class_var := !!as.name(cat_col))
#
#       print(group_cat_classes)
#
#       data <- data %>%
#         dplyr::left_join(group_cat_classes,
#                          by = colnames(group_cat_classes)[
#                            colnames(group_cat_classes) != tmp_group_class_var]
#         )
#     } else if (cat_class %in% levels(group_cat_classes[[cat_col]])){
#       # Calculate ratio of the chosen class and use num_col balancing?
#       # TODO
#     }
#   }
#
# }
