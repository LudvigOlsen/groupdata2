
# These are possible simplified versions of collapse_groups
# that may also allow different methods for each case
# that cannot be handled by collapse_groups

# collapse_groups_by_size <- function(
#   data,
#   n,
#   group_cols,
#   method = "balance", # ascending/descending/ignore
#   extreme_pairing_levels = 1,
#   col_name = ".coll_groups") {
#
#
#
#
# }
#
# collapse_groups_by_numeric <- function(
#   data,
#   n,
#   group_cols,
#   num_col = NULL,
#   method = "balance", # ascending/descending
#   group_aggregation_fn = mean,
#   extreme_pairing_levels = 1,
#   col_name = ".coll_groups") {
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
#
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
