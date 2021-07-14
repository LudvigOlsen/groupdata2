
# Should be done on grouped data as well?
collapse_groups <- function(
  data,
  n,
  group_cols,
  cat_col = NULL,
  cat_method = "balance",
  cat_selection_method = "majority",
  num_col = NULL,
  num_method = "balance",
  group_aggregation_fn = mean,
  extreme_pairing_levels = 1,
  col_name = ".coll_groups") {

  if (!is.null(cat_col) && !is.null(num_col)){
    # Both must use the balance method (currently at least)
    if (cat_method != "balance" || num_method != "balance"){
      stop("when both `cat_col` and `num_col` are specified, they must both use the `balance` method.")
    }
  }

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
    updated <- replace_fold_in_names(
      data = data,
      data_group_cols = data_group_cols,
      group_cols = group_cols
    )
    data <- updated[["data"]]
    data_group_cols <- updated[["data_group_cols"]]
    group_cols <- updated[["group_cols"]]
  }

  # Create unique old groups factor
  tmp_old_group_var <- create_tmp_var(data = data, tmp_var = ".old_group")
  data <- data %>%
    dplyr::group_by(!!!rlang::syms(c(data_group_cols, group_cols)))
  data[[tmp_old_group_var]] <- dplyr::group_indices(data)
  data <- data %>%
    dplyr::group_by(!!!rlang::syms(c(data_group_cols)))

  #### Collapse groups ####

  tmp_group_class_var <- NULL
  if (!is.null(cat_col)){
    # TODO When all have the same majority class, we can't really balance them?
    # Check that the smallest chosen level has frequence >= `n`!
    if (cat_selection_method %in% c("majority", "minority")){
      tmp_group_class_var <- create_tmp_var(data = data, tmp_var = ".group_cat_col")
      group_cat_classes <- data %>%
        dplyr::count(!!!rlang::syms(c(group_cols, cat_col))) %>%
        dplyr::sample_frac() %>%
        dplyr::group_by(!!!rlang::syms(c(data_group_cols, group_cols))) %>%
        print()

      if (cat_selection_method == "majority"){
        group_cat_classes <- group_cat_classes %>%
          dplyr::slice(which.max(n))
      } else if (cat_selection_method == "minority"){
        group_cat_classes <- group_cat_classes %>%
          dplyr::slice(which.min(n))
      }

      enough_of_smallest_level <- group_cat_classes %>%
        dplyr::group_by(!!!rlang::syms(data_group_cols)) %>%
        dplyr::count(!!as.name(cat_col)) %>%
        dplyr::pull(.data$n) %>%
        min() >= n

      if (!isTRUE(enough_of_smallest_level)){
        stop("One of the class levels in `cat_col` is not selected frequently enough to balance.")
      }

      group_cat_classes <- group_cat_classes %>%
        dplyr::select(-.data$n) %>%
        dplyr::rename(!!tmp_group_class_var := !!as.name(cat_col))

      print(group_cat_classes)

      data <- data %>%
        dplyr::left_join(group_cat_classes,
                         by = colnames(group_cat_classes)[
                           colnames(group_cat_classes) != tmp_group_class_var]
        )
    } else if (cat_selection_method %in% levels(group_cat_classes[[cat_col]])){
      # Calculate ratio of the chosen class and use num_col balancing?
      # TODO
    }
  }

  # TODO refactor so the actual groupings are made after finding the cat class and num_col summary!

  print(data)

  if (!is.null(num_col)){

    data_num_summary <- data %>%
      dplyr::group_by(!!!rlang::syms(c(data_group_cols, tmp_old_group_var))) %>%
      # Must be grouped by the `data_group_cols` afterwards!
      dplyr::summarise(aggr = group_aggregation_fn(!!as.name(num_col)), .groups = "drop_last")

    if (num_method == "balance"){
      new_groups <- fold(
        data = data_num_summary,
        k = n,
        num_col = "aggr",
        extreme_pairing_levels = extreme_pairing_levels
      ) %>%
        dplyr::rename(!!col_name := .data$.folds)

    } else if (num_method %in% c("descending", "ascending")){
      desc_fn <- dplyr::desc
      if (num_method == "ascending") desc_fn <- identity

      new_groups <- data_num_summary %>%
        dplyr::arrange(desc_fn(.data$aggr)) %>%
        group(n = n, method = "n_dist", col_name = col_name)
    }
  }

  # Select the relevant columns
  new_groups <- new_groups %>%
    dplyr::select(dplyr::one_of(tmp_old_group_var, col_name))

  # Add new groups
  data <- data %>%
    dplyr::left_join(new_groups, by = tmp_old_group_var)

  # If `data` contained a fold column
  # We need to invert the renaming
  if (isTRUE(replaced_fold_name)) {
    updated <- replace_fold_in_names(
      data = data,
      data_group_cols = data_group_cols,
      group_cols = group_cols,
      invert = TRUE
    )
    data <- updated[["data"]]
    data_group_cols <- updated[["data_group_cols"]]
    group_cols <- updated[["group_cols"]]
  }

  # Remove tmp column
  data[[tmp_old_group_var]] <- NULL

  # Group by the new groups
  data <- dplyr::group_by(data, !!!rlang::syms(c(data_group_cols, col_name)))

  data
}


# Replace ".fold" with ".____fold" in names to avoid overwriting columns
replace_fold_in_names <- function(data, data_group_cols, group_cols, invert = FALSE){

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
  colnames(data) <- replace_fold(colnames(data))
  data_group_cols <- replace_fold(data_group_cols)
  group_cols <- replace_fold(group_cols)

  list(data = data,
       data_group_cols = data_group_cols,
       group_cols = group_cols)
}
