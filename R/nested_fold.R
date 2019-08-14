nested_fold <- function(data,
                        ks = c(5, 10),
                        cat_col = NULL,
                        num_col = NULL,
                        id_col = NULL,
                        group_method = 'n_dist',
                        nesting_method = "x",
                        id_aggregation_fn = sum,
                        extreme_pairing_levels = 1,
                        num_fold_cols = 1, # Currently only used at last level
                        unique_fold_cols_only = TRUE,
                        max_iters = 5,
                        handle_existing_fold_cols = "keep_warn", # TODO Make sure this is meaningful and works!!
                        parallel = FALSE) {


  original_colnames <- colnames(data)

  # Method x (rename):
  ## 'fold col 1': fold dataset
  ## 'fold col 2': group by 'fold col 1' and fold() each group separately
  ## 'fold col 3': group by 'fold col 1' and 'fold col 2' and fold() each group separately
  ## ...
  ## The last level may have multiple, unique fold columns

  if (nesting_method == "x") {

    nested <- internal_nested_fold_method_x(
      data = data,
      ks = ks,
      by = NULL,
      cat_col = cat_col,
      num_col = num_col,
      id_col = id_col,
      method = group_method,
      id_aggregation_fn = id_aggregation_fn,
      extreme_pairing_levels = extreme_pairing_levels,
      num_fold_cols = num_fold_cols,
      unique_fold_cols_only = unique_fold_cols_only,
      max_iters = max_iters,
      handle_existing_fold_cols = "remove",
      parallel = parallel
    )

    folded_data <- nested[["data"]]
    fold_col_names <- nested[["col_names"]]

    # Do check
    new_colnames <- setdiff(colnames(folded_data),
                            original_colnames)
    if (length(setdiff(new_colnames, fold_col_names)) != 0 ||
        length(setdiff(fold_col_names, new_colnames)) != 0) {
      stop("something went wrong when creating nested folds")
    }

    fold_columns <- folded_data %>%
      dplyr::select(dplyr::one_of(fold_col_names))

    # Rename new columns meaningfully
    meaningful_names <- create_nested_fold_cols_names(num_fold_cols = num_fold_cols,
                                                      ks = ks,
                                                      fold_col_names = fold_col_names)

    if (length(meaningful_names) != length(fold_col_names)) {
      stop("something went wrong when creating names for the nested folds")
    }

    fold_columns <- fold_columns %>%
      dplyr::rename_at(dplyr::vars(fold_col_names), ~ meaningful_names)

    data %>%
      dplyr::bind_cols(fold_columns)
  }

}


# TODO Add tests:
# create_nested_fold_cols_names(c(3,1,3), c(2,2,4), NULL)
# create_nested_fold_cols_names(3, c(2,2), NULL)
# create_nested_fold_cols_names(1, NULL, c("a","t","f","g"))
create_nested_fold_cols_names <- function(num_fold_cols, ks, fold_col_names, nesting_method = "x"){
  if (nesting_method == "x"){

    # Rename new columns meaningfully
    if (length(num_fold_cols) > 1){
      # Note: This part should currently not be used,
      # as we only allow multiple fold cols in the last level
      meaningful_names <-
        plyr::llply(1:length(num_fold_cols),
                    function(nm_ind) {
                      paste0(".nested_folds_", nm_ind, "_", 1:num_fold_cols[[nm_ind]])
                    }) %>% unlist()
    } else if (num_fold_cols > 1){
      first_levels_names <- paste0(".nested_folds_",
                                   1:(length(ks)-1))
      last_level_names <- paste0(".nested_folds_",
                                 length(ks), "_",
                                 1:num_fold_cols)
      meaningful_names <- c(first_levels_names, last_level_names)
    } else {
      meaningful_names <- paste0(".nested_folds_", 1:length(fold_col_names))
    }
  }
  meaningful_names
}

internal_nested_fold_method_x <- function(data,
                                 ks = c(5, 10),
                                 by_ = NULL,
                                 cat_col = NULL,
                                 num_col = NULL,
                                 id_col = NULL,
                                 method = 'n_dist',
                                 id_aggregation_fn = sum,
                                 extreme_pairing_levels = 1,
                                 num_fold_cols = 1,
                                 unique_fold_cols_only = TRUE,
                                 max_iters = 5,
                                 handle_existing_fold_cols = "keep_warn",
                                 parallel = FALSE) {

  # Get current number of fold cols
  # and update num_fold_cols for next iteration if necessary
  if (length(num_fold_cols) == 1){
    if (length(ks) == 1){
      current_num_fold_cols <- num_fold_cols
    } else {
      current_num_fold_cols <- 1
    }
  } else {
    current_num_fold_cols <- num_fold_cols[[1]]
    num_fold_cols <- num_fold_cols[-1]
  }

  # Get the current number of folds to create
  # and update ks if necessary
  if (length(ks) > 1){
    current_k <- ks[[1]]
    ks <- ks[-1]
    last_level <- FALSE
  } else {
    current_k <- ks
    last_level <- TRUE
  }

  # Get original column names
  original_cols <- colnames(data)

  if (!is.null(by_)){
    data <- data %>%
      dplyr::group_by(!!! rlang::syms(by_)) %>%
      dplyr::group_modify( ~ fold_rename_wrapper(
        .x,
        k = current_k,
        cat_col = cat_col,
        num_col = num_col,
        id_col = id_col,
        method = method,
        id_aggregation_fn = id_aggregation_fn,
        extreme_pairing_levels = extreme_pairing_levels,
        num_fold_cols = current_num_fold_cols,
        unique_fold_cols_only = unique_fold_cols_only,
        max_iters = max_iters,
        handle_existing_fold_cols = handle_existing_fold_cols, # TODO think this arg through in nested
        parallel = parallel,
        cols_to_remove_post_fold = by_), keep = TRUE) %>%
      dplyr::ungroup()
  } else {
    data <- fold_rename_wrapper(
      data,
      k = current_k,
      cat_col = cat_col,
      num_col = num_col,
      id_col = id_col,
      method = method,
      id_aggregation_fn = id_aggregation_fn,
      extreme_pairing_levels = extreme_pairing_levels,
      num_fold_cols = current_num_fold_cols,
      unique_fold_cols_only = unique_fold_cols_only,
      max_iters = max_iters,
      handle_existing_fold_cols = handle_existing_fold_cols, # TODO think this arg through in nested
      parallel = parallel) %>%
      dplyr::ungroup()
  }

  # Get name of new fold column
  new_col <- setdiff(colnames(data), original_cols)

  # Append new fold col to group variables
  by_ <- c(by_, new_col)

  # If .folds was renamed to .folds_1, we need to remove the
  # .folds from by_
  if (by_[[1]] == ".folds" && length(by_) > 1){
    by_ <- by_[-1]
  }

  if (!isTRUE(last_level)){
    return(
      internal_nested_fold_method_x(data = data,
                           ks = ks,
                           by_ = by_,
                           cat_col = cat_col,
                           num_col = num_col,
                           id_col = id_col,
                           method = method,
                           id_aggregation_fn = id_aggregation_fn,
                           extreme_pairing_levels = extreme_pairing_levels,
                           num_fold_cols = num_fold_cols,
                           unique_fold_cols_only = unique_fold_cols_only,
                           max_iters = max_iters,
                           handle_existing_fold_cols = "keep",
                           parallel = parallel))
  } else {

    return(
      list(
        "data" = data,
        "col_names" = by_
        )
      )
  }


}

fold_rename_wrapper <- function(data, k, cat_col, num_col, id_col,
                                method, id_aggregation_fn,
                                extreme_pairing_levels, num_fold_cols,
                                unique_fold_cols_only, max_iters,
                                handle_existing_fold_cols, parallel,
                                cols_to_remove_post_fold = NULL){

  # Extract original column names
  original_cols <- colnames(data)

  # Fold the dataset
  data <-
    fold(
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

  # Extract new fold column names
  new_cols <- setdiff(colnames(data), original_cols)

  # Create new unique temporary column names
  # Needed when called in dplyr::group_modify
  new_tmp_names <- plyr::llply(new_cols, function(nc){
    create_tmp_var(data, paste0("tmp_", nc, "_var_", num_fold_cols > 1),
                   disallowed = cols_to_remove_post_fold)
  }) %>% unlist()

  # Remove specified columns
  # As dplyr::group_modify does not allow us to
  # return the original grouping variables
  if (!is.null(cols_to_remove_post_fold)){
    data <- data %>%
      dplyr::select(-dplyr::one_of(cols_to_remove_post_fold))
  }

  data %>%
    dplyr::rename_at(dplyr::vars(new_cols), ~new_tmp_names)
}
