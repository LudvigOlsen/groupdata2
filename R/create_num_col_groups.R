
#' @importFrom dplyr n
create_num_col_groups <- function(data, n, num_col,
                                  cat_col = NULL, id_col = NULL,
                                  col_name,
                                  id_aggregation_fn = sum,
                                  extreme_pairing_levels = 1,
                                  method = "n_fill",
                                  unequal_method = "first",
                                  use_triplets = FALSE,
                                  optimize_for = "mean",
                                  force_equal = FALSE,
                                  pre_randomize = TRUE) {

  # Most have been checked in parent
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = col_name, add = assert_collection)
  checkmate::assert_string(x = unequal_method, add = assert_collection)
  checkmate::assert_string(x = optimize_for, add = assert_collection)
  checkmate::assert_flag(x = pre_randomize, add = assert_collection)
  checkmate::assert_flag(x = use_triplets, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # If method is n_*, we are doing folding
  is_n_method <- substring(method, 1, 2) == "n_"

  if (!is.null(cat_col)){
    # In case of >1 cat columns
    # We work with the group indices
    local_tmp_cat_col_var <- create_tmp_var(data, tmp_var = ".__cat_col")
    data[[local_tmp_cat_col_var]] <- data %>%
      dplyr::group_by(!!!rlang::syms(cat_col)) %>%
      dplyr::group_indices()
    cat_col <- local_tmp_cat_col_var
  }

  # Sample data frame before use.
  if (isTRUE(pre_randomize)) {

    # Create unique local temporary index
    local_tmp_index_var <- create_tmp_var(data)
    data[[local_tmp_index_var]] <- seq_len(nrow(data))

    # Reorder randomly
    data <- data %>%
      dplyr::sample_frac(1)
  }

  # Init rank summary for balanced joining of fold ID's
  # when cat_col is specified
  if (isTRUE(is_n_method)) rank_summary <- NULL

  # If cat_col is not NULL
  if (!is.null(cat_col)) {

    # If id_col is not NULL
    if (!is.null(id_col)) {

      # aggregate val col per ID
      ids_aggregated <- data %>%
        dplyr::group_by(!!as.name(cat_col), !!as.name(id_col)) %>%
        dplyr::summarize(aggr_val = id_aggregation_fn(!!as.name(num_col))) %>%
        dplyr::ungroup()

      # Find groups for each category
      ids_grouped <- plyr::ldply(unique(ids_aggregated[[cat_col]]), function(category) {
        ids_for_cat <- ids_aggregated[
          ids_aggregated[[cat_col]] == category,
        ]

        # Add group factor
        ids_for_cat <- call_extreme_grouping(
          data = ids_for_cat,
          n = n,
          method = method,
          unequal_method = unequal_method,
          extreme_pairing_levels = extreme_pairing_levels,
          num_col = "aggr_val",
          new_col = "._new_groups_",
          use_triplets = use_triplets
        )

        if (isTRUE(is_n_method)) {
          # Rename groups to be combined in the most balanced way
          if (is.null(rank_summary)) {
            rank_summary <<- create_rank_summary(
              ids_for_cat,
              levels_col = "._new_groups_",
              num_col = "aggr_val"
            )
          } else {
            renaming_levels_list <- rename_levels_by_reverse_rank_summary(
              data = ids_for_cat,
              rank_summary = rank_summary,
              levels_col = "._new_groups_",
              num_col = "aggr_val"
            )
            rank_summary <<-
              renaming_levels_list[["updated_rank_summary"]]
            ids_for_cat <- renaming_levels_list[["updated_data"]]
          }
        }

        ids_for_cat %>%
          base_deselect(cols = "aggr_val")
      })

      # Transfer groups to data
      data <- data %>%
        dplyr::inner_join(ids_grouped, by = c(cat_col, id_col))

      # If id_col is NULL
    } else {

      # For each category in cat_col
      # .. create value balanced group factor

      # Find groups for each category
      data <- plyr::ldply(unique(data[[cat_col]]), function(category) {
        data_for_cat <- data[
          data[[cat_col]] == category,
        ]

        # Add group factor
        data_for_cat <- call_extreme_grouping(
          data = data_for_cat,
          n = n,
          method = method,
          unequal_method = unequal_method,
          extreme_pairing_levels = extreme_pairing_levels,
          num_col = num_col,
          new_col = "._new_groups_",
          use_triplets = use_triplets
        )

        if (isTRUE(is_n_method)) {
          # Rename groups to be combined in the most balanced way
          if (is.null(rank_summary)) {
            rank_summary <<- create_rank_summary(
              data_for_cat,
              levels_col = "._new_groups_",
              num_col = num_col
            )
          } else {
            renaming_levels_list <- rename_levels_by_reverse_rank_summary(
              data = data_for_cat, rank_summary = rank_summary,
              levels_col = "._new_groups_", num_col = num_col
            )
            rank_summary <<- renaming_levels_list[["updated_rank_summary"]]
            data_for_cat <- renaming_levels_list[["updated_data"]]
          }
        }

        data_for_cat
      })
    }

    # If cat_col is NULL
  } else {

    # If id_col is not NULL
    if (!is.null(id_col)) {

      # Aggregate num_col for IDs with the passed id_aggregation_fn
      # Create value balanced group factor based on aggregated values
      # Join the groups back into the data

      # aggregate val col per ID
      ids_aggregated <- data %>%
        dplyr::group_by(!!as.name(id_col)) %>%
        dplyr::summarize(aggr_val = id_aggregation_fn(!!as.name(num_col))) %>%
        dplyr::ungroup()

      # Add group factor
      ids_aggregated <- call_extreme_grouping(
        data = ids_aggregated,
        n = n,
        method = method,
        unequal_method = unequal_method,
        extreme_pairing_levels = extreme_pairing_levels,
        num_col = "aggr_val",
        new_col = "._new_groups_",
        use_triplets = use_triplets
      )

      ids_aggregated$aggr_val <- NULL

      # Transfer groups to data
      data <- data %>%
        dplyr::inner_join(ids_aggregated, by = c(id_col))

      # If id_col is NULL
    } else {

      # Add group factor
      data <- call_extreme_grouping(
        data = data,
        n = n,
        method = method,
        unequal_method = unequal_method,
        extreme_pairing_levels = extreme_pairing_levels,
        num_col = num_col,
        new_col = "._new_groups_",
        use_triplets = use_triplets
      )
    }
  }

  # Reorder if pre-randomized
  if (isTRUE(pre_randomize)) {
    data <- data %>%
      dplyr::arrange(!!as.name(local_tmp_index_var))
    data[[local_tmp_index_var]] <- NULL
  }

  # Force equal
  # Remove stuff
  if (method == "l_sizes" & isTRUE(force_equal)) {
    number_of_groups_specified <- length(n)

    data <- data[
      factor_to_num(data[["._new_groups_"]]) <= number_of_groups_specified,
    ]
  }

  # replace column name
  if (col_name != "._new_groups_")
    data <- base_rename(data, before = "._new_groups_", after = col_name)

  # Remove temporary cat_col
  if (!is.null(cat_col)){
      data <- data %>%
        base_deselect(cat_col)
  }

  dplyr::as_tibble(data)

}


call_extreme_grouping <- function(
  data,
  n,
  method,
  extreme_pairing_levels,
  unequal_method,
  num_col = "aggr_val",
  new_col = "._new_groups_",
  use_triplets = FALSE) {

  #
  # Wrapper for choosing between
  # numerically_balanced_group_factor_ and
  # numerically_balanced_group_factor_triplets_
  #

  # Create group factor
  if (!isTRUE(use_triplets)){

    # Use extreme pairing
    data[[new_col]] <- numerically_balanced_group_factor_(
      data = data,
      n = n,
      num_col = num_col,
      method = method,
      unequal_method = unequal_method,
      extreme_pairing_levels = extreme_pairing_levels
    )

  } else {

    # Use extreme triplet grouping
    # First ensure extreme grouping levels are compatible
    tmp_local_extreme_grouping_levels <- extreme_pairing_levels
    if (extreme_pairing_levels > 1) {
      while (nrow(data) < 3 * 3 ^ tmp_local_extreme_grouping_levels &&
             tmp_local_extreme_grouping_levels > 1) {
        tmp_local_extreme_grouping_levels <- tmp_local_extreme_grouping_levels - 1
      }
    }
    data[[new_col]] <-
      numerically_balanced_group_factor_triplets_(
        data = data,
        n = n,
        num_col = num_col,
        method = method,
        extreme_grouping_levels = tmp_local_extreme_grouping_levels
      )
  }

  data

}
