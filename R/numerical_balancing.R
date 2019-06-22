
numerically_balanced_group_factor_ <- function(data, n, num_col, method="n_fill",
                                               unequal_method="first", force_equal=FALSE,
                                               randomize_pairs=T) {

  # Create unique local temporary index variable name
  local_tmp_index_var <- create_tmp_var(data)
  local_tmp_groups_var <- create_tmp_var(data, ".groups")

  nrows_equal <- nrow(data) %% 2 == 0

  print(n)
  # Check if we have enough data for pairwise folding
  # or if we are running partitioning (l_sizes)
  if (method == "l_sizes" || (length(n) == 1 && n > 1 && nrow(data) < n * 2)){
      randomize_pairs <- FALSE
  }

  # Arrange by smallest, biggest, 2nd smallest, 2nd biggest, etc.
  # If the number of rows is unequal, the row with the smallest value is alone
  # This is done, as it is the one with the least effect on sum of values in a group
  data_sorted <- data # TODO is this renaming necessary?
  data_sorted[[local_tmp_index_var]] <- 1:nrow(data_sorted)
  data_sorted <- data_sorted %>%
    dplyr::arrange(!!as.name(num_col)) %>%
    rearrange(method = "pair_extremes",
              unequal_method = unequal_method,
              drop_rearrange_factor = FALSE,
              rearrange_factor_name = "rearrange_factor") # Not actually a factor....

  if(isTRUE(randomize_pairs)){ # TODO rename "randomize_pairs", as it's not very descriptive

    has_excessive <- nlevels(factor(data_sorted$rearrange_factor)) %% n != 0

    if (has_excessive){

      if (!nrows_equal){
        group_to_distribute <- dplyr::case_when(unequal_method == "first" ~ as.numeric(1),
                                                unequal_method == "last" ~ as.numeric(floor(nrow(data_sorted) / 2) + 1),
                                                unequal_method == "middle" ~ as.numeric(ceiling(nrow(data_sorted) / 4) + 1))
      } else {
        group_to_distribute <- sample(unique(data_sorted[["rearrange_factor"]]), 1)
      }

      rows_to_distribute <- data_sorted %>%
        dplyr::filter(rearrange_factor == group_to_distribute) %>%
        dplyr::arrange(!! as.name(num_col))

      data_sorted <- data_sorted %>%
        dplyr::filter(rearrange_factor != group_to_distribute)

    }

    # Create groups
    data_sorted <- data_sorted %>%
      group_uniques_(n=n,
                     id_col="rearrange_factor",
                     method=method,
                     col_name=local_tmp_groups_var)

    if (has_excessive){
      # Calculate sums of the other pairs
      # Get the smallest (and second smallest if we have 2 rows to distribute)
      smallest_folds <- data_sorted %>%
        dplyr::group_by(!!as.name(local_tmp_groups_var)) %>%
        dplyr::summarize(group_aggr = sum(!!as.name(num_col))) %>%
        dplyr::arrange(.data$group_aggr) %>%
        dplyr::filter(dplyr::row_number() %in% 1:nrow(rows_to_distribute)) %>%
        dplyr::pull(!!as.name(local_tmp_groups_var))

      rows_to_distribute[[local_tmp_groups_var]] <- smallest_folds

      data_sorted <- data_sorted %>%
        dplyr::bind_rows(rows_to_distribute)
    }

    data_sorted <- data_sorted %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!as.name(local_tmp_index_var))

    # The following was an attempt to make the groups even more balanced
    # and with small dataset it seems to work well
    # but it's deterministic, which we might not always want
    # also I've seen it produce less balanced solutions that the new version.
  } else {

    # Find sum of values in each rearrange factor group
    # Once again, arrange by smallest, biggest, 2nd smallest, 2nd biggest, etc.
    # Create groups, filling up from the top, so the potentially single rearrange factor group
    # ie. the row with the smallest value, is the one being added to another group.

    tmp_group_scores <- data_sorted %>%
      dplyr::group_by(.data$rearrange_factor) %>%
      dplyr::summarize(group_aggr = sum(!!as.name(num_col)))

    if (!nrows_equal & unequal_method == "first") {
      # Reorder with first group always first (otherwise doesn't work with negative numbers)
      tmp_group_scores_sorted <- tmp_group_scores %>%
        dplyr::filter(dplyr::row_number() == 1) %>%
        dplyr::bind_rows(tmp_group_scores %>%
                           dplyr::filter(dplyr::row_number() != 1) %>%
                           dplyr::arrange(.data$group_aggr))
    } else {
      tmp_group_scores_sorted <- tmp_group_scores %>%
        dplyr::arrange(.data$group_aggr)
    }

    # Rearrange again
    tmp_second_rearrange <- tmp_group_scores_sorted %>%
      rearrange(method = "pair_extremes", unequal_method = unequal_method,
                drop_rearrange_factor = FALSE,
                rearrange_factor_name = "rearrange_factor_2") %>%
      dplyr::select(.data$rearrange_factor, .data$rearrange_factor_2)

    # Join data_sorted with the second rearrange factor
    # Order by this factor and group the dataset, filling from the top
    # Revert order back to original order of data
    # Get group factor
    data_sorted <- data_sorted %>%
      dplyr::left_join(tmp_second_rearrange, by = "rearrange_factor") %>%
      dplyr::arrange(.data$rearrange_factor_2, .data$rearrange_factor, !!as.name(num_col)) %>%
      group(n=n, method = method, col_name = local_tmp_groups_var, force_equal = force_equal) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!as.name(local_tmp_index_var))

  }

  data_sorted %>%
    dplyr::pull(!!as.name(local_tmp_groups_var)) %>%
    as.factor()

}


