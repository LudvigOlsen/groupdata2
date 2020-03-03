
numerically_balanced_group_factor_ <- function(data, n, num_col, method = "n_fill",
                                               unequal_method = "first",
                                               # Internal for now ("mean" or "sd" or (alternating) list)
                                               optimize_for = "mean",
                                               extreme_pairing_levels = 1,
                                               force_equal = FALSE) {

  # Create unique local temporary index variable name
  local_tmp_index_var <- create_tmp_var(data, ".tmp_index_")
  local_tmp_index_2_var <- create_tmp_var(data, ".tmp_index_2_")
  local_tmp_groups_var <- create_tmp_var(data, ".groups")
  local_tmp_rearrange_var <- create_tmp_var(data, ".rearrange_factor_")
  local_tmp_rearrange_2_var <- create_tmp_var(data, ".rearrange_factor_2_")

  # If method is n_*, we are doing folding
  is_n_method <- substring(method, 1, 2) == "n_"

  equal_nrows <- nrow(data) %% 2 == 0

  # Check if we have enough data for pairwise folding
  # or if we are running partitioning (l_sizes)
  if (method == "l_sizes") {
    group_by_rearrange_id <- FALSE
    num_final_groups <- length(n)
  } else if (isTRUE(is_n_method)) {
    if (length(n) > 1) {
      stop(paste0("n contained more than one element with method '", method, "'."))
    }
    if (n < 1) {
      n <- ceiling(nrow(data) / convert_percentage_(n, data))
    }
    num_final_groups <- n
    group_by_rearrange_id <- ifelse(nrow(data) < n * 2, FALSE, TRUE)
  } else {
    stop(paste0("method '", method, "' is currently not supported with num_col balancing."))
  }

  # Check if extreme_pairing_levels is too big for the dataset
  # TODO: Check that this calculation holds in all usecases
  # e.g. is it enough with one group level per fold when isTRUE(group_by_rearrange_id)?
  if (group_by_rearrange_id && extreme_pairing_levels > 1 &&
    nrow(data) < num_final_groups * 2^extreme_pairing_levels) {
    stop(paste0(
      "num_col: The (subset of) data is too small to perform ",
      extreme_pairing_levels,
      " levels of extreme pairing. Decrease 'extreme_pairing_levels'."
    ))
  }

  # If method="l_sizes" for instance, we want the last pairing to have at least one pair (two sub pairs)
  if (!group_by_rearrange_id && extreme_pairing_levels > 1 &&
    nrow(data) < 2 * 2^extreme_pairing_levels) {
    stop(paste0(
      "num_col: The (subset of) data is too small to perform ", extreme_pairing_levels,
      " levels of extreme pairing. Decrease 'extreme_pairing_levels'."
    ))
  }

  # Arrange by smallest, biggest, 2nd smallest, 2nd biggest, etc.
  # If the number of rows is unequal, the row with the smallest value is alone
  # This is done, as it is the one with the least effect on sum of values in a group
  data_sorted <- data # TODO is this renaming necessary?
  data_sorted[[local_tmp_index_var]] <- seq_len(nrow(data_sorted))
  data_sorted <- data_sorted %>%
    dplyr::arrange(!!as.name(num_col)) %>%
    rearrange(
      method = "pair_extremes",
      unequal_method = unequal_method,
      drop_rearrange_factor = FALSE,
      rearrange_factor_name = local_tmp_rearrange_var
    ) # Not actually a factor....

  # Perform the rearranging of extreme pairs again if specified
  # We do it on the rearrange factor levels, so a pair consists of the rows from two
  # rearrange factor levels
  if (extreme_pairing_levels > 1) {

    # print(paste0("n: ",n,
    #              " num_final_groups: ",num_final_groups,
    #              " extreme_pairing_levels: ",extreme_pairing_levels,
    #              " num_final_groups*2^extreme_pairing_levels: ", num_final_groups*2^extreme_pairing_levels,
    #              " nrows: ",nrow(data_sorted),
    #              " num rearrange factor levels: ", nlevels(data_sorted$rearrange_factor)))


    plyr::l_ply(seq_len(extreme_pairing_levels - 1), function(i) {
      if (length(optimize_for) > 1) {
        current_optimize_for <- optimize_for[[i]]
      } else {
        current_optimize_for <- optimize_for
      }

      if (current_optimize_for == "mean") {
        tmp_group_scores <- data_sorted %>%
          dplyr::group_by(!!as.name(local_tmp_rearrange_var)) %>%
          dplyr::summarize(group_aggr = sum(!!as.name(num_col)))
      } else if (current_optimize_for == "sd") {

        # Note: Only works for pairs, right?
        tmp_group_scores <- data_sorted %>%
          dplyr::group_by(!!as.name(local_tmp_rearrange_var)) %>%
          dplyr::summarize(group_aggr = sum(abs(diff(!!as.name(num_col)))))
      }

      if (!equal_nrows & unequal_method == "first") {

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
      tmp_rearrange <- tmp_group_scores_sorted %>%
        rearrange(
          method = "pair_extremes", unequal_method = unequal_method,
          drop_rearrange_factor = FALSE,
          rearrange_factor_name = local_tmp_rearrange_2_var
        ) %>%
        base_select(cols = c(local_tmp_rearrange_var, local_tmp_rearrange_2_var))

      data_sorted <<- data_sorted %>%
        dplyr::left_join(tmp_rearrange, by = local_tmp_rearrange_var) %>%
        dplyr::arrange(
          !!as.name(local_tmp_rearrange_2_var),
          !!as.name(local_tmp_rearrange_var),
          !!as.name(num_col)
        ) %>%
        base_deselect(cols = local_tmp_rearrange_var) %>%
        base_rename(
          before = local_tmp_rearrange_2_var,
          after = local_tmp_rearrange_var
        )
    })
  }

  if (isTRUE(group_by_rearrange_id)) {
    num_excessive_rearrange_levels <- nlevels(factor(
      data_sorted[[local_tmp_rearrange_var]])) %% num_final_groups
    has_excessive_rearrange_levels <- num_excessive_rearrange_levels > 0

    if (has_excessive_rearrange_levels) {
      if (!equal_nrows) {
        groups_to_distribute <- dplyr::case_when(
          unequal_method == "first" ~ as.numeric(1),
          unequal_method == "last" ~ as.numeric(floor(nrow(data_sorted) / 2) + 1),
          unequal_method == "middle" ~ as.numeric(ceiling(nrow(data_sorted) / 4) + 1)
        )
        if (num_excessive_rearrange_levels > 1) {
          # Add the remaining number of excess groups
          rearrange_levels <- unique(data_sorted[[local_tmp_rearrange_var]])
          choices <- rearrange_levels[rearrange_levels != groups_to_distribute]
          groups_to_distribute <- c(
            groups_to_distribute,
            sample(choices, num_excessive_rearrange_levels - 1)
          )
        }
      } else {
        groups_to_distribute <- sample(
          unique(data_sorted[[local_tmp_rearrange_var]]),
          num_excessive_rearrange_levels
        )
      }

      rows_to_distribute <- data_sorted[
        data_sorted[[local_tmp_rearrange_var]] %in% groups_to_distribute,
      ] %>%
        dplyr::arrange(!!as.name(num_col))

      data_sorted <- data_sorted[
        data_sorted[[local_tmp_rearrange_var]] %ni% groups_to_distribute,
      ]
    }

    # Create groups
    data_sorted <- data_sorted %>%
      group_uniques_(
        n = n,
        id_col = local_tmp_rearrange_var,
        method = method,
        col_name = local_tmp_groups_var
      )

    if (has_excessive_rearrange_levels) {
      # Calculate sums of the other pairs
      # Get the smallest (and second smallest if we have 2 rows to distribute)
      # TODO What if extreme_pairing_levels > 1 ???

      data_sorted_rank_summary <- create_rank_summary(
        data_sorted,
        levels_col = local_tmp_groups_var,
        num_col = num_col
      )

      if (nrow(data_sorted_rank_summary) >= nrow(rows_to_distribute)) {
        rows_to_distribute[[local_tmp_groups_var]] <- data_sorted_rank_summary %>%
          dplyr::filter(dplyr::row_number() %in% seq_len(nrow(rows_to_distribute))) %>%
          dplyr::pull(!!as.name(local_tmp_groups_var))
      } else {
        # Given that this will mostly happen with a few excess datapoints
        # the following might not be the optimal approach
        rows_to_distribute[[local_tmp_groups_var]] <- numerically_balanced_group_factor_(
          data = rows_to_distribute,
          n = n,
          num_col = num_col,
          method = method,
          # We definitely don't want to add to the biggest group
          # E.g. if 5 rows and n=3, then we want the two excess rows
          # to be combined with the smallest and second smallest
          # It's possible it would be more balanced with smallest, smallest
          # which is a thing to try (TODO)
          unequal_method = "last",
          extreme_pairing_levels = 1
        )

        renaming_levels_list <- rename_levels_by_reverse_rank_summary(
          data = rows_to_distribute,
          rank_summary = data_sorted_rank_summary,
          levels_col = local_tmp_groups_var,
          num_col = num_col
        )

        rows_to_distribute <- renaming_levels_list[["updated_data"]]
      }

      data_sorted <- data_sorted %>%
        dplyr::bind_rows(rows_to_distribute)
    }

    data_sorted <- data_sorted %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!as.name(local_tmp_index_var))

  } else {

    # If we are working with fold() and an unequal dataset
    # we want to make sure the unequal_method is followed.
    # So we remove the excess row and insert first/last after
    # we have reordered the pairs
    if (is_n_method && !equal_nrows) {
      if (unequal_method == "last") {
        excessive_row <- data_sorted[
          data_sorted[[local_tmp_rearrange_var]] ==
            max(data_sorted[[local_tmp_rearrange_var]]),
        ]
        data_sorted <- data_sorted[
          data_sorted[[local_tmp_rearrange_var]] !=
            max(data_sorted[[local_tmp_rearrange_var]]),
        ]
      } else if (unequal_method == "first") {
        excessive_row <- data_sorted[
          data_sorted[[local_tmp_rearrange_var]] ==
            min(data_sorted[[local_tmp_rearrange_var]]),
        ]
        data_sorted <- data_sorted[
          data_sorted[[local_tmp_rearrange_var]] !=
            min(data_sorted[[local_tmp_rearrange_var]]),
        ]
      }
    }

    # Sample rearrange levels
    # Order pairs by this shuffled order
    rearrange_factor_levels <- tibble::enframe(
      x = unique(data_sorted[[local_tmp_rearrange_var]]),
      name = NULL, value = local_tmp_rearrange_var
    ) %>%
      dplyr::ungroup() %>%
      dplyr::sample_frac()
    rearrange_factor_levels[[local_tmp_index_2_var]] <- seq_len(nrow(rearrange_factor_levels))
    data_sorted <- data_sorted %>%
      dplyr::left_join(rearrange_factor_levels,
        by = local_tmp_rearrange_var
      ) %>%
      dplyr::arrange(!!as.name(local_tmp_index_2_var))

    # Insert the excess row again
    if (isTRUE(is_n_method) && !isTRUE(equal_nrows)) {
      if (unequal_method == "last") {
        data_sorted <- data_sorted %>%
          dplyr::bind_rows(excessive_row)
      } else if (unequal_method == "first") {
        data_sorted <- excessive_row %>%
          dplyr::bind_rows(data_sorted)
      }
    }

    # Create the groups and get original order
    data_sorted <- data_sorted %>%
      group(
        n = n,
        method = method,
        col_name = local_tmp_groups_var,
        force_equal = force_equal
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!as.name(local_tmp_index_var))
  }

  data_sorted %>%
    dplyr::pull(!!as.name(local_tmp_groups_var)) %>%
    as.factor()
}
