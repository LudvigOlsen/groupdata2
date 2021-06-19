

# TODO Currently, it is very confusing what is going on and why
# So make an overview before the code and up the commenting game a lot
# TODO Answer: why is the "grouped by ID" different every time? No shuffling in that?

# Either called by fold() with a n_* method
# or partition() with the l_sizes method

numerically_balanced_group_factor_ <- function(
  data,
  n,
  num_col,
  method = "n_fill",
  unequal_method = "first",
  # Internal for now ("mean" or "sd" or (alternating) list)
  optimize_for = "mean",
  extreme_pairing_levels = 1,
  force_equal = FALSE) {

  # Create unique local temporary index variable name
  local_tmp_index_var <- create_tmp_var(data, ".tmp_index_")
  local_tmp_groups_var <- create_tmp_var(data, ".groups")
  local_tmp_rearrange_var <- create_tmp_var(data, ".rearrange_factor_")

  # If method is n_*, it was called from fold()
  is_n_method <- substring(method, 1, 2) == "n_"

  # Check method is allowed
  if (!is_n_method && method != "l_sizes"){
    stop(paste0("method `", method, "` is currently not supported with num_col balancing."))
  }

  # Whether we have an equal number of rows (relevant for pairing)
  equal_nrows <- nrow(data) %% 2 == 0

  # Check if we have enough data for pairwise folding
  # or if we are running partitioning (l_sizes)

  # TODO: explain group_by_rearrange_id
  # In partitioning, we group directly on the rows, as that is the way to get
  # those specific group sizes
  # In folding, we group the final pair indices (rearrange IDs) (when enough data points)
  # When the number of rearrange IDs do not divide appropriately, why we have a distribution step
  # for "excessive" rows. (THIS MIGHT BE WRONG, CHECK!)

  if (method == "l_sizes") {
    group_by_rearrange_id <- FALSE
    num_final_groups <- length(n)
  } else if (isTRUE(is_n_method)) {
    if (length(n) > 1) {
      stop(paste0("`n` contained more than one element with method `", method, "`."))
    }
    if (n < 1) {
      n <- ceiling(nrow(data) / convert_percentage_(n, data))
    }
    num_final_groups <- n
    # If we have enough data to create >=n pairs
    # we group the pairs, otherwise the rows
    group_by_rearrange_id <- nrow(data) >= n * 2
  }

  # Check if extreme_pairing_levels is too big for the dataset
  # when grouping on rearrange IDs
  # TODO: Check that this calculation holds in all use cases
  # e.g. is it enough with one group level per fold when isTRUE(group_by_rearrange_id)?
  if (group_by_rearrange_id && extreme_pairing_levels > 1 &&
    nrow(data) < num_final_groups * 2^extreme_pairing_levels) {
    stop(paste0(
      "`num_col`: The (subset of) data is too small to perform ",
      extreme_pairing_levels,
      " levels of extreme pairing. Decrease `extreme_pairing_levels`."
    ))
  }

  # Check when grouping on rows
  # If method="l_sizes" for instance, we want the last pairing to have at least one pair (two sub pairs)
  if (!group_by_rearrange_id && extreme_pairing_levels > 1 &&
    nrow(data) < 2 * 2^extreme_pairing_levels) {
    stop(paste0(
      "`num_col`: The (subset of) data is too small to perform ", extreme_pairing_levels,
      " levels of extreme pairing. Decrease `extreme_pairing_levels`."
    ))
  }

  # Save the order of the data frame
  data[[local_tmp_index_var]] <- seq_len(nrow(data))

  # Arrange by smallest, biggest, 2nd smallest, 2nd biggest, etc.
  # If the number of rows is unequal, the row with the smallest value is alone
  # This is done, as it is the one with the least effect on sum of values in a group
  data <- data %>%
    rearrr::pair_extremes(
      col = num_col,
      unequal_method = unequal_method,
      num_pairings = extreme_pairing_levels,
      balance = optimize_for,
      shuffle_members = FALSE,
      shuffle_pairs = FALSE,
      factor_name = local_tmp_rearrange_var
    )

  # Names of the pairing factors
  if (extreme_pairing_levels == 1){
    pairing_factors <- local_tmp_rearrange_var
  } else {
    pairing_factors <- paste0(local_tmp_rearrange_var, "_", seq_len(extreme_pairing_levels))
  }
  final_rearrange_var <- tail(pairing_factors, 1)

  # If we are grouping the rearrange IDs
  # I.e. for n_* methods with >= 2*n data points
  if (isTRUE(group_by_rearrange_id)) {
    num_excessive_rearrange_ids <-
      nlevels(factor(data[[final_rearrange_var]])) %% num_final_groups
    has_excessive_rearrange_ids <- num_excessive_rearrange_ids > 0

    # We have extra rearrange IDs to distribute the rows of
    if (has_excessive_rearrange_ids) {
      if (!equal_nrows) {
        # Add index of unequal row to a list of IDs to distribute
        ids_to_distribute <- dplyr::case_when(
          unequal_method == "first" ~ as.numeric(1),
          unequal_method == "last" ~ as.numeric(floor(nrow(data) / 2) + 1),
          unequal_method == "middle" ~ as.numeric(ceiling(nrow(data) / 4) + 1)
        )
        if (num_excessive_rearrange_ids > 1) {
          # Add indices for the remaining number of excess IDs
          rearrange_ids <- unique(data[[final_rearrange_var]])
          possible_choices <- rearrange_ids[rearrange_ids != ids_to_distribute]
          ids_to_distribute <- c(
            ids_to_distribute,
            sample(possible_choices, num_excessive_rearrange_ids - 1)
          )
        }
      } else {
        ids_to_distribute <- sample(
          unique(data[[final_rearrange_var]]),
          num_excessive_rearrange_ids
        )
      }

      # Extract the actual rows to distribute
      rows_to_distribute <- data[
        data[[final_rearrange_var]] %in% ids_to_distribute,
      ] %>%
        dplyr::arrange(!!as.name(num_col))

      # Remove the rows that will be distributed after grouping
      data <- data[
        data[[final_rearrange_var]] %ni% ids_to_distribute,
      ]
    }

    # Group the IDs
    data <- data %>%
      group_uniques_(
        n = n,
        id_col = final_rearrange_var,
        method = method,
        col_name = local_tmp_groups_var
      )

    if (has_excessive_rearrange_ids) {
      # Calculate sums of the other pairs
      # Get the smallest (and second smallest if we have 2 rows to distribute)
      # TODO What if extreme_pairing_levels > 1 ???
      data_rank_summary <- create_rank_summary(
        data = data,
        levels_col = local_tmp_groups_var,
        num_col = num_col
      )

      if (nrow(data_rank_summary) >= nrow(rows_to_distribute)) {
        # When there are fewer rows to distribute than number of groups
        # Add group IDs to the excess rows
        # such that the smallest groups get the additional rows
        rows_to_distribute[[local_tmp_groups_var]] <- data_rank_summary %>%
          dplyr::filter(dplyr::row_number() %in% seq_len(nrow(rows_to_distribute))) %>%
          dplyr::pull(!!as.name(local_tmp_groups_var))
      } else {
        # When there are more rows to distribute than number of groups
        # we run the balancing again
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

        # Rename the groups for the excess rows such that
        # the biggest group becomes part of the smallest group in the data
        renaming_levels_list <- rename_levels_by_reverse_rank_summary(
          data = rows_to_distribute,
          rank_summary = data_rank_summary,
          levels_col = local_tmp_groups_var,
          num_col = num_col
        )

        # Extract regrouped rows
        rows_to_distribute <- renaming_levels_list[["updated_data"]]
      }

      # Add the now distributed rows
      data <- data %>%
        dplyr::bind_rows(rows_to_distribute)
    }

    # Reorder the data to the original order
    data <- data %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!as.name(local_tmp_index_var))

  } else {

    # If we are working with fold() and an unequal dataset
    # we want to make sure the unequal_method is followed.
    # So we remove the excess row and insert first/last after
    # we have reordered the pairs
    if (is_n_method && !equal_nrows) {
      if (unequal_method == "last") {
        excessive_row <- data[
          data[[final_rearrange_var]] ==
            max(data[[final_rearrange_var]]),
        ]
        data <- data[
          data[[final_rearrange_var]] !=
            max(data[[final_rearrange_var]]),
        ]
      } else if (unequal_method == "first") {
        excessive_row <- data[
          data[[final_rearrange_var]] ==
            min(data[[final_rearrange_var]]),
        ]
        data <- data[
          data[[final_rearrange_var]] !=
            min(data[[final_rearrange_var]]),
        ]
      }
    }

    # Shuffle hierarchy of pairs and pair members
    shuffle_cols <- c(rev(pairing_factors), local_tmp_index_var)
    data <- rearrr::shuffle_hierarchy(
      data = data,
      group_cols = shuffle_cols,
      leaf_has_groups = FALSE
    )

    # Insert the excess row again
    if (isTRUE(is_n_method) && !isTRUE(equal_nrows)) {
      if (unequal_method == "last") {
        data <- data %>%
          dplyr::bind_rows(excessive_row)
      } else if (unequal_method == "first") {
        data <- excessive_row %>%
          dplyr::bind_rows(data)
      }
    }

    # Create the groups and get original order
    data <- data %>%
      group(
        n = n,
        method = method,
        col_name = local_tmp_groups_var,
        force_equal = force_equal
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!as.name(local_tmp_index_var))
  }

  data %>%
    dplyr::pull(!!as.name(local_tmp_groups_var)) %>%
    as.factor()
}
