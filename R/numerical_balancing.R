

#   __________________ #< 55d8c930d88d3c93ad6e3c22a6f18fa4 ># __________________
#   Numerically balanced group factor                                       ####


##  .................. #< b035f131de4beef545b4052e8c8be0ca ># ..................
##  Extreme pairing version                                                 ####


# Either called by fold() with a n_* method
# or partition() with the l_sizes method

numerically_balanced_group_factor_pairs_ <- function(
  data,
  n,
  num_col,
  method = "n_fill",
  unequal_method = "first",
  # Internal for now ("mean" or "sd" or (alternating) list)
  optimize_for = "mean",
  extreme_pairing_levels = 1,
  force_equal = FALSE) {

  if (unequal_method %ni% c("first","last")){
    stop('`unequal_method` must be in {first, last}.')
  }

  # Create unique local temporary index variable name
  local_tmp_index_var <- create_tmp_var(data, ".tmp_index_")
  local_tmp_groups_var <- create_tmp_var(data, ".groups")
  local_tmp_rearrange_var <- create_tmp_var(data, ".rearrange_factor_")

  # If method is n_*, it was called from fold()
  is_n_method <- substring(method, 1, 2) == "n_"

  # Check method is allowed
  if (!is_n_method && method != "l_sizes"){
    stop(paste0("method `", method, "` is currently not supported with `num_col` balancing."))
  }

  # Whether we have an equal number of rows (relevant for pairing)
  equal_nrows <- nrow(data) %% 2 == 0

  # Check if we have enough data for pairwise folding
  # or if we are running partitioning (l_sizes)

  # In partitioning, we group directly on the rows, as that is the way to get
  # those specific group sizes
  # In folding, we group the final pair indices (rearrange IDs) (when enough data points)
  # When the number of rearrange IDs do not divide appropriately, why we have a distribution step
  # for "excessive" rows.

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

  # Save the order of the data frame
  data[[local_tmp_index_var]] <- seq_len(nrow(data))

  # Check when grouping on rows
  # If method="l_sizes" for instance, we want the last pairing to have at least one pair (two sub pairs)
  if (!isTRUE(group_by_rearrange_id) &&
      extreme_pairing_levels > 1 &&
      nrow(data) < 2 * 2 ^ extreme_pairing_levels) {
    stop(
      paste0(
        "`num_col`: The (subset of) data is too small to perform ",
        extreme_pairing_levels,
        " levels of extreme pairing. Decrease `extreme_pairing_levels`."
      )
    )
  }

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
    # Check whether we have extra rearrange IDs to distribute the rows of
    excess_list <-
      get_excessive_rows_(
        data = data,
        num_col = num_col,
        num_final_groups = num_final_groups,
        final_rearrange_var = final_rearrange_var,
        nrows_divisible = equal_nrows,
        nrows_nondivisible_id = dplyr::case_when(
          unequal_method == "first" ~ as.numeric(1),
          unequal_method == "last" ~ as.numeric(length(unique(data[[final_rearrange_var]])))
        ),
        optimize_for = optimize_for
      )
    has_excessive_rearrange_ids <- excess_list[["has_excessive_rearrange_ids"]]
    if (isTRUE(has_excessive_rearrange_ids)){
      data <- excess_list[["data"]]
      rows_to_distribute <- excess_list[["rows_to_distribute"]]
    }

    # Group the IDs randomly
    data <- data %>%
      group_uniques_randomly_(
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
        num_col = num_col,
        fn = get_optimization_measure_fn(
          optimize_for=optimize_for)
      )

      if (nrow(data_rank_summary) >= nrow(rows_to_distribute)) {
        # When there are fewer rows to distribute than number of groups
        # Add group IDs to the excess rows
        # such that the smallest groups get the additional rows
        rows_to_distribute[[local_tmp_groups_var]] <- data_rank_summary %>%
          head(nrow(rows_to_distribute)) %>%
          dplyr::pull(!!as.name(local_tmp_groups_var))

      } else {
        # When there are more rows to distribute than number of groups
        # we run the balancing again
        # Given that this will mostly happen with a few excess datapoints
        # the following might not be the optimal approach
        rows_to_distribute[[local_tmp_groups_var]] <- numerically_balanced_group_factor_pairs_(
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
    # Either using in partition()
    # or fold() with small number of data points <n*2

    # If we are working with fold() and an unequal dataset
    # we want to make sure the unequal_method is followed.
    # So we remove the excess row and insert first/last after
    # we have reordered the pairs
    if (is_n_method && !equal_nrows) {
      if (unequal_method == "last") {
        excessive_row <- data[
          data[[final_rearrange_var]] ==
            max(factor_to_num(data[[final_rearrange_var]])),
        ]
        data <- data[
          data[[final_rearrange_var]] !=
            max(factor_to_num(data[[final_rearrange_var]])),
        ]
      } else if (unequal_method == "first") {
        excessive_row <- data[
          data[[final_rearrange_var]] ==
            min(factor_to_num(data[[final_rearrange_var]])),
        ]
        data <- data[
          data[[final_rearrange_var]] !=
            min(factor_to_num(data[[final_rearrange_var]])),
        ]
      }
    }

    # Shuffle hierarchy of pairs and pair members
    # We're grouping the rows so we shuffle the indices as well
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

    # Create the groups
    data <- data %>%
      group(
        n = n,
        method = method,
        col_name = local_tmp_groups_var,
        force_equal = force_equal
      ) %>%
      dplyr::ungroup()

    # Restore original order
    data <- data %>%
      dplyr::arrange(!!as.name(local_tmp_index_var))
  }

  # Extract grouping factor
  data %>%
    dplyr::pull(!!as.name(local_tmp_groups_var)) %>%
    as.factor()
}


##  .................. #< 82e8f7fb4223b2bacb152fe18c916ee0 ># ..................
##  Extreme triplets version                                                ####


# Similar but using triplets instead of pairs
# Likely less balanced but allows creating more unique group columns
numerically_balanced_group_factor_triplets_ <- function(
  data,
  n,
  num_col,
  method = "n_fill",
  # Internal for now ("mean" or "sd" or (alternating) list)
  optimize_for = "mean",
  extreme_grouping_levels = 1,
  force_equal = FALSE) {

  # Create unique local temporary index variable name
  local_tmp_index_var <- create_tmp_var(data, ".tmp_index_")
  local_tmp_groups_var <- create_tmp_var(data, ".groups")
  local_tmp_rearrange_var <- create_tmp_var(data, ".rearrange_factor_")

  # If method is n_*, it was called from fold()
  is_n_method <- substring(method, 1, 2) == "n_"

  # Check method is allowed
  if (!is_n_method && method != "l_sizes"){
    stop(paste0("method `", method, "` is currently not supported with `num_col` balancing."))
  }

  # Whether we have an equal number of rows (relevant for pairing)
  nrows_divisible_by_3 <- nrow(data) %% 3 == 0

  # Check if we have enough data for triplet-wise folding
  # or if we are running partitioning (l_sizes)

  # In partitioning, we group directly on the rows, as that is the way to get
  # those specific group sizes
  # In folding, we group the final triplet indices (rearrange IDs) (when enough data points)
  # When the number of rearrange IDs do not divide appropriately, why we have a distribution step
  # for "excessive" rows.

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
    # If we have enough data to create >=n triplets
    # we group the pairs, otherwise the rows
    group_by_rearrange_id <- nrow(data) >= n * 3
  }

  # Check if extreme_grouping_levels is too big for the dataset
  # when grouping on rearrange IDs
  # TODO: Ensure this is never seen by the end user!
  if (isTRUE(group_by_rearrange_id) && extreme_grouping_levels > 1 &&
      nrow(data) < num_final_groups * 3^extreme_grouping_levels) {
    stop(paste0(
      "`num_col`: The (subset of) data is too small to perform ",
      extreme_grouping_levels,
      " levels of extreme triplet groupings. Decrease `extreme_grouping_levels`."
    ))
  }

  # Find a suitable `extreme_grouping_levels` setting
  if (extreme_grouping_levels > 1) {
    old_extreme_grouping_levels <- extreme_grouping_levels
    while (nrow(data) < 3 * 3 ^ extreme_grouping_levels &&
           extreme_grouping_levels > 1) {
      extreme_grouping_levels <- extreme_grouping_levels - 1
    }
    if (extreme_grouping_levels != old_extreme_grouping_levels) {
      message(
        paste0(
          "`extreme_grouping_levels` was reduced to ",
          extreme_grouping_levels,
          " during extreme triplets numerical balancing."
        )
      )
    }
  }

  # Save the order of the data frame
  data[[local_tmp_index_var]] <- seq_len(nrow(data))

  # Unequal methods are returned as first group in triplet_extremes
  data <- data %>%
    rearrr::triplet_extremes(
      col = num_col,
      unequal_method_1 = "min",
      unequal_method_2 = c("min", "min"),
      num_groupings = extreme_grouping_levels,
      balance = optimize_for,
      shuffle_members = FALSE,
      shuffle_triplet = FALSE,
      factor_name = local_tmp_rearrange_var
    )

  # Names of the pairing factors
  if (extreme_grouping_levels == 1){
    triplet_grouping_factors <- local_tmp_rearrange_var
  } else {
    triplet_grouping_factors <- paste0(local_tmp_rearrange_var, "_", seq_len(extreme_grouping_levels))
  }
  final_rearrange_var <- tail(triplet_grouping_factors, 1)

  # If we are grouping the rearrange IDs
  # I.e. for n_* methods with >= 3*n data points
  if (isTRUE(group_by_rearrange_id)) {

    # Check whether we have extra rearrange IDs to distribute the rows of
    excess_list <-
      get_excessive_rows_(
        data = data,
        num_col = num_col,
        num_final_groups = num_final_groups,
        final_rearrange_var = final_rearrange_var,
        nrows_divisible = nrows_divisible_by_3,
        nrows_nondivisible_id = 1,
        optimize_for = optimize_for
      )
    has_excessive_rearrange_ids <- excess_list[["has_excessive_rearrange_ids"]]
    if (isTRUE(has_excessive_rearrange_ids)){
      data <- excess_list[["data"]]
      rows_to_distribute <- excess_list[["rows_to_distribute"]]
    }

    # Group the IDs randomly
    data <- data %>%
      group_uniques_randomly_(
        n = n,
        id_col = final_rearrange_var,
        method = method,
        col_name = local_tmp_groups_var
      )

    if (has_excessive_rearrange_ids) {
      # Calculate sums of the other triplet
      # Get the n smallest (where n = rows to distribute)
      # TODO What if extreme_grouping_levels > 1 ???
      data_rank_summary <- create_rank_summary(
        data = data,
        levels_col = local_tmp_groups_var,
        num_col = num_col,
        fn = get_optimization_measure_fn(
          optimize_for=optimize_for)
      )

      if (nrow(data_rank_summary) >= nrow(rows_to_distribute)) {
        # When there are fewer rows to distribute than number of groups
        # Add group IDs to the excess rows
        # such that the smallest groups get the additional rows
        rows_to_distribute[[local_tmp_groups_var]] <- data_rank_summary %>%
          head(nrow(rows_to_distribute)) %>%
          dplyr::pull(!!as.name(local_tmp_groups_var))

      } else {
        # When there are more rows to distribute than number of groups
        # we run the balancing again
        # Given that this will mostly happen with a few excess datapoints
        # the following might not be the optimal approach
        rows_to_distribute[[local_tmp_groups_var]] <- numerically_balanced_group_factor_triplets_(
          data = rows_to_distribute,
          n = n,
          num_col = num_col,
          method = method,
          extreme_grouping_levels = 1
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
    # Either using in partition()
    # or fold() with small number of data points <n*3

    # Remove the excess rows and insert first after
    # we have reordered the triplets

    if (is_n_method &&
        # We can use `nrows_divisible_by_3` as it should not be here
        # for an n_method if we have enough rows for extreme triplet grouping
        !isTRUE(nrows_divisible_by_3)) {
      excessive_rows <- data[data[[final_rearrange_var]] ==
                               min(factor_to_num(data[[final_rearrange_var]])),] %>%
        dplyr::sample_frac()
      data <- data[data[[final_rearrange_var]] !=
                     min(factor_to_num(data[[final_rearrange_var]])),]
    }

    # Shuffle hierarchy of pairs and pair members
    # We're grouping the rows so we shuffle the indices as well
    shuffle_cols <- c(rev(triplet_grouping_factors), local_tmp_index_var)
    data <- rearrr::shuffle_hierarchy(
      data = data,
      group_cols = shuffle_cols,
      leaf_has_groups = FALSE
    )

    # Insert the excess row(s) again
    if (isTRUE(is_n_method) && !isTRUE(nrows_divisible_by_3)) {
      data <- excessive_rows %>%
          dplyr::bind_rows(data)
    }

    # Create the groups
    data <- data %>%
      group(
        n = n,
        method = method,
        col_name = local_tmp_groups_var,
        force_equal = force_equal
      ) %>%
      dplyr::ungroup()

    # Restore original order
    data <- data %>%
      dplyr::arrange(!!as.name(local_tmp_index_var))
  }

  # Extract grouping factor
  data %>%
    dplyr::pull(!!as.name(local_tmp_groups_var)) %>%
    as.factor()
}

##  .................. #< 343b07a0c0c09df6eb6f87ce74366fb9 ># ..................
##  Utils                                                                   ####


get_excessive_rows_ <- function(data, num_col, num_final_groups, final_rearrange_var, nrows_divisible,
                                nrows_nondivisible_id, optimize_for){

  # Find whether and how many IDs to redistribute
  # Also find whether to redistribute the smallest ID
  redistribution_info <-
    get_redistribution_info(
      data = data,
      num_col = num_col,
      num_final_groups = num_final_groups,
      final_rearrange_var = final_rearrange_var,
      optimize_for = optimize_for
    )
  has_excessive_rearrange_ids = redistribution_info[["has_excessive_rearrange_ids"]]
  redistribute_smallest_group = redistribution_info[["redistribute_smallest_group"]]
  redistribute_largest_group = redistribution_info[["redistribute_largest_group"]]
  num_excessive_rearrange_ids = redistribution_info[["num_excessive_rearrange_ids"]]
  smallest_group_id = redistribution_info[["smallest_group_id"]]
  largest_group_id = redistribution_info[["largest_group_id"]]

  # We have extra rearrange IDs to distribute the rows of
  if (has_excessive_rearrange_ids) {
    ids_to_distribute <- numeric(0)

    if (!nrows_divisible ||
        isTRUE(redistribute_smallest_group)) {
      if (isTRUE(redistribute_smallest_group)) {
        ids_to_distribute <- smallest_group_id
      }
      if (isTRUE(redistribute_largest_group)) {
        ids_to_distribute <- c(ids_to_distribute, largest_group_id)
      }
      if (!nrows_divisible) {
        if (length(ids_to_distribute) == 0 ||
            (nrows_nondivisible_id %ni% ids_to_distribute &&
             num_excessive_rearrange_ids > length(ids_to_distribute))) {
          ids_to_distribute <- c(ids_to_distribute, nrows_nondivisible_id)
        }
      }
      if (num_excessive_rearrange_ids > length(ids_to_distribute)) {
        # Add indices for the remaining number of excess IDs
        rearrange_ids <- unique(data[[final_rearrange_var]])
        possible_choices <-
          rearrange_ids[rearrange_ids %ni% ids_to_distribute]
        ids_to_distribute <- c(
          ids_to_distribute,
          sample(
            possible_choices,
            num_excessive_rearrange_ids - length(ids_to_distribute)
          )
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

    return(list(
      "has_excessive_rearrange_ids" = has_excessive_rearrange_ids,
      "data" = data,
      "rows_to_distribute" = rows_to_distribute
    ))

  } else {
    return(list(
      "has_excessive_rearrange_ids" = has_excessive_rearrange_ids
    ))
  }
}


get_redistribution_info <- function(data, num_col, num_final_groups, final_rearrange_var, optimize_for){

  smallest_group_id <- NULL
  largest_group_id <- NULL
  redistribute_smallest_group <- FALSE
  redistribute_largest_group <- FALSE
  num_ids <- nlevels(data[[final_rearrange_var]])
  num_possible_to_redistribute <- num_ids - num_final_groups

  # Calculate how many IDs we need to redistribute
  # if we redistribute 1 or 2 IDs
  to_redistribute_if_one_is_redistributed <- calculate_excessive_ids(
    data,
    id_col = final_rearrange_var,
    num_groups = num_final_groups,
    num_removed = 1)
  to_redistribute_if_two_are_redistributed <- calculate_excessive_ids(
    data,
    id_col = final_rearrange_var,
    num_groups = num_final_groups,
    num_removed = 2)

  if (to_redistribute_if_one_is_redistributed <= num_possible_to_redistribute &&
      num_ids > max(2, num_final_groups)){

    # If the smallest extreme group in the final_rearrange_var has 1+ standard deviations
    # up to the second smallest extreme group, we redistribute that
    aggr_list <- aggregate_groups_(
      data = data,
      final_rearrange_var = final_rearrange_var,
      num_col = num_col,
      optimize_for = optimize_for
    )
    sd_aggregate <- aggr_list[["sd_aggregate"]]
    two_smallest <- aggr_list[["two_smallest"]]
    two_largest <- aggr_list[["two_largest"]]
    smallest_group_id <- aggr_list[["smallest_group_id"]]
    largest_group_id <- aggr_list[["largest_group_id"]]

    if (two_smallest[[2]] - two_smallest[[1]] > sd_aggregate){
      # We should redistribute the smallest one
      redistribute_smallest_group <- TRUE
    }

    if (two_largest[[2]] - two_largest[[1]] > sd_aggregate &&
        (!isTRUE(redistribute_smallest_group) ||
         to_redistribute_if_two_are_redistributed <= num_possible_to_redistribute)) {
      # We should redistribute the largest one
      redistribute_largest_group <- TRUE
    }
  }

  # Calculate the number of excessive IDs to distribute
  # num_extreme: Either 0, 1 or 2
  num_extreme <- sum(c(as.integer(isTRUE(redistribute_smallest_group)),
                       as.integer(isTRUE(redistribute_largest_group))))
  num_excessive_rearrange_ids <- calculate_excessive_ids(
    data,
    id_col = final_rearrange_var,
    num_groups = num_final_groups,
    num_removed = num_extreme)
  has_excessive_rearrange_ids <- num_excessive_rearrange_ids > 0

  list(
    "has_excessive_rearrange_ids" = has_excessive_rearrange_ids,
    "redistribute_smallest_group" = redistribute_smallest_group,
    "redistribute_largest_group" = redistribute_largest_group,
    "num_excessive_rearrange_ids" = num_excessive_rearrange_ids,
    "smallest_group_id" = smallest_group_id,
    "largest_group_id" = largest_group_id
  )
}


aggregate_groups_ <- function(data, final_rearrange_var, num_col, optimize_for){
  # Note: This is mainly a separate function to allow testing
  aggr_fn <- get_optimization_measure_fn(optimize_for=optimize_for)
  tmp_aggr_var <- create_tmp_var(data, tmp_var = '.__aggr__')
  aggregates <- data %>%
    dplyr::group_by(!!as.name(final_rearrange_var)) %>%
    dplyr::summarise(!!tmp_aggr_var := aggr_fn(!!as.name(num_col))) %>%
    dplyr::arrange(!!as.name(tmp_aggr_var))
  if (nrow(aggregates) < 2){
    stop("`aggregate_groups_` found only a single group. `data` must contain multiple groups.")
  }
  sd_aggregate <- sd(aggregates[[tmp_aggr_var]])
  two_smallest <- unlist(head(aggregates, 2)[, tmp_aggr_var], use.names = FALSE)
  two_largest <- unlist(tail(aggregates, 2)[, tmp_aggr_var], use.names = FALSE)
  smallest_group_id <- as.numeric(as.character(aggregates[[final_rearrange_var]][[1]]))
  largest_group_id <- as.numeric(as.character(tail(aggregates, 1)[[final_rearrange_var]][[1]]))

  list(
    'sd_aggregate' = sd_aggregate,
    'two_smallest' = two_smallest,
    'two_largest' = two_largest,
    'smallest_group_id' = smallest_group_id,
    'largest_group_id' = largest_group_id
  )
}


calculate_excessive_ids <- function(data, id_col, num_groups, num_removed=0){
  # num_removed: Number of IDs already decided to be excessive
  num_removed + ((nlevels(factor(data[[id_col]])) - num_removed) %% num_groups)
}


get_optimization_measure_fn <- function(optimize_for){
  list(
    'mean' = sum,
    'spread' = spread_fn <- function(x) {
      if (length(x) < 2) {
        return(0)
      }
      sd(x)
    },
    'min' = min,
    'max' = max
  )[[optimize_for]]
}
