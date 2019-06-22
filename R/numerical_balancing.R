
numerically_balanced_group_factor_ <- function(data, n, num_col, method="n_fill",
                                               unequal_method="first", extreme_pairing_levels=1,
                                               force_equal=FALSE) {

  # Create unique local temporary index variable name
  local_tmp_index_var <- create_tmp_var(data, ".tmp_index_")
  local_tmp_index_2_var <- create_tmp_var(data, ".tmp_index_2_")
  local_tmp_groups_var <- create_tmp_var(data, ".groups")

  # TODO Add "rearrange_factor" var name with create_tmp_var

  # If method is n_*, we are doing folding
  is_n_method <- substring(method, 1, 2) == "n_"

  equal_nrows <- nrow(data) %% 2 == 0

  # Check if we have enough data for pairwise folding
  # or if we are running partitioning (l_sizes)
  if (method == "l_sizes"){
    group_by_rearrange_id <- FALSE
  } else if (is_n_method){
    if (length(n) > 1){
      stop(paste0("n contained more than one element with method '",method,"'."))}
    if (n > 1 && nrow(data) < n * 2) {
      group_by_rearrange_id <- FALSE
    } else {
        group_by_rearrange_id <- TRUE
    }
  } else {
    stop(paste0("method '",method,"' is currently not supported with num_col balancing."))
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

  # Perform the rearranging of extreme pairs again if specified
  # We do it on the rearrange factor levels, so a pair consists of the rows from two
  # rearrange factor levels
  if (extreme_pairing_levels > 1){

    plyr::l_ply(1:(extreme_pairing_levels-1), function(i){
      tmp_group_scores <- data_sorted %>%
        dplyr::group_by(.data$rearrange_factor) %>%
        dplyr::summarize(group_aggr = sum(!!as.name(num_col)))

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
        rearrange(method = "pair_extremes", unequal_method = unequal_method,
                  drop_rearrange_factor = FALSE,
                  rearrange_factor_name = "rearrange_factor_2") %>%
        dplyr::select(.data$rearrange_factor, .data$rearrange_factor_2)

      data_sorted <<- data_sorted %>%
        dplyr::left_join(tmp_rearrange, by = "rearrange_factor") %>%
        dplyr::arrange(.data$rearrange_factor_2, .data$rearrange_factor, !!as.name(num_col)) %>%
        dplyr::select(-.data$rearrange_factor) %>%
        dplyr::rename(rearrange_factor = .data$rearrange_factor_2)

    })

  }

  if(isTRUE(group_by_rearrange_id)){

    equal_num_rearrange_levels <- nlevels(factor(data_sorted$rearrange_factor)) %% n == 0

    if (!equal_num_rearrange_levels){

      if (!equal_nrows){
        group_to_distribute <- dplyr::case_when(
          unequal_method == "first" ~ as.numeric(1),
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

    if (!equal_num_rearrange_levels){
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

    rearrange_factor_levels <- tibble::enframe(
        unique(data_sorted[["rearrange_factor"]]),
        name=NULL, value="rearrange_levels") %>%
        dplyr::ungroup() %>%
        dplyr::sample_frac()
    rearrange_factor_levels[[local_tmp_index_2_var]] <- 1:nrow(rearrange_factor_levels)
    data_sorted <- data_sorted %>%
      dplyr::left_join(rearrange_factor_levels,
                       by=c("rearrange_factor"="rearrange_levels")) %>%
      dplyr::arrange(!!as.name(local_tmp_index_2_var)) %>%
      group(n=n, method = method, col_name = local_tmp_groups_var,
            force_equal = force_equal) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!as.name(local_tmp_index_var))

  }

  data_sorted %>%
    dplyr::pull(!!as.name(local_tmp_groups_var)) %>%
    as.factor()

}


