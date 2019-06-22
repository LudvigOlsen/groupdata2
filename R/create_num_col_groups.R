
#' @importFrom dplyr n
create_num_col_groups <- function(data, n, num_col, cat_col=NULL, id_col=NULL, col_name,
                                  id_aggregation_fn = sum, method="n_fill",
                                  unequal_method="first", force_equal=FALSE,
                                  pre_randomize=TRUE, randomize_pairs = TRUE
                                  ) {

  # Run some checks
  # TODO: This was copied from group_factor, not sure we need it here
  n <- check_convert_check_(data=data, n=n, method=method, force_equal=force_equal,
                       allow_zero=FALSE, descending=FALSE,
                       remove_missing_starts=FALSE,
                       starts_col = NULL)

  # Sample dataframe before use.
  if (isTRUE(pre_randomize)){

    # Create unique local temporary index
    local_tmp_index_var <- create_tmp_var(data)
    data[[local_tmp_index_var]] <- 1:nrow(data)

    data <- data %>%
      dplyr::sample_frac(1)
  }

  # Init rank summary for balanced joining of fold ID's
  # when cat_col is specified
  rank_summary <- NULL

  # If cat_col is not NULL
  if (!is.null(cat_col)){

    # If id_col is not NULL
    if (!is.null(id_col)){

      # aggregate val col per ID
      ids_aggregated <- data %>%
        group_by(!! as.name(cat_col), !! as.name(id_col)) %>%
        dplyr::summarize(aggr_val = id_aggregation_fn(!! as.name(num_col))) %>%
        dplyr::ungroup()

      # Find groups for each category
      ids_grouped <- plyr::ldply(unique(ids_aggregated[[cat_col]]), function(category){
        ids_for_cat <- ids_aggregated %>%
          dplyr::filter(!!as.name(cat_col) == category)
        ids_for_cat$._new_groups_ <- numerically_balanced_group_factor_(
          ids_for_cat, n=n, num_col="aggr_val",
          method=method, unequal_method=unequal_method,
          randomize_pairs=randomize_pairs)

        # Rename groups to be combined in the most balanced way
        if (is.null(rank_summary)){
          rank_summary <<- create_rank_summary(ids_for_cat,
                                               levels_col = "._new_groups_",
                                               num_col="aggr_val")
        } else {
          renaming_levels_list <- rename_levels_by_reverse_rank_summary(
            data=ids_for_cat, rank_summary=rank_summary,
            levels_col="._new_groups_", num_col="aggr_val")
          rank_summary <<- renaming_levels_list[["updated_rank_summary"]]
          ids_for_cat <- renaming_levels_list[["updated_data"]]
        }

        ids_for_cat %>%
          dplyr::select(-c(.data$aggr_val))
      })

      # Transfer groups to data
      data <- data %>%
        dplyr::inner_join(ids_grouped, by=c(cat_col, id_col))

    # If id_col is NULL
    } else {

      # For each category in cat_col
      # .. create value balanced group factor

      # Find groups for each category
      data <- plyr::ldply(unique(data[[cat_col]]), function(category){
        data_for_cat <- data %>%
          dplyr::filter(!!as.name(cat_col) == category)
        data_for_cat$._new_groups_ <- numerically_balanced_group_factor_(
          data_for_cat, n=n, num_col=num_col, method=method,
          unequal_method=unequal_method,
          randomize_pairs=randomize_pairs)

        # Rename groups to be combined in the most balanced way
        if (is.null(rank_summary)){
          rank_summary <<- create_rank_summary(data_for_cat,
                                               levels_col = "._new_groups_",
                                               num_col = num_col)
        } else {
          renaming_levels_list <- rename_levels_by_reverse_rank_summary(
            data = data_for_cat, rank_summary = rank_summary,
            levels_col = "._new_groups_", num_col = num_col)
          rank_summary <<- renaming_levels_list[["updated_rank_summary"]]
          data_for_cat <- renaming_levels_list[["updated_data"]]
        }
        data_for_cat
      })

    }

  # If cat_col is NULL
  } else {

    # If id_col is not NULL
    if (!is.null(id_col)){

      # Aggregate num_col for IDs with the passed id_aggregation_fn
      # Create value balanced group factor based on aggregated values
      # Join the groups back into the data

      # aggregate val col per ID
      ids_aggregated <- data %>%
        group_by(!! as.name(id_col)) %>%
        dplyr::summarize(aggr_val = id_aggregation_fn(!!as.name(num_col))) %>%
        dplyr::ungroup()

      # Create group factor
      ids_aggregated$._new_groups_ <- numerically_balanced_group_factor_(
        ids_aggregated, n=n, num_col = "aggr_val", method=method,
        unequal_method=unequal_method,
        randomize_pairs=randomize_pairs)
      ids_aggregated$aggr_val <- NULL

      # Transfer groups to data
      data <- data %>%
        dplyr::inner_join(ids_aggregated, by=c(id_col))

    # If id_col is NULL
    } else {

      # Add group factor
      data$._new_groups_ <- numerically_balanced_group_factor_(data, n=n,
                                                       num_col = num_col,
                                                       method = method,
                                                       unequal_method=unequal_method,
                                                       randomize_pairs=randomize_pairs)

    }
  }

  # Reorder if pre-randomized
  if(isTRUE(pre_randomize)){
    data <- data %>%
      dplyr::arrange(!! as.name(local_tmp_index_var)) %>%
      dplyr::select(-dplyr::one_of(local_tmp_index_var))
  }

  # Force equal
  # Remove stuff
  if(method == "l_sizes" & isTRUE(force_equal)){

    number_of_groups_specified <- length(n)

    data <- data %>%
      dplyr::filter(factor_to_num(.data$._new_groups_) <= number_of_groups_specified)

  }

  # replace column name
  data <- replace_col_name(data, '._new_groups_', col_name)

  return(data)

}
