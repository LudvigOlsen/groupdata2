
# For creating
create_num_col_groups <- function(data, n, num_col, cat_col=NULL, id_col=NULL, col_name,
                                  id_aggregation_fn = sum, method="n_fill",
                                  unequal_method="first", force_equal=FALSE,
                                  pre_randomize=TRUE
                                  ) {

  # Sample dataframe before use.
  if (isTRUE(pre_randomize)){
    data <- data %>%
      dplyr::mutate(.__tmp_index__ = 1:n()) %>%
      dplyr::sample_frac(1)
  }

  # If cat_col is not NULL
  if (!is.null(cat_col)){

    # If id_col is not NULL
    if (!is.null(id_col)){

      # aggregate val col per ID
      ids_aggregated <- data %>%
        group_by(!! as.name(cat_col), !! as.name(id_col)) %>%
        dplyr::summarize(aggr_val = id_aggregation_fn(!!as.name(num_col))) %>%
        dplyr::ungroup()

      # Find groups for each category
      ids_grouped <- plyr::ldply(unique(ids_aggregated[[cat_col]]), function(category){
        ids_for_cat <- ids_aggregated %>%
          dplyr::filter(!!as.name(cat_col) == category)
        ids_for_cat$._new_groups_ <- numerically_balanced_group_factor_(ids_for_cat, n=n, num_col = "aggr_val",
                                                                method=method, unequal_method=unequal_method)
        ids_for_cat %>%
          dplyr::select(-c(aggr_val))
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
        data_for_cat$._new_groups_ <- numerically_balanced_group_factor_(data_for_cat, n=n,
                                                                 num_col = num_col,
                                                                 method=method,
                                                                 unequal_method=unequal_method)
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
        ids_aggregated, n=n, num_col = "aggr_val", method=method, unequal_method=unequal_method)
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
                                                       unequal_method=unequal_method)

    }
  }

  # Reorder if pre-randomized
  if(isTRUE(pre_randomize)){
    data <- data %>%
      dplyr::arrange(.__tmp_index__) %>%
      dplyr::select(-c(.__tmp_index__))
  }


  # Force equal
  # Remove stuff
  if(method=="l_sizes" & isTRUE(force_equal)){

    number_of_groups_specified <- length(n)

    data <- data %>%
      dplyr::filter(factor_to_num(._new_groups_) <= number_of_groups_specified)

  }


  # replace column name
  data <- replace_col_name(data, '._new_groups_', col_name)

  return(data)

}
