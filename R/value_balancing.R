
value_balanced_group_factor_ <- function(data, n, num_col, method = "n_fill") {

  # Do function that tests if a col name already exists and then add an extra _ to the internal tmp col names until it's distinct

  nrows_equal <- nrow(data) %% 2 == 0

  # Arrange by smallest, biggest, 2nd smallest, 2nd biggest, etc.
  # If the number of rows is unequal, the row with the smallest value is alone
  # This is done, as it is one with the least effect on sum of values in a group
  data_sorted <- data %>%
    dplyr::mutate(.tmp_index_ = 1:n()) %>%
    dplyr::arrange(!!as.name(num_col)) %>%
    rearrange(method = "pair_extremes", unequal_method = "first",
              drop_rearrange_factor = FALSE,
              rearrange_factor_name = "rearrange_factor")

  # Find sum of values in each rearrange factor group
  # Once again, arrange by smallest, biggest, 2nd smallest, 2nd biggest, etc.
  # Create groups, filling up from the top, so the potentially single rearrange factor group
  # ie. the row with the smallest value, is the one being added to another group.

  tmp_group_scores <- data_sorted %>%
    dplyr::group_by(rearrange_factor) %>%
    dplyr::summarize(group_aggr = sum(!!as.name(num_col)))

  if(nrows_equal){
    tmp_group_scores_sorted <- tmp_group_scores %>%
      dplyr::arrange(group_aggr)
  } else {
    # Reorder with first group always first (otherwise doesn't work with negative numbers)
    tmp_group_scores_sorted <- tmp_group_scores %>%
      dplyr::filter(row_number() == 1) %>%
      dplyr::bind_rows(tmp_group_scores %>%
                         dplyr::filter(row_number() != 1) %>%
                         dplyr::arrange(group_aggr))
  }

  # Rearrange again
  tmp_second_rearrange <- tmp_group_scores_sorted %>%
    rearrange(method = "pair_extremes", unequal_method = "first",
              drop_rearrange_factor = FALSE,
              rearrange_factor_name = "rearrange_factor_2") %>%
    dplyr::select(rearrange_factor, rearrange_factor_2)

  # Join data_sorted with the second rearrange factor
  # Order by this factor and group the dataset, filling from the top
  # Revert order back to original order of data
  # Get group factor
  data_sorted %>%
    dplyr::left_join(tmp_second_rearrange, by = "rearrange_factor") %>%
    dplyr::arrange(rearrange_factor_2) %>%
    group(n, method = method, col_name = ".vbs_groups_") %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.tmp_index_) %>%
    dplyr::pull(.vbs_groups_) %>%
    as.factor()


}


