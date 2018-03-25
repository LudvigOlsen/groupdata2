
value_balanced_group_factor_ <- function(data, n, val_col, method = "n_fill") {
  # Do function that tests if a col name already exists and then add an extra _ to the internal tmp col names until it's distinct

  # Arrange by smallest, biggest, 2nd smallest, 2nd biggest, etc.
  # If the number of rows is unequal, the row with the smallest value is alone
  # This is done, as it is one with the least effect on sum of values in a group
  data_sorted <- data %>%
    dplyr::mutate(.tmp_index_ = 1:n()) %>%
    dplyr::arrange(!!as.name(val_col)) %>%
    dplyr::mutate(rearrange_factor = create_rearrange_factor(n(), unequal_method = "first")) %>%
    dplyr::arrange(rearrange_factor)

  # Find sum of values in each rearrange factor group
  # Once again, arrange by smallest, biggest, 2nd smallest, 2nd biggest, etc.
  # Create groups, filling up from the top, so the potentially single rearrange factor group
  # ie. the row with the smallest value, is the one being added to another group.

  tmp_group_scores <- data_sorted %>%
    dplyr::group_by(rearrange_factor) %>%
    dplyr::summarize(group_sum = sum(!!as.name(val_col)))

  # Reorder with first group always first (otherwise doesn't work with negative numbers)
  tmp_group_scores_sorted <- tmp_group_scores %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::bind_rows(tmp_group_scores %>%
                       dplyr::filter(row_number() != 1) %>%
                       dplyr::arrange(group_sum))

  # Create groups
  tmp_groups <- tmp_group_scores_sorted %>%
    dplyr::mutate(rearrange_factor_2 = create_rearrange_factor(n(), unequal_method = "first")) %>%
    dplyr::arrange(rearrange_factor_2) %>%
    group(n, method = method, col_name = ".vbs_groups_") %>%
    dplyr::ungroup() %>%
    dplyr::select(rearrange_factor, .vbs_groups_)

  # Reorder to original order and get the grouping factor
  data_sorted %>%
    dplyr::left_join(tmp_groups, by = "rearrange_factor") %>%
    dplyr::arrange(.tmp_index_) %>%
    dplyr::pull(.vbs_groups_) %>%
    as.factor()

}

create_rearrange_factor <- function(size, unequal_method = "middle") { # TODO Turn into arrange function
  #
  # Creates factor for rearranging in 1st, last, 2nd, 2nd last, 3rd, 3rd last, ...
  # When size is unequal, there are two methods for dealing with it:
  # .. "middle":
  # .. .. adds ceiling(size / 4) in the middle of the factor
  # .. .. every value larger than or equal to the middle value gets +1
  # .. .. e.g. 1,2,4,5,3,5,4,2,1
  # .. "first":
  # .. .. the first row becomes group 1 on its own.
  # .. .. creates rearrange factor on the rest, all gets +1
  # .. .. e.g. 1,2,3,4,4,3,2
  #

  half_size <- floor(size / 2)
  idx <- 1:(half_size)
  if (half_size * 2 == size) {
    return(c(idx, rev(idx)))
  } else {
    if (unequal_method == "middle") {
      middle <- ceiling((half_size / 2)) + 1
      idx <- idx %>%
        dplyr::as_data_frame() %>%
        dplyr::mutate(value = ifelse(value >= middle, value + 1, value)) %>%
        dplyr::pull(value)
      return(c(idx, middle, rev(idx)))
    } else if (unequal_method == "first") {
      return(c(1, c(idx, rev(idx)) + 1))
    }
  }
}
