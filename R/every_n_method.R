

every_n_group_factor_ <- function(v, every_n, force_equal = FALSE, descending = FALSE) {

  #
  # Takes a vector and the distance between group members
  # Simply repeats 1:every_n until it fits the size of v
  # Returns grouping factor
  #

  # Calculate the expected number of group elements
  # AKA. the number of repetitions of 1:every_n
  rounding_fn <- ceiling
  if (isTRUE(force_equal))
    rounding_fn <- floor
  num_expected_group_elements <- rounding_fn(length(v) / every_n)

  # Create vector with one index per group
  group_indices <- seq_len(every_n)
  # Reverse to every_n:1
  # Note: It's not clear whether this is what we want
  # or simply reversing the final factor (meaning that we start from the bottom)
  # if (isTRUE(descending)){
  #   group_indices <- rev(group_indices)
  # }

  # Repeat the 1:every_n sequence to get the right sized vector
  grouping_factor <- rep(group_indices, times = num_expected_group_elements)

  # Remove excessive indices
  if (!isTRUE(force_equal)){
    grouping_factor <- head(grouping_factor, length(v))
  }

  # Return grouping factor
  factor(grouping_factor)
}
