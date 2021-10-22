## Group size methods

# Greedy

greedy_group_factor_ <- function(v, size, force_equal = FALSE, descending = FALSE) {

  #
  # Takes a vector and the size of the wanted windows
  # Returns a factor with 1's for window 1, 2's for window 2, etc.
  # This can be used for subsetting, group_by, etc.
  #
  # Notice: The last window will contain fewer elements
  # if length of the vector isn't divisible with size
  #


  ### Force equal ### Set n_windows ###

  # If force_equal is set to TRUE,
  # and we don't already have equally sized windows,
  # remove values from v, until we get
  # largest possible equally sized windows

  if (isTRUE(force_equal) && !(is_wholenumber_(length(v) / size))) {
    n_windows <- floor(length(v) / size)
    v <- v[1:(n_windows * size)]
  } else {

    # Get the total number of windows
    n_windows <- ceiling(length(v) / size)
  }


  ### Creating grouping factor ###

  # Get the size of the last window
  size_last_window <- length(v) - (n_windows - 1) * size

  # Create window grouping factor
  # This creates too many values in the last window,
  # if size_last_window is smaller than size
  window_grouping_factor <- rep(c(1:n_windows), each = size)

  if (size != size_last_window) {
    # Remove the excessive values in the last window

    # Find the number of values to remove
    n_to_remove <- size - size_last_window

    # Remove excessive values
    window_grouping_factor <- window_grouping_factor[1:(length(window_grouping_factor) - n_to_remove)]
  }

  return(as.factor(window_grouping_factor))
}
