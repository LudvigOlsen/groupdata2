# Group Factor Methods


## Greedy functions

greedy_group_factor_ <- function(v, size, force_equal = FALSE, descending = FALSE){

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

  if ( isTRUE(force_equal) && !(is_wholenumber_(length(v)/size)) ){

    n_windows <- floor(length(v)/size)
    v <- v[1:(n_windows*size)]

  } else {

    # Get the total number of windows
    n_windows <- ceiling(length(v)/size)

  }


  ### Creating grouping factor ###

  # Get the size of the last window
  size_last_window <- length(v)-(n_windows-1)*size

  # Create window grouping factor
  # This creates too many values in the last window,
  # if size_last_window is smaller than size
  window_grouping_factor <- rep(c(1:n_windows), each = size)

  if (size != size_last_window){
    # Remove the excessive values in the last window

    # Find the number of values to remove
    n_to_remove <- size-size_last_window

    # Remove excessive values
    window_grouping_factor <- window_grouping_factor[1:(length(window_grouping_factor)-n_to_remove)]

  }

  return(as.factor(window_grouping_factor))


}


## Number of windows functions

n_last_group_factor_ <- function(v, n_windows, force_equal = FALSE, descending = FALSE){

  #
  # Takes a vector and the number of wanted splits
  # Returns a factor with 1's for window 1, 2's for window 2, etc.
  # This can be used for subsetting, group_by, etc.
  #
  # Notice: The last window will contain fewer OR more elements
  # if length of the vector isn't divisible with n_windows
  #


  ### Force equal ### Set window_size ###

  # If force_equal is set to TRUE,
  # and we don't already have equally sized windows,
  # remove values from v, until we get
  # largest possible equally sized windows

  if ( isTRUE(force_equal) && !(is_wholenumber_(length(v)/n_windows)) ){

    window_size <- floor(length(v)/n_windows)
    v <- v[1:(n_windows*window_size)]

  } else {

    # Calculate size of windows
    window_size <- ceiling(length(v)/n_windows)

  }


  ### Creating grouping factor ###

  # Try to use use greedy_group_factor_ and check
  # if it returns the right number of windows

  # Set grouping_factor with greedy_group_factor_
  window_grouping_factor <- greedy_group_factor_(v, window_size)

  # If it didn't return the right number of windows
  if (max(as.numeric(window_grouping_factor)) != n_windows ||
      !is_optimal_(window_grouping_factor, n_windows)){

    window_size <- floor(length(v)/n_windows)

    if (window_size < 1){

      message('window_size < 1. This should not be possible!')
      window_size <- 1

    }

    # Get the size of the last window
    size_last_window <- length(v)-(n_windows-1)*window_size

    window_grouping_factor <- rep(c(1:n_windows), each = window_size)

    # Add the missing values in the last window

    # Find the number of values to add
    n_to_add <- size_last_window-window_size

    window_grouping_factor <- append(window_grouping_factor, rep(n_windows, n_to_add))


  }

  return(as.factor(window_grouping_factor))


}


# Number of windows - equal windows - Fill up (find better name)
# The point is that first all windows are equally big, and then
# excess datapoints are distributed one at a time ascending/descending

n_fill_group_factor_ <- function(v, n_windows, force_equal = FALSE, descending = FALSE){

  #
  # Takes a vector and a number of windows to create
  # First creates equal groups
  # then fills the excess values into the windows
  # either from the first window up or last window down
  # .. So. 111 222 33 44 or 11 22 333 444
  # Returns grouping factor
  #

  # Create a grouping factor with the biggest possible equal windows
  equal_groups <- n_last_group_factor_(v, n_windows, force_equal=TRUE)

  # Find how many excess datapoints there are
  excess_data_points <- length(v)-length(equal_groups)


  # If there are no excess_data_points or force_equal
  # is set to TRUE, we simply return the equal groups
  if (excess_data_points == 0 || isTRUE(force_equal)){

    return(equal_groups)

  }

  # We create a vector the size of excess_data_points
  # If descending is set to TRUE the values will
  # correspond to the last windows, if set to FALSE
  # the values will correspond to the first windows

  if (isTRUE(descending)){

    # Find where to start the values from
    start_rep <- (n_windows-excess_data_points)+1

    # Create vector of values to add
    values_to_add <- c(start_rep:n_windows)

  } else {

    # Create vector of values to add
    values_to_add <- c(1:excess_data_points)

  }

  # Create grouping factor
  # .. Converts the equal groups factor to a numeric vector
  # .. Adds the values to the equal groups vector
  # .. Sorts the vector so 1s are together, 2s are together, etc.
  # .. Converts the vector to a factor

  grouping_factor <- factor(sort(c(as.numeric(equal_groups),values_to_add)))

  # Return grouping factor
  return(grouping_factor)

}


# number of windows random assign of excess values

n_rand_group_factor_ <- function(v, n_windows, force_equal = FALSE, descending = FALSE){

  #
  # Takes a vector and a number of windows to create
  # First creates equal groups
  # then fills the excess values into randomly chosen windows
  # .. E.g. 111 22 33 444, 11 222 333 44, etc.
  # .. Only adds one per window though!
  # Returns grouping factor
  #

  # Create a grouping factor with the biggest possible equal windows
  equal_groups <- n_last_group_factor_(v, n_windows, force_equal=TRUE)

  # Find how many excess datapoints there are
  excess_data_points <- length(v)-length(equal_groups)

  # If there are no excess_data_points or force_equal
  # is set to TRUE, we simply return the equal groups
  if (excess_data_points == 0 || isTRUE(force_equal)){

    # Return equal groups grouping factor
    return(equal_groups)

  }

  # Get values to add
  # .. Creates a vector with values from 1 to the number
  # .. of windows
  # .. Randomly picks a value for each excess data point
  values_to_add <- sample(c(1:n_windows), excess_data_points)

  # Create grouping factor
  # .. Converts the equal groups factor to a numeric vector
  # .. Adds the values to the equal groups vector
  # .. Sorts the vector so 1s are together, 2s are together, etc.
  # .. Converts the vector to a factor
  grouping_factor <- factor(sort(c(as.numeric(equal_groups),values_to_add)))

  # Return grouping factor
  return(grouping_factor)

}


# N distributed

n_dist_group_factor_ <- function(v, n_windows, force_equal = FALSE, descending = FALSE){

  #
  # Takes a vector and a number of windows to create
  # Distributes excess elements somewhat evenly across windows
  # Returns grouping factor
  #

  # If force_equal is set to TRUE
  # .. Create equal groups and return these
  if (isTRUE(force_equal)){

    # Create a grouping factor with the biggest possible equal windows
    equal_groups <- n_last_group_factor_(v, n_windows, force_equal=TRUE)

    return(equal_groups)

  } else {

    # Create grouping factor with distributed excess elements
    grouping_factor <- factor(ceiling(seq_along(v)/(length(v)/n_windows)))

    return(grouping_factor)
  }

}


# Staircasing
#' @importFrom dplyr %>%
stair_split_group_factor_ <- function(v, step_size, force_equal = FALSE, descending = FALSE){

  #
  # Takes a vector and the step size
  # Returns a staircased grouping factor
  # .. 1223334444 etc.
  #

  # Get the number of groups with no staircasing
  n_groups <- ceiling(length(v)/step_size)

  # Create a dataframe with 1 column containing a group index
  group_data <- data.frame('groups' = c(1:n_groups))


  # Create a column with number of elements (group number times step size)
  # Create a column with cumulative sum of the number of elements
  group_data <- group_data %>%
    dplyr::mutate(n_elements = groups*step_size,
                  cumsum = cumsum(n_elements))

  # Get the first row where cumsum is larger or equal to the vector
  # This contains info on how many groups we need for our staircasing
  last_group_row <- dplyr::filter(group_data, cumsum >= length(v))[1,]

  # Find how many rows we need for staircasing
  n_needed_groups <- last_group_row[1,1]

  # Get the cumulative sum for that group
  # This can be used to calculate excess elements
  # if we include this group in the grouping factor
  cumsum_last_group <- last_group_row[1,3]

  # Get how many excess elements there are if we
  # include this group in the grouping factor
  excess_elements <- cumsum_last_group-length(v)


  # If force_equal is set to TRUE
  if (isTRUE(force_equal)){

    # If there are any excess elements
    if (excess_elements > 0){

      # We will remove the last group
      group_data <- group_data %>%
        dplyr::filter(groups <= n_needed_groups-1)

      # Get the new last row in group_data
      last_row <- tail(group_data, 1)

      # Get the cumulative sum in the last row
      cumsum_last_row <- last_row[1,3]

      # Subset the vector to the cumulative sum
      # of the last row in group_data
      # .. So rows 1 to cumulative sum
      v <- head(v, cumsum_last_row)

    } else {

      # If there are no excess elements
      # subset group_data to get the needed groups only
      group_data <- group_data %>%
        dplyr::filter(groups <= n_needed_groups)

    }

  } else {

    # If force_equal is set to FALSE
    # subset group_data to get the needed groups only

    group_data <- group_data %>%
      dplyr::filter(groups <= n_needed_groups)

  }

  # Create grouping factor
  # .. using 'rep(groups, n_elements)'
  grouping_factor <- factor(head(rep(group_data[[1]], group_data[[2]]), length(v)))

  # Return grouping factor
  return(grouping_factor)


}

