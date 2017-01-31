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
  grouping_factor <- factor(head(rep(group_data[[1]],
                                     group_data[[2]]),
                                 length(v)))

  # Return grouping factor
  return(grouping_factor)


}

# Prime numbers staircasing
#' @importFrom dplyr %>%
primes_split_group_factor_ <- function(v, start_at=2, force_equal = FALSE, descending = FALSE){

  #
  # Takes a vector and the prime number to start at
  # Returns a grouping factor with prime number sized (staircasing design) groups
  # .. 1223334444 etc.
  #

  stopifnot(start_at >= 2)

  # Get a number of groups sure to be >= than the final number of groups
  n_groups <- ceiling(length(v)/start_at)

  # Create a dataframe with 1 column containing a group index
  group_data <- data.frame('groups' = c(1:n_groups))

  # Create a column with number of elements (group number times step size)
  # Create a column with cumulative sum of the number of elements
  group_data <- group_data %>%
    dplyr::mutate(n_elements = create_n_primes(length(groups), start_at),
                  cumsum = cumsum(as.numeric(n_elements)))

  # Get the first row where cumsum is larger or equal to the length of
  # the vector. This contains info on how many groups we need.
  last_group_row <- dplyr::filter(group_data, cumsum >= length(v))[1,]

  # Find how many rows we need
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
  grouping_factor <- factor(head(rep(group_data[[1]],
                                     group_data[[2]]),
                                 length(v)))

  # Return grouping factor
  return(grouping_factor)


}

