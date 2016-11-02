##############################################
#
#   splitters
#   by Ludvig R. Olsen
#   Oct. 2016 
#   Cognitive Science, Aarhus University
#
##############################################

# Main functions:
# .. group
# .. group_factor
# .. splt



# Naming convention: underscore_seperated
# Commenting convention: one space after #

# Definitions:
# .. Excess elements
# .... Elements that would get cut out if using force_equal
# .... So if we make equally large groups from the data
# .... and there are any datapoints left. These are excess elements.


# Libraries

library(plyr)

# Helper functions

is_wholenumber_ <- function(n) { 
  
  # If n is a whole number
  # .. return TRUE
  # else 
  # .. return FALSE
  
  return( floor(n) == n )
}

arg_is_wholenumber_ = function(n){
  
  # Checks if n is a whole number of either 
  # type integer or numeric
  # Returns TRUE if yes, else FALSE 
  
  # If n is an integer, return TRUE
  # else check if it is a numeric
  # .. if yes, check if it is a whole number
  # .... if yes, return TRUE
  # .... if no, return FALSE
  # .. if not a numeric
  # .... return FALSE
  
  if ( is.integer(n) ){
    
    return(TRUE)
    
  } else if ( is.numeric(n) ){
    
    if ( is_wholenumber_(n) ){

      return(TRUE)
      
    } else {
      
      return(FALSE)
    }
    
  } else {
    
    return(FALSE)
  }
}

arg_is_number_ = function(n){
  
  # Checks if n is either an integer or a numeric
  # Returns TRUE if yes, FALSE if no
  
  if ( is.integer(n) || is.numeric(n) ){
    
    return(TRUE)
    
  } else {
    
    return(FALSE)

  }

}

is_optimal_ = function(grouping_factor, n_windows) {
  
  # Takes a grouping factor and the number of windows in it
  # Checks if the difference between the count of values in
  # the last window and the other windows would be bigger or 
  # smaller if we removed 1 element from all windows except 
  # the last window and added those elements to the last window
  
  # Count the values of all the windows
  count_values = plyr::count(as.numeric(grouping_factor))
  
  # Get the count of values in the first window
  first_count_value = count_values[1,]$freq
  
  # Get the count of values in the last window
  last_count_value = count_values[n_windows,]$freq
  
  # Get the difference of count values
  difference = abs(first_count_value-last_count_value)
  
  # If we had one element less in the first windows
  # and added those to the last window instead,
  # would the last window be closer or further from the others?
  # .. So would the difference be smaller?
  
  # Remove 1 value from the first window value count
  f2 = first_count_value-1
  
  # Add the amount of values that would have been removed
  # to the last window value count
  l2 = last_count_value+n_windows-1
  
  # Get the difference between these
  difference2 = abs(f2-l2)
  
  # If difference is smaller than difference 2,
  # it means that the original distribution of
  # values was optimal.
  
  if (difference<difference2){
    
    return(TRUE)
    
  } else {
    
    return(FALSE)
    
  }
  
}

convert_percentage_ = function(per, data) {
  
  # Converts a percentage of vector elements 
  # into a count of elements
  
  # Example:
  # A vector with 100 elements
  # A percentage given as 0.1 (so 10 percent)
  # Returns 10 
  
  if(is.data.frame(data)){
    
    return(floor(nrow(data)*per))
    
  } else {
    
    return(floor(length(data)*per))
    
  }
  
  
  
}

is_between_ <- function(x, a, b) {
  
  # Checks if x is between a and b
  
  x > a && x < b
}

check_arguments_ = function(data, n, method, force_equal, 
                            allow_zero, descending){
  
  # Checks if the given arguments live up to certain rules,
  # which allow them to be used in the function
  
  # "data" can be both a dataframe or a vector
  
  # Stop execution if input variables aren't what we expect / can handle
  stopifnot((!is.null(n)),
            arg_is_number_(n),
            n > 0,
            is.logical(force_equal),
            is.logical(allow_zero),
            is.logical(descending),
            method %in% c('greedy', 
                          'n_dist',
                          'n_last', 
                          'n_fill', 
                          'n_rand',
                          'staircase'))
  
  if (is.data.frame(data)){
    
    # Stop execution if input variables aren't what we expect / can handle
    stopifnot(nrow(data) > 0)
    
  } else {
    
    # Stop execution if input variables aren't what we expect / can handle
    stopifnot((!is.null(data)),
              is.vector(data),
              length(data) > 0)
    
  }
  
  
}

check_convert_check_ = function(data, n, method, force_equal, 
                                allow_zero, descending){
  
  # Checks arguments 
  # Converts n if given as percentage
  # Checks more arguments
  # Returns the converted/non-converted n
  
  # Notice: This is used in more than one of the main functions
  # so I put it in a function to make those functions more readable
  
  ### Check arguments
  
  # Check if given arguments are allowed
  # If not -> stop execution
  check_arguments_(data, n, method, force_equal, allow_zero, descending)
  
  
  ### Convert from percentage
  
  # We check if n is given as percentage
  # This would be done by giving a number between 
  # 0 and 1
  # If it is, we convert it to the actual number 
  # of windows 
  
  if (is_between_(n, 0,1)){
    
    n = convert_percentage_(n, data)
    
  }
  
  
  ### Check arguments 2 
  
  # Check if 
  # .. n is a whole number
  # .. Length of the data is larger or
  # .. equal to n
  # If not -> stop execution
  
  if(is.data.frame(data)){
    
    stopifnot(arg_is_wholenumber_(n),
              nrow(data) >= n)
    
  } else {
    
    stopifnot(arg_is_wholenumber_(n),
              length(data) >= n)
  }
  
  
  return(n)
  
}


# Main functions

group = function(data, n, method = 'n_dist', force_equal = FALSE, 
                 allow_zero = FALSE, return_factor = FALSE, 
                 descending = FALSE, randomize = FALSE){
  
  #
  # Takes dataframe or vector
  # Creates a grouping factor
  # If data is a vector
  # .. Return dataframe with vector and grouping factor 
  # .. grouped by grouping factor 
  # If data is a dataframe:
  # .. Return dataframe grouped by grouping factor 
  #
  
  # Create grouping factor
  grouping_factor = group_factor(data, n, method, force_equal = force_equal, 
                                 allow_zero = allow_zero, descending = descending, 
                                 randomize = randomize)
  
  # If return_factor is set to TRUE
  # .. return the created grouping factor
  if (isTRUE(return_factor)){
    
    return(grouping_factor)
    
  }
  
  
  # If data is a dataframe
  # .. Check if force_equal is TRUE
  # .... if so, shorten data to the length of the
  # .... grouping factor
  # .. Add grouping factor to data
  # .. Group by grouping factor and return data
  # If data is a vector
  # .. Check if force_equal is TRUE
  # .... if so, shorten data to the length of the
  # .... grouping factor
  # .. Create a dataframe 
  # .... with data and the grouping factor
  # .. Group by grouping factor and return data
  
  # If data is dataframe
  if(is.data.frame(data)){
    
    # If force_equal is TRUE
    if(isTRUE(force_equal)){
      
      # Shorten data to the length of the grouping factor
      data = head(data, length(grouping_factor))
      
    }
    
    # Add the grouping factor to data
    data$.groups = grouping_factor
    
    # Return data grouped by the grouping factor
    return(dplyr::group_by(data, .groups))
    
  } else { # If data is vector
      
    # If force_equal is TRUE
    if(isTRUE(force_equal)){
        
      # Shorten data to the length of the grouping factor
      data = head(data, length(grouping_factor))
        
    }
    
    # Create dataframe with data and the grouping factor
    data = data.frame(data, ".groups" = grouping_factor)
    
    # Return data grouped by the grouping factor
    return(dplyr::group_by(data, .groups))
      
  }
  
}
  
group_factor = function(data, n, method = 'n_dist', force_equal = FALSE, 
                        allow_zero = FALSE, descending = FALSE,
                        randomize = FALSE){
  
  #
  # Takes dataframe or vector
  # Returns a grouping factor
  #
  
  
  ### Allow zero ###
  
  # If allow_zero is TRUE and n is 0 
  # return NAs instead of giving an error
  if (isTRUE(allow_zero) && n == 0){
    
    if(is.data.frame(data)){
      
      return(rep(NA, each = nrow(data)))
      
    } else {
     
      return(rep(NA, each = length(data)))
      
    }
    
    
  }
  
  # Check arguments
  # Convert n if given as percentage
  # Check more arguments
  n = check_convert_check_(data, n, method, force_equal, allow_zero, descending)
  
  
  # Create grouping factors
  # .. Check if data is a dataframe or a vector
  # .. Call grouping factor function for specified method
  
  if(is.data.frame(data)){
    
    if(method == 'greedy'){
      
      groups = greedy_group_factor_(data[,1], n, force_equal, descending)
      
    } else if (method == 'n_dist'){
      
      groups = n_dist_group_factor_(data[,1], n, force_equal, descending)
      
    } else if (method == 'n_last'){
      
      groups = n_last_group_factor_(data[,1], n, force_equal, descending)
      
    } else if (method == 'n_fill'){
      
      groups = n_fill_group_factor_(data[,1], n, force_equal, descending)
      
    } else if (method == 'n_rand'){
      
      groups = n_rand_group_factor_(data[,1], n, force_equal, descending)
      
    } else if (method == 'staircase'){
      
      groups = stair_split_grouping_factor_(data[,1], n, force_equal, descending)
      
    }
    
  } else {
    
    if(method == 'greedy'){
      
      groups = greedy_group_factor_(data, n, force_equal, descending)
      
    } else if (method == 'n_dist'){
      
      groups = n_dist_group_factor_(data, n, force_equal, descending)
      
    } else if (method == 'n_last'){
      
      groups = n_last_group_factor_(data, n, force_equal, descending)
      
    } else if (method == 'n_fill'){
      
      groups = n_fill_group_factor_(data, n, force_equal, descending)
      
    } else if (method == 'n_rand'){
      
      groups = n_rand_group_factor_(data, n, force_equal, descending)
      
    } else if (method == 'staircase'){
      
      groups = stair_split_grouping_factor_(data, n, force_equal, descending)
      
    }
    
  }
  
  # If randomize is set to TRUE
  # .. reorganize the grouping factor randomly
  
  if (isTRUE(randomize)){
    
    groups = sample(groups) 
    
  }
  
  # Return grouping factor
  return(groups)
 
}

splt = function(data, n, method = 'n_dist', force_equal = FALSE, 
                allow_zero = FALSE, descending = FALSE, randomize = FALSE){
  
  #
  # Takes dataframe or vector
  # Splits into the specified windows
  # Returns list with the windows (dataframes or vectors)
  #
  
  # If allow_zero is TRUE, and n is 0 
  # .. Return the given data in a list 
  # .. instead of giving an error
  if (isTRUE(allow_zero) && n == 0){
    
    return(split(data, factor(1)))
    
  }

  # Check arguments
  # Convert n if given as percentage
  # Check more arguments
  n = check_convert_check_(data, n, method, force_equal, allow_zero, descending)
  
  
  # Force equal
  # .. Some methods have a different way of calculating 
  # .. "equality". They will do this themselves,
  # .. the others can be forced equal here.
  
  if(isTRUE(force_equal)){
    
    if(!(method %in% c('staircase'))){
      
      # If force_equal is set to TRUE,
      # and we don't already have equally sized windows,
      # remove elements/rows from data, until we get
      # largest possible equally sized windows
      if(is.data.frame(data)){
        
        if (!(is_wholenumber_(nrow(data)/n))){
          
          # Multiply window size and number of windows to find
          # how much data to keep
          to_keep = floor(nrow(data)/n)*n
          
          # Keep the first to_keep elements/rows
          data = head(data, to_keep)
          
        }
        
      } else {
        
        if (!(is_wholenumber_(length(data)/n))){
          
          # Multiply window size and number of windows to find
          # how much data to keep
          to_keep = floor(length(data)/n)*n
          
          # Keep the first to_keep elements/rows
          data = head(data, to_keep)
          
        }
        
      }
      
      
    }
  }
  
  
  # Split the data 
  # .. Checks if data is dataframe or vector
  # .. Calls the right splitter
  
  if (is.data.frame(data)){
    
    return(dsplit_(data, n, method, force_equal, allow_zero, descending, randomize))
    
  } else {
    
    return(vsplit_(data, n, method, force_equal, allow_zero, descending, randomize))
    
  }
  
}



## Dataframe and vector splitters

dsplit_ = function(data, n, method, force_equal = FALSE, allow_zero = FALSE, 
                   descending = FALSE, randomize = FALSE){
  
  #
  # Takes a dataframe
  # Creates grouping factor based on method
  # Splits the dataframe 
  # Returns a list of dataframes
  #
  
  if (method == 'staircase') {
    
    # Create grouping factor
    groups = group_factor(data[,1], n, method = method, force_equal = force_equal, 
                         descending = descending, randomize = randomize)
    
    if (isTRUE(force_equal)){
      
      data = head(data, length(groups))
      
    }
    
  } else {
    
    # Create grouping factor
    groups = group_factor(data[,1], n, method = method, descending = descending, 
                         randomize = randomize)
    
  }
  # Split dataframe into a list of dataframes
  data_splitted = split(data , f = groups)
  
  return(data_splitted)
  
}

vsplit_ = function(v, n, method, force_equal = FALSE, allow_zero = FALSE, 
                   descending = FALSE, randomize = FALSE){
  
  #
  # Takes a vector
  # Creates grouping factor based on method
  # Splits the vector
  # Returns a list of vectors
  #
  
  
  if (method == 'staircase') {
    
    # Create grouping factor
    groups = group_factor(v, n, method = method, force_equal = force_equal, 
                         descending = descending, randomize = randomize)
    
    if (isTRUE(force_equal)){
      
      v = head(v, length(groups))
      
    }
    
    
  } else {
    
    # Create grouping factor
    groups = group_factor(v, n, method = method, descending = descending, 
                         randomize = randomize)
    
  }
  
  
  # Split vector into a list of vectors
  data_splitted = split(v , f = groups)
  
  return(data_splitted)
  
}



## Greedy functions

greedy_group_factor_ = function(v, size, force_equal = FALSE, descending = FALSE){
  
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
    
    n_windows = floor(length(v)/size)
    v = v[1:(n_windows*size)]
    
  } else {
    
    # Get the total number of windows
    n_windows = ceiling(length(v)/size)
    
  }
  
  
  ### Creating grouping factor ###
  
  # Get the size of the last window
  size_last_window = length(v)-(n_windows-1)*size  
  
  # Create window grouping factor
  # This creates too many values in the last window,
  # if size_last_window is smaller than size
  window_grouping_factor = rep(c(1:n_windows), each = size)
  
  if (size != size_last_window){
    # Remove the excessive values in the last window
    
    # Find the number of values to remove
    n_to_remove = size-size_last_window
    
    # Remove excessive values
    window_grouping_factor = window_grouping_factor[1:(length(window_grouping_factor)-n_to_remove)]
    
  }
  
  return(as.factor(window_grouping_factor))
  
  
}


## Number of windows functions

n_last_group_factor_ = function(v, n_windows, force_equal = FALSE, descending = FALSE){
  
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
    
    window_size = floor(length(v)/n_windows)
    v = v[1:(n_windows*window_size)]
    
  } else {
  
    # Calculate size of windows
    window_size = ceiling(length(v)/n_windows)

  }
    
  
  ### Creating grouping factor ###
  
  # Try to use use greedy_group_factor_ and check 
  # if it returns the right number of windows
  
  # Set grouping_factor with greedy_group_factor_
  window_grouping_factor = greedy_group_factor_(v, window_size)
  
  # If it didn't return the right number of windows
  if (max(as.numeric(window_grouping_factor)) != n_windows || 
      !is_optimal_(window_grouping_factor, n_windows)){
    
    window_size = floor(length(v)/n_windows)
    
    if (window_size < 1){
      
      message('window_size < 1. This should not be possible!')
      window_size = 1
      
    }
      
    # Get the size of the last window
    size_last_window = length(v)-(n_windows-1)*window_size
    
    window_grouping_factor = rep(c(1:n_windows), each = window_size)
    
    # Add the missing values in the last window
    
    # Find the number of values to add
    n_to_add = size_last_window-window_size
    
    window_grouping_factor = append(window_grouping_factor, rep(n_windows, n_to_add))
      

  } 
  
  return(as.factor(window_grouping_factor))
  
  
}
  

# Number of windows - equal windows - Fill up (find better name)
# The point is that first all windows are equally big, and then 
# excess datapoints are distributed one at a time ascending/descending

n_fill_group_factor_ = function(v, n_windows, force_equal = FALSE, descending = FALSE){
  
  # 
  # Takes a vector and a number of windows to create
  # First creates equal groups
  # then fills the excess values into the windows 
  # either from the first window up or last window down
  # .. So. 111 222 33 44 or 11 22 333 444
  # Returns grouping factor
  #
  
  # Create a grouping factor with the biggest possible equal windows 
  equal_groups = n_last_group_factor_(v, n_windows, force_equal=TRUE)
  
  # Find how many excess datapoints there are 
  excess_data_points = length(v)-length(equal_groups)
  
  
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
    start_rep = (n_windows-excess_data_points)+1
    
    # Create vector of values to add
    values_to_add = c(start_rep:n_windows)
    
  } else {
  
    # Create vector of values to add
    values_to_add = c(1:excess_data_points)
    
  }
  
  # Create grouping factor
  # .. Converts the equal groups factor to a numeric vector
  # .. Adds the values to the equal groups vector
  # .. Sorts the vector so 1s are together, 2s are together, etc.
  # .. Converts the vector to a factor
  
  grouping_factor = factor(sort(c(as.numeric(equal_groups),values_to_add)))
  
  # Return grouping factor
  return(grouping_factor)
  
}


# number of windows random assign of excess values

n_rand_group_factor_ = function(v, n_windows, force_equal = FALSE, descending = FALSE){
  
  # 
  # Takes a vector and a number of windows to create
  # First creates equal groups
  # then fills the excess values into randomly chosen windows 
  # .. E.g. 111 22 33 444, 11 222 333 44, etc.
  # .. Only adds one per window though!
  # Returns grouping factor
  #
  
  # Create a grouping factor with the biggest possible equal windows 
  equal_groups = n_last_group_factor_(v, n_windows, force_equal=TRUE)
  
  # Find how many excess datapoints there are 
  excess_data_points = length(v)-length(equal_groups)
  
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
  values_to_add = sample(c(1:n_windows), excess_data_points)
  
  # Create grouping factor
  # .. Converts the equal groups factor to a numeric vector
  # .. Adds the values to the equal groups vector
  # .. Sorts the vector so 1s are together, 2s are together, etc.
  # .. Converts the vector to a factor
  grouping_factor = factor(sort(c(as.numeric(equal_groups),values_to_add)))
  
  # Return grouping factor
  return(grouping_factor)
  
}


# N distributed

n_dist_group_factor_ = function(v, n_windows, force_equal = FALSE, descending = FALSE){
  
  # 
  # Takes a vector and a number of windows to create
  # Distributes excess elements somewhat evenly across windows
  # Returns grouping factor 
  #
  
  # If force_equal is set to TRUE
  # .. Create equal groups and return these
  if (isTRUE(force_equal)){
    
    # Create a grouping factor with the biggest possible equal windows 
    equal_groups = n_last_group_factor_(v, n_windows, force_equal=TRUE)
    
    return(equal_groups)
    
  } else {
  
    # Create grouping factor with distributed excess elements
    grouping_factor = factor(ceiling(seq_along(v)/(length(v)/n_windows)))
  
    return(grouping_factor)
  }
  
}


# Staircasing 

stair_split_grouping_factor_ = function(v, step_size, force_equal = FALSE, descending = FALSE){
  
  # 
  # Takes a vector and the step size
  # Returns a staircased grouping factor
  # .. 1223334444 etc.
  #
  
  # Get the number of groups with no staircasing
  n_groups = ceiling(length(v)/step_size)
  
  # Create a dataframe with 1 column containing a group index 
  group_data = data.frame('groups' = c(1:n_groups))
  
  
  # Create a column with number of elements (group number times step size)
  # Create a column with cumulative sum of the number of elements
  group_data = group_data %>%
    mutate(n_elements = groups*step_size,
           cumsum = cumsum(n_elements))
  
  # Get the first row where cumsum is larger or equal to the vector
  # This contains info on how many groups we need for our staircasing
  last_group_row = filter(group_data, cumsum >= length(v))[1,]

  # Find how many rows we need for staircasing
  n_needed_groups = last_group_row[1,1]

  # Get the cumulative sum for that group
  # This can be used to calculate excess elements
  # if we include this group in the grouping factor
  cumsum_last_group = last_group_row[1,3]
   
  # Get how many excess elements there are if we
  # include this group in the grouping factor
  excess_elements = cumsum_last_group-length(v)

  
  # If force_equal is set to TRUE
  if (isTRUE(force_equal)){
    
    # If there are any excess elements
    if (excess_elements > 0){
      
      # We will remove the last group
      group_data = group_data %>%
        filter(groups <= n_needed_groups-1)
      
      # Get the new last row in group_data
      last_row = tail(group_data, 1)
      
      # Get the cumulative sum in the last row
      cumsum_last_row = last_row[1,3]
      
      # Subset the vector to the cumulative sum 
      # of the last row in group_data
      # .. So rows 1 to cumulative sum
      v = head(v, cumsum_last_row)
    
    } else {
      
      # If there are no excess elements
      # subset group_data to get the needed groups only
      group_data = group_data %>%
        filter(groups <= n_needed_groups)
      
    }
    
  } else {
    
    # If force_equal is set to FALSE
    # subset group_data to get the needed groups only
    
    group_data = group_data %>%
      filter(groups <= n_needed_groups)
    
  }
  
  # Create grouping factor 
  # .. using 'rep(groups, n_elements)'
  grouping_factor = factor(head(rep(group_data[,1], group_data[,2]), length(v)))
  
  # Return grouping factor
  return(grouping_factor)

  
}




