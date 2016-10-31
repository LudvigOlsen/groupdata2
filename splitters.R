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
  count_values = count(as.numeric(grouping_factor))
  
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

check_arguments_ = function(data, n, method, force_equal, allow_zero){
  
  # Checks if the given arguments live up to certain rules,
  # which allow them to be used in the function
  
  # "data" can be both a dataframe or a vector
  
  # Stop execution if input variables aren't what we expect / can handle
  stopifnot((!is.null(n)),
            arg_is_number_(n),
            n > 0,
            is.logical(force_equal),
            is.logical(allow_zero),
            method %in% c('greedy', 'n_windows', 'n_fill'))
  
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


# Main functions

group = function(data, n, method = 'n_windows', force_equal = FALSE, 
                 allow_zero = FALSE, return_factor = FALSE, 
                 descending = FALSE){
  
  #
  # Takes dataframe or vector
  # Creates a grouping factor
  #
  # If data is a vector
  # .. Return dataframe with vector grouped by grouping factor 
  # If data is a dataframe:
  # .. Return dataframe grouped by grouping factor 
  #
  
  grouping_factor = group_factor(data, n, method, force_equal = force_equal, 
                                 allow_zero = allow_zero, descending = descending)
  
  print(grouping_factor)
  
  if (isTRUE(return_factor)){
    
    return(grouping_factor)
    
  }
  
  
  if(is.data.frame(data)){
    
    if(isTRUE(force_equal)){
      
      data = head(data, length(grouping_factor))
      
    }
    
    data$.groups = grouping_factor
    
    return(dplyr::group_by(data, .groups))
    
  } else {
      
    if(isTRUE(force_equal)){
        
        data = head(data, length(grouping_factor))
        
    }
    
    data = data.frame(data, ".groups" = grouping_factor)
    
    return(dplyr::group_by(data, .groups))
      
  }
  
}
  
group_factor = function(data, n, method = 'n_windows', force_equal = FALSE, allow_zero = FALSE, descending = FALSE){
  
  #
  # Takes dataframe or vector
  # Returns a grouping factor
  #
  
  print(descending)
  
  if(is.data.frame(data)){
    
    if(method == 'greedy'){
      
      return(gsplit_grouping_factor_(data[,1], n, force_equal, allow_zero))
      
    } else if (method == 'n_windows'){
      
      return(nsplit_grouping_factor_(data[,1], n, force_equal, allow_zero))
      
    } else if (method == 'n_fill'){
      
      return(n_fill_split_grouping_factor_(data[,1], n, force_equal, allow_zero, descending))
      
    }
    
  } else {
    
    if(method == 'greedy'){
      
      return(gsplit_grouping_factor_(data, n, force_equal, allow_zero))
      
    } else if (method == 'n_windows'){
      
      return(nsplit_grouping_factor_(data, n, force_equal, allow_zero))
      
    } else if (method == 'n_fill'){
      
      return(n_fill_split_grouping_factor_(data, n, force_equal, allow_zero, descending))
      
    }
    
  }
 
}

splt = function(data, n, method = 'n_windows', force_equal = FALSE, allow_zero = FALSE, descending = FALSE){
  
  #
  # Takes dataframe or vector
  # Splits into the specified windows
  # Returns list with the windows (dataframes or vectors)
  #
  
  
  ### Allow zero
  
  # If allow_zero is TRUE, and n is 0 
  # return the given data in a list 
  # instead of giving an error
  if (isTRUE(allow_zero) && n == 0){
    
    return(split(data, factor(1)))
    
  }


  ### Check arguments
  
  # Check if given arguments are allowed
  # If not -> stop execution
  check_arguments_(data, n, method, force_equal, allow_zero)
  
  
  ### Convert from percentage
  
  # We check if n is given as percentage
  # This would be done by giving a number between 
  # 0 and 1
  # If it is, we convert it to the actual number 
  # of windows 
  
  if (is_between_(n, 0,1)){
    
    n = convert_percentage_(n, data)
    
  }

  
  ### Check arguments 2 :)
  
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
  
  
  ### Force equal ###
  
  # If force_equal is set to TRUE,
  # and we don't already have equally sized windows,
  # remove elements/rows from data, until we get
  # largest possible equally sized windows
  
  if ( isTRUE(force_equal) && !(is_wholenumber_(nrow(data)/n)) ){
    
    # Multiply window size and number of windows to find
    # how much data to keep
    to_keep = floor(nrow(data)/n)*n
    
    # Keep the first to_keep elements/rows
    data = head(data, to_keep)
    
  }
  
  
  if(method == 'greedy'){
    
    return(gsplit_(data, n, force_equal, allow_zero))
    
  } else if (method == 'n_windows'){
    
    return(nsplit_(data, n, force_equal, allow_zero))
    
  } else if (method == 'n_fill'){
    
    return(n_fill_split_(data, n, force_equal, allow_zero, descending))
    
  }
  
}


## Greedy functions

gsplit_grouping_factor_ = function(v, size, force_equal = FALSE, allow_zero = FALSE){
  
  #
  # Takes a vector and the size of the wanted windows
  # Returns a factor with 1's for window 1, 2's for window 2, etc.
  # This can be used for subsetting, group_by, etc.
  #
  # Notice: The last window will contain fewer elements 
  # if length of the vector isn't divisible with size
  #
  
  
  ### Allow zero ###
  
  # If allow_zero is TRUE and size is 0 
  # return NAs instead of giving an error
  if (isTRUE(allow_zero) && size == 0){
    
    return(rep(NA, each = length(v)))
    
  }
  
  
  ### Check arguments ### Convert from percentage ###
  
  # Check if given arguments are allowed
  # If not -> stop execution
  g_check_arguments_(v, size, force_equal)
  
  # We check if size is given as percentage
  # This would be done by giving a number between 
  # 0 and 1
  # If it is, we convert it to the actual size 
  
  if (is_between_(size, 0,1)){
    
    size = convert_percentage_(size, v)
    
  }
  
  # Check if 
  # .. size is a whole number
  # .. Length of the vector is larger or
  # .. equal to size
  # If not -> stop execution
  stopifnot(arg_is_wholenumber_(size),
            length(v) >= size)
   
  
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

gsplit_ = function(data, size, force_equal = FALSE, allow_zero = FALSE){
  
  #
  # Wrapper function for gdsplt and gvsplit_
  # 
  # Checks if data is a dataframe
  # .. if yes, it calls gdsplit_
  # .. if no, it calls gvsplit_
  # 
  
  if (is.data.frame(data)){
    
    return(gdsplit_(data, size, force_equal, allow_zero))
    
  } else {
    
    return(gvsplit_(data, size, force_equal, allow_zero))
    
  }
  
  
}

gdsplit_ = function(data, size, force_equal = FALSE, allow_zero = FALSE){
  
  #
  # Takes a dataframe
  # Splits the dataframe greedily based on size
  # Returns a list of dataframes
  #
  
  
  ### Creating grouping factor ###
  
  # Create grouping factor
  group = gsplit_grouping_factor_(data[,1], size)
  
  
  ### Split data ###
  
  # Split dataframe into a list of dataframes
  data_splitted = split(data, group)
  
  return(data_splitted)

  
}

gvsplit_ = function(v, size, force_equal = FALSE, allow_zero = FALSE){
  
  #
  # Takes a vector and greedily splits it
  # based on size
  # Returns a list of vectors
  #
  
  
  ### Split data ###
  
  return(split(v, ceiling(seq_along(v)/size)))
  
  
}


## Number of windows functions

nsplit_grouping_factor_ = function(v, n_windows, force_equal = FALSE, allow_zero = FALSE){
  
  #
  # Takes a vector and the number of wanted splits
  # Returns a factor with 1's for window 1, 2's for window 2, etc.
  # This can be used for subsetting, group_by, etc.
  #
  # Notice: The last window will contain fewer OR more elements 
  # if length of the vector isn't divisible with n_windows
  #
  
  
  ### Allow zero ###
  
  # If allow_zero is TRUE and n_windows is 0
  # return NAs instead of giving an error
  if (isTRUE(allow_zero) && n_windows == 0){
    
    return(rep(NA, each = length(v)))
    
  }
  
  
  ### Check arguments ### Convert from percentage ###
  
  # Check if given arguments are allowed
  # If not -> stop execution
  n_check_arguments_(v, n_windows, force_equal)
  
  # We check if n_windows is given as percentage
  # This would be done by giving a number between 
  # 0 and 1
  # If it is, we convert it to the actual number 
  # of windows 
  
  if (is_between_(n_windows, 0,1)){
    
    n_windows = convert_percentage_(n_windows, v)
    
  }
  
  # Check if 
  # .. n_windows is a whole number
  # .. Length of the vector is larger or
  # .. equal to n_windows
  # If not -> stop execution
  stopifnot(arg_is_wholenumber_(n_windows),
            length(v) >= n_windows)
  
  
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
  
  # Try to use use gsplit_grouping_factor_ and check 
  # if it returns the right number of windows
  
  # Set grouping_factor with gsplit_grouping_factor_
  window_grouping_factor = gsplit_grouping_factor_(v, window_size)
  
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
  
nsplit_ = function(data, size, force_equal = FALSE, allow_zero = FALSE){
  
  #
  # Wrapper function for ndsplt and nvsplit_
  # Checks if data is a dataframe
  # .. if yes, it calls ndsplit_
  # .. if no, it calls nvsplit_
  #
  
  
  if (is.data.frame(data)){
    
    return(ndsplit_(data, size, force_equal, allow_zero))
    
  } else {
    
    return(nvsplit_(data, size, force_equal, allow_zero))
    
  }
  
  
}

ndsplit_ = function(data, n_windows, force_equal = FALSE, allow_zero = FALSE){
  
  #
  # Takes a dataframe
  # Splits the dataframe into n_windows
  # Returns a list of dataframes
  #
  
  # Create grouping_factor for splitting data
  group = nsplit_grouping_factor_(data[,1], n_windows)
  
  # Split dataframe into a list of dataframes
  data_splitted = split(data , f = group)
  
  return(data_splitted)
  
}
  
nvsplit_ = function(v, n_windows, force_equal = FALSE, allow_zero = FALSE){
  
  #
  # Takes a vector and splits in into n_windows
  # Returns a list of vectors
  #
  
  # Use nsplit_grouping_factor_() to get a grouping_factor to split by
  split_grouping_factor = nsplit_grouping_factor_(v, n_windows)
  
  
  # Split data
  
  return(split(v , f = split_grouping_factor))
  
}


# Number of windows - equal windows - Fill up (find better name)
# The point is that first all windows are equally big, and then 
# excess datapoints are distributed one at a time ascending/descending

n_fill_split_grouping_factor_ = function(v, n_windows, force_equal = FALSE, allow_zero = FALSE, descending = FALSE){
  
  # Check inputs, allow_zero, etc. 
  # Could these be done in the main function instead?
  
  # Create a grouping factor with the biggest possible equal windows 
  equal_groups = nsplit_grouping_factor_(v, n_windows, force_equal=TRUE)
  
  
  
  excess_data_points = length(v)-length(equal_groups)
  
  if (excess_data_points == 0 || isTRUE(force_equal)){
    
    return(equal_groups)
    
  } 
  
  
  if (isTRUE(descending)){
    
    start_rep = (n_windows-excess_data_points)+1
    
    values_to_add = rep(c(start_rep:n_windows))
    
  } else {
  
    values_to_add = rep(c(1:excess_data_points))
    
  }
  
  grouping_factor = factor(sort(c(as.numeric(equal_groups),values_to_add)))
  
  return(grouping_factor)
  
}

n_fill_split_ = function(data, size, force_equal = FALSE, allow_zero = FALSE, descending = FALSE){
  
  #
  # Wrapper function for nd_fill_split_ and nv_fill_split_
  # Checks if data is a dataframe
  # .. if yes, it calls nd_fill_split_
  # .. if no, it calls nv_fill_split_
  #
  
  
  if (is.data.frame(data)){
    
    return(nd_fill_split_(data, size, force_equal, allow_zero, descending))
    
  } else {
    
    return(nv_fill_split_(data, size, force_equal, allow_zero, descending))
    
  }
  
  
}

nd_fill_split_ = function(data, n_windows, force_equal = FALSE, allow_zero = FALSE, descending = FALSE){
  
  #
  # Takes a dataframe
  # Splits the dataframe into n_windows
  # Returns a list of dataframes
  #
  
  # Create grouping_factor for splitting data
  group = n_fill_split_grouping_factor_(data[,1], n_windows, descending = descending)
  
  
  # Split dataframe into a list of dataframes
  data_splitted = split(data , f = group)
  
  return(data_splitted)
  
}

nv_fill_split_ = function(v, n_windows, force_equal = FALSE, allow_zero = FALSE, descending = FALSE){
  
  #
  # Takes a vector and splits in into n_windows
  # Returns a list of vectors
  #
  
  # Use nsplit_grouping_factor_() to get a grouping_factor to split by
  split_grouping_factor = n_fill_split_grouping_factor_(v, n_windows, descending = descending)
  
  
  # Split data
  
  return(split(v , f = split_grouping_factor))
  
}



