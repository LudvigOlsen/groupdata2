# Helper functions

is_wholenumber_ <- function(n) {

  # If n is a whole number
  # .. return TRUE
  # else
  # .. return FALSE

  return( floor(n) == n )
}

arg_is_wholenumber_ <- function(n){

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

arg_is_number_ <- function(n){

  # Checks if n is either an integer or a numeric
  # Returns TRUE if yes, FALSE if no

  if ( is.integer(n) || is.numeric(n) ){

    return(TRUE)

  } else {

    return(FALSE)

  }

}

is_optimal_ <- function(grouping_factor, n_windows) {

  # Takes a grouping factor and the number of windows in it
  # Checks if the difference between the count of values in
  # the last window and the other windows would be bigger or
  # smaller if we removed 1 element from all windows except
  # the last window and added those elements to the last window

  # Count the values of all the windows
  count_values <- plyr::count(as.numeric(grouping_factor))

  # Get the count of values in the first window
  first_count_value <- count_values[1,]$freq

  # Get the count of values in the last window
  last_count_value <- count_values[n_windows,]$freq

  # Get the difference of count values
  difference <- abs(first_count_value-last_count_value)

  # If we had one element less in the first windows
  # and added those to the last window instead,
  # would the last window be closer or further from the others?
  # .. So would the difference be smaller?

  # Remove 1 value from the first window value count
  f2 <- first_count_value-1

  # Add the amount of values that would have been removed
  # to the last window value count
  l2 <- last_count_value+n_windows-1

  # Get the difference between these
  difference2 <- abs(f2-l2)

  # If difference is smaller than difference 2,
  # it means that the original distribution of
  # values was optimal.

  if (difference<difference2){

    return(TRUE)

  } else {

    return(FALSE)

  }

}

convert_percentage_ <- function(per, data) {

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

check_arguments_ <- function(data, n, method, force_equal,
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
              is.vector(data) || is.factor(data),
              length(data) > 0)

  }


}

check_convert_check_ <- function(data, n, method, force_equal,
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

    n <- convert_percentage_(n, data)

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

#' @importFrom dplyr %>%
max_num_factor <- function(factor){

  #
  # Convert factor to numeric
  # Return maximum value
  #

  factor %>%
    as.numeric() %>%
    max() %>%
    return()

}

replace_level <- function(factor, match, replace){

  #
  # Replace the value (match) of a factor level
  # with another value (replace)
  #

  levels(factor)[match(match,levels(factor))] <- replace

  return(factor)

}

group_uniques_ <- function(data, n, id_col, method, col_name='.groups'){

  #
  # Creates groups of unique IDs (e.g. subjects)
  # Returns dataframe with grouping factor
  #

  # Get list of unique IDs in id_col
  unique_ids <- unique(data[[id_col]])

  # Create groups of IDs
  id_groups <- group(unique_ids, n, method = method, randomize = TRUE, col_name = col_name)

  # Add grouping factor to data
  data <- merge(data,id_groups,by.x=c(id_col), by.y=c(colnames(id_groups)[1]))

  # Return data
  return(data)

}

replace_col_name <- function(data, old_name, new_name){

  #
  # Replaces name of column in dataframe
  #
  colnames(data)[names(data) == old_name] <- new_name
  return(data)

}

get_column_index <- function(data, col){

  #
  # Finds column index in dataframe given column name
  # Currently not in use
  #

  return(which( colnames(data)==col ))

}



