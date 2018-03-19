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

int_to_perc_ <- function(data, int) {

  # Converts an integer to percentage of vector elements

  # Example:
  # A vector with 100 elements
  # An integer given as 10
  # returns 0.1 (so 10 percent)
  # Percentage is NOT rounded

  if(is.data.frame(data)){

    return(int/nrow(data))

  } else {

    return(int/length(data))

  }

}

is_between_ <- function(x, a, b) {

  # Checks if x is between a and b

  x > a && x < b
}

`%ni%` <- function(x, table) {

  return(!(x %in% table))

}

isEmpty_ <- function(x){
  return(length(x)==0)
}


check_arguments_ <- function(data, n, method, force_equal,
                            allow_zero, descending,
                            remove_missing_starts){

  # Checks if the given arguments live up to certain rules,
  # which allow them to be used in the function

  # "data" can be both a dataframe or a vector

  stopifnot(method %in% c('greedy',
                          'n_dist',
                          'n_last',
                          'n_fill',
                          'n_rand',
                          'l_sizes',
                          'l_starts',
                          'staircase',
                          'primes'))

  if (!(method %in% c('l_starts','l_sizes'))){

    stopifnot(arg_is_number_(n),
              n > 0)

  } else if (method == 'l_starts'){

    # Check n for l_starts
    stopifnot(is.list(n) || is.vector(n) || n == 'auto')
    stopifnot(is.logical(remove_missing_starts))

  } else if (method == 'l_sizes'){

    stopifnot(is.list(n) || is.vector(n) && !is.character(n))
  }



  # Stop execution if input variables aren't what we expect / can handle
  stopifnot((!is.null(n)),
            is.logical(force_equal),
            is.logical(allow_zero),
            is.logical(descending))

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
                                allow_zero, descending,
                                remove_missing_starts,
                                starts_col = NULL){

  # Checks arguments
  # Converts n if given as percentage
  # Checks more arguments
  # Returns the converted/non-converted n

  # Notice: This is used in more than one of the main functions
  # so I put it in a function to make those functions more readable

  ### Check arguments

  # Check if given arguments are allowed
  # If not -> stop execution
  check_arguments_(data, n, method, force_equal, allow_zero, descending, remove_missing_starts)

  if (!(method %in% c('l_starts','l_sizes'))){


    ### Convert from percentage

    # We check if n is given as percentage
    # This would be done by giving a number between
    # 0 and 1
    # If it is, we convert it to the actual number
    # of windows

    if (is_between_(n, 0,1)){

      n <- convert_percentage_(n, data)

      # If the percentage given returns 0
      # throw an error
      stopifnot(n > 0)

    }

    stopifnot(arg_is_wholenumber_(n))


    ### Check arguments 2

    # Check if
    # .. n is a whole number
    # .. Length of the data is larger or
    # .. equal to n
    # If not -> stop execution


    if(is.data.frame(data)){

      stopifnot(nrow(data) >= n)

    } else {

      stopifnot(length(data) >= n)
    }

  } else {

    if(is.data.frame(data)){

      stopifnot(nrow(data) >= length(n))

      if (method == 'l_starts' && is.null(starts_col)){

        stop("'starts_col' cannot be NULL when using method 'l_starts' with a data.frame.")

      }

    } else {

      stopifnot(length(data) >= length(n))
    }


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
    as.character() %>%
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

group_uniques_ <- function(data, n, id_col, method, starts_col = NULL,
                           col_name='.groups', force_equal = FALSE,
                           remove_missing_starts = FALSE){

  #
  # Creates groups of unique IDs (e.g. subjects)
  # Returns dataframe with grouping factor
  #

  # Get list of unique IDs in id_col
  unique_ids <- unique(data[[id_col]])

  # Create groups of IDs
  id_groups <- group(unique_ids, n, method = method,
                     starts_col = starts_col,
                     randomize = TRUE, col_name = col_name,
                     force_equal = force_equal,
                     remove_missing_starts = remove_missing_starts)

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

create_n_primes <- function(n, start_at=2){

  #
  # Create a specific number of primes
  # start_at: start prime numbers at (integer)
  #

  # Check if start_at is prime
  if (!numbers::isPrime(start_at)){

    stop("start_at is not a prime number")

  }

  stopifnot(n>1)

  # Initialize n_primes
  # Counter for created groups
  n_primes <- 0

  # Initialize exponent
  # Used to create a large set of primes to subset from
  exp <- 1

  while (n_primes < n){

    # Generate a set of primes
    primes <- numbers::Primes(n*100^exp)

    # Remove primes lower than start_at
    primes <- primes[primes >= start_at]

    # Get number of generated primes
    n_primes <- length(primes)

    # Add 1 to exp
    exp <- exp+1

  }

  # Return n primes
  return(primes[0:n])

}


# l_starts helpers

relist_starts_ <- function(list){

  list %>% unlist() %>% splt(n = 2, method = 'greedy') %>% return()

}

extract_start_values_ <- function(nested_list){

  unlisted <- nested_list %>% unlist()

  return(unlisted[seq(1, length(unlisted), 2)])

}

assign_starts_col <- function(data, starts_col) {

  if(is.data.frame(data) && !is.null(starts_col)){

    # If starts_col is 'index', create column with row names for matching values
    if (starts_col == 'index'){

      # Check if there is a column in dataframe
      # called 'index'
      # If so, throw warning that the index column in
      # data will be used.
      # Use the 'index' colum present in data.

      if ('index' %in% colnames(data)){

        warning("data contains column named 'index'. This is used as starts_col instead of row names.
                Change starts_col to \'.index\' to use row names - no matter if \'.index\' exists in data.")

        starts_col <- data[[starts_col]]

        # Else get the row names of data to use as starts_col
      } else {

        starts_col <- rownames(data)

      }

      # Else if starts_col is '.index'
      # get row names no matter if it exists already
      # in data
    } else if (starts_col == '.index') {

      # Check if .index exists as column in dataframe
      # If so, warn that it will not be used.
      if ('.index' %in% colnames(data)){

        warning("data contains column named '.index' but this is ignored. Using row names as starts_col instead.")

      }

      # Get the row names of data to use as starts_col
      starts_col <- rownames(data)

      # If starts_col is not NULL (and not 'index')
      # Check that the column exists in data
      # and get the column from data
    } else {

      # If starts_col is wholenumber
      # convert to integer
      if (arg_is_wholenumber_(starts_col)) starts_col <- as.integer(starts_col)

      # If the column is given as name (string),
      # check if the column exists in data
      if (starts_col %ni% colnames(data) && !is.integer(starts_col)){

        stop(paste("starts_col '", starts_col,
                   "' not found in data.frame.", sep = ""))


        # Else if starts_col is given as integer (col index)
        # Check if the number is in the column indices list
      } else if (is.integer(starts_col) && starts_col %ni% col(data)[1,]){

        stop(paste("starts_col with index '", starts_col,
                   "' not found in data.frame.", sep = ""))

      } else {

        starts_col <- data[[starts_col]]

      }

    }

  }

  return(starts_col)
}

l_starts_find_indices_ <- function(v, n_list, remove_missing_starts){

  #
  # Note:
  # When using recursion to remove missing starts
  # we currently rerun the entire finding of indices.
  # This is pretty fast, but perhaps it would be even
  # faster to only rerun for the indices after the
  # already found indices. I.e. if the last found start value
  # was the fifth element of v, we don't need to match
  # start values before index 5 again.
  # This means updating variables and subsetting of data
  # though, so perhaps it's not faster?
  #

  # Initialize ind_prev
  # This is used to make sure that we get an index
  # further down in v, even if the value is also
  # found above the previously found index
  ind_prev <- 0


  tryCatch({

    # We iterate through n and find the index for each value
    indices <- plyr::llply(1:length(n_list), function(i){

      # Get all indices of v where it has the current value of n
      indices <- which(v == n_list[[i]][1])

      # Get all the indices that are larger the the index found in
      # the previous iteration
      indices_larger_than_prev <- indices[which(indices > ind_prev)]

      # Get the wanted index
      ind_next = indices_larger_than_prev[as.integer(n_list[[i]][2])]

      # Set ind_prev to the index we just found for use in the
      # next iteration
      # <<- saves to parent scope (outer function)
      ind_prev <<- ind_next


      # If a value is not found
      # ind_next will be NA
      # In this case we remove the start_value
      # or raise an error
      if (is.na(ind_next)){

        if (isTRUE(remove_missing_starts)){

          # Delete the start value that wasn't found
          # We delete it in the parent scope, so it
          # is used when calling the function again
          # recursively
          n_list[[i]] <<- NULL

          stop("Missing start value removed from n_list. You should not be seeing this error. Please contact the author.")

        } else {

          # Raise error
          stop(paste("Start value \"", n_list[[i]][1], "\" not found in vector.", sep=""))

        }

      }

      # Return the found index
      return(ind_next)

    })

    return(list(indices, n_list))

  }, error = function(e){

    # Removed missing start value? Use recursion.
    if (grepl('Missing start value removed from n_list', e$message)){

      return(l_starts_find_indices_(v, n_list, remove_missing_starts))

    } else {

      stop(e$message)

    }

  })

}

# Sampling

find_group_sizes_summary <- function(data, cat_col){
  cat_sizes <- data %>%
    dplyr::count(!! as.name(cat_col))
  summ <- as.integer(summary(cat_sizes$n))
  names(summ) <- c("min","1q","median","mean","3q","max")
  summ
}

add_rows_with_sampling <- function(data, to_size){
  extra_rows <- data %>%
    dplyr::sample_n(size=to_size-nrow(.), replace = TRUE)
  data %>%
    dplyr::bind_rows(extra_rows)
}

