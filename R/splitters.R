## Data frame and vector splitters

#' @importFrom utils head tail
dsplit_ <- function(data, n, method, starts_col = NULL, force_equal = FALSE, allow_zero = FALSE,
                   descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE){

  #
  # Takes a data frame
  # Creates grouping factor based on method
  # Splits the data frame
  # Returns a list of dataf rames
  #

  if (method %in% c('staircase', 'primes', 'l_sizes')) {

    # Create grouping factor
    groups <- group_factor(data[[1]], n, method = method,
                           force_equal = force_equal,
                          descending = descending, randomize = randomize)

    if (isTRUE(force_equal)){

      data <- head(data, length(groups))

    }

  } else if (method == 'l_starts'){

    groups <- group_factor(data, n, method = method,
                           starts_col = starts_col,
                           force_equal = force_equal,
                           allow_zero = allow_zero,
                           descending = descending,
                           randomize = randomize,
                           remove_missing_starts = remove_missing_starts)

  } else {

    # Create grouping factor
    groups <- group_factor(data[[1]], n, method = method,
                           descending = descending,
                          randomize = randomize)
  }

  # Split data frame into a list of data frames
  data_splitted <- split(data , f = groups)

  return(data_splitted)

}

vsplit_ <- function(v, n, method, force_equal = FALSE, allow_zero = FALSE,
                   descending = FALSE, randomize = FALSE, remove_missing_starts = FALSE){

  #
  # Takes a vector
  # Creates grouping factor based on method
  # Splits the vector
  # Returns a list of vectors
  #

  if (method %in% c('staircase', 'primes', 'l_sizes')) {

    # Create grouping factor
    groups <- group_factor(v, n, method = method, force_equal = force_equal,
                          descending = descending, randomize = randomize)


    if (isTRUE(force_equal)){

      v <- head(v, length(groups))

    }


  } else {

    # Create grouping factor
    groups <- group_factor(v, n, method = method, descending = descending,
                          randomize = randomize,
                          remove_missing_starts = remove_missing_starts)

  }


  # Split vector into a list of vectors
  data_splitted <- split(v , f = groups)

  return(data_splitted)

}

