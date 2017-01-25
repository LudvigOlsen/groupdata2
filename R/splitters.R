## Dataframe and vector splitters

#' @importFrom utils head tail
dsplit_ <- function(data, n, method, force_equal = FALSE, allow_zero = FALSE,
                   descending = FALSE, randomize = FALSE){

  #
  # Takes a dataframe
  # Creates grouping factor based on method
  # Splits the dataframe
  # Returns a list of dataframes
  #

  if (method == 'staircase') {

    # Create grouping factor
    groups <- group_factor(data[[1]], n, method = method, force_equal = force_equal,
                          descending = descending, randomize = randomize)

    if (isTRUE(force_equal)){

      data <- head(data, length(groups))

    }

  } else {

    # Create grouping factor
    groups <- group_factor(data[[1]], n, method = method, descending = descending,
                          randomize = randomize)

  }
  # Split dataframe into a list of dataframes
  data_splitted <- split(data , f = groups)

  return(data_splitted)

}

vsplit_ <- function(v, n, method, force_equal = FALSE, allow_zero = FALSE,
                   descending = FALSE, randomize = FALSE){

  #
  # Takes a vector
  # Creates grouping factor based on method
  # Splits the vector
  # Returns a list of vectors
  #


  if (method == 'staircase') {

    # Create grouping factor
    groups <- group_factor(v, n, method = method, force_equal = force_equal,
                          descending = descending, randomize = randomize)

    if (isTRUE(force_equal)){

      v <- head(v, length(groups))

    }


  } else {

    # Create grouping factor
    groups <- group_factor(v, n, method = method, descending = descending,
                          randomize = randomize)

  }


  # Split vector into a list of vectors
  data_splitted <- split(v , f = groups)

  return(data_splitted)

}

