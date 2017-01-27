## l_ methods ("list x")
# Methods where you specify n as a list or vector


# method: l_sizes
# Creates grouping factor from given list of window sizes
# Under development

# #' @param n List of group sizes
# #' @export
l_sizes <- function(n){

  # n: vector or list of group sizes

  elements <- plyr::llply(1:length(n), function(s){

    return(rep(s, n[s]))

  })

  return(unlist(elements))
}

# system.time(
#   a <- l_sizes(c(30000,50000,60000, 30000,50000,60000,
#                  30000,50000,60000, 30000,50000,60000,
#                  30000,50000,60000, 30000,50000,60000))
# )


# method: l_starts
# Takes values to start groups at
# Allows skipping of values
# Under development

# #' @param v Vector
# #' @param n List of values to start groups from
# #' @export
l_starts <- function(v, n){

  #
  # method: l_starts
  # Takes values to start groups at
  # Allows skipping of values
  # Under development
  #

  # For each element in n
  # .. If it has length 1, return (element, 1)
  # .. Else if it has length 2, return element
  # .. Else raise error, as it has too many values

  n_list <- plyr::llply(1:length(n), function(element){

    if (length(n[[element]]) == 1){

      return(c(n[[element]],1))

    } else if (length(n[[element]]) == 2){

      return(n[[element]])

    } else {

      stop('An element of n contains too many elements! Try: c(pattern, number)')
    }

  })

  # We need to start with 1 from the beginning,
  # so all values of v gets a group id
  # So if user does not list the first value of v as the first value
  # of n, we insert it.
  if (n_list[[1]][1] != v[1]){

    # Insert first value of v in the beginning
    # of n
    n_list <- append(n_list, list(c(v[1],1)), 0)

  }

  # Initialize ind_prev
  # This is used to make sure that we get an index
  # further down in v, even if the value is also
  # found above the previously found index
  ind_prev <- 0

  # We iterate through n and find the index for each value
  start_indices <- plyr::llply(1:length(n_list), function(i){

    # Get all indices of v where it has the current value of n
    indices <- which(v == n_list[[i]][1])

    # Get all the indices that are larger the the index found in
    # the previous iteration
    indices_larger_than_prev <- indices[which(indices > ind_prev)]

    # Get the wanted index
    ind_next = indices_larger_than_prev[n_list[[i]][2]]

    # Set ind_prev to the index we just found for use in the
    # next iteration
    # <<- saves to parent scope (outer function)
    ind_prev <<- ind_next

    # Return the found index
    return(ind_next)

  })

  # Get the group sizes by taking the difference
  # between each index (so indices 1,5,7,8 get group sizes 4,2,1)
  group_sizes <- diff(unlist(start_indices))

  # Now we're lacking group sizes for the last group,
  # because there is no end index to find difference with
  # So we find the number of missing values and insert it
  # at the end of group_sizes
  group_sizes <- append(group_sizes, (length(v)-sum(group_sizes)))

  # Return the grouping factor
  return(l_sizes(group_sizes))

}

# sampleData <- rep(sample(1:1000, 1000),100)
#length(sampleData)
#
# system.time(
#   l_starts(sampleData, n=sample(1:1000,100))
# )
#
# system.time(
#   group_factor(sampleData, 100)
# )
