## l_ methods ("list x")
# Methods where you specify n as a list or vector


# method: l_sizes
# Creates grouping factor from given list of window sizes
# Under development

# #' @param n List of group sizes
# #' @export
l_sizes <- function(v, n, fixed = FALSE){

  # If any elements in n are between 0-1
  # .. Convert all wholenumbers in n to percentages (0-1)
  # ... in a copy of n
  # .. Calculate for each percentage if it would lead to a group size
  # ... of at least 1. If not, set the percentage to 0.
  ### .. If rounded sum of all percentages == 1
  ### .... Make sure that only length(n) groups are
  ### .... returned, containg all elements of v
  # .. Else
  # .... Let exceeding elements have their own group
  # ..... Unless of course, specified no to (fixed_n(??) = TRUE)
  # .. Create list of group sizes
  # .. Return grouping factor from group sizes
  # Else just return the given group sizes

  # If any values negative or 0
  # raise error.
  if (any(n <= 0)){

    stop("Element <= 0 found in n")

  }

  if (length(n)>length(v)){

    stop("n contains more values than v")

  }

  # Check if any element in n is between 0-1
  n_perc <- lapply(n, function(x) is_between_(x,0,1)) %>%
    unlist()

  # If any percentage values in n
  if (any(n_perc)){

    # Make a copy of n where all elements are in percentage (0-1)
    n_all_perc <- plyr::llply(1:length(n), function(element){

      ifelse(isTRUE(n_perc[element]),
             n[element],
             int_to_perc_(v, n[element])) %>%
        return()

    }) %>% unlist()

    # To calculate the sum of percentages
    # We first want to zero out percentages leading to a
    # group size of 0 because of rounding

    perc_sum_rounded <- n_all_perc %>%
      { ifelse(round(.*length(v)) > 0,.,0) } %>%
      sum() %>%
      round(digits = 2)

    # Check that sum of percentages
    # is above 0 and below or equal
    # to 1

    if (perc_sum_rounded > 1){

      stop("Sum of percentages exceedes 1, excluding percentages leading to a group size of 0.")

    } else if (perc_sum_rounded <= 0) {

      stop("Sum of percentages <= 0")

    }

    # Now find the sum of the elements given as whole numbers
    # We need to make sure that these groups will have the exact number
    # of values given by the user.
    #
    # To recomment:::::
    # We find the percentage of this sum.
    # For each of the other percentages we create new percentages reflecting
    # that we have removed a percentage of the total vector elements

    sum_n_integers <- plyr::llply(1:length(n), function(element){

      ifelse(!isTRUE(n_perc[element]),
             n[element],
             0) %>%
        return()

    }) %>% unlist %>%
      sum()

    # How many elements are left in percentage?
    perc_left <- 1 - (sum_n_integers / length(v))

    # How many elements are left?
    elements_for_perc <- length(v) - sum_n_integers

    # Find group_sizes
    group_sizes <- plyr::llply(1:length(n), function(element){

      if (isTRUE(n_perc[element])){

        # perc_left can't be 0, because it's the divisor
        # elements_for_perc
        ifelse(perc_left < 1, 0,
               (n[element]/perc_left)*elements_for_perc) %>%
          return()

      } else {

        n[element] %>% return()
      }


    }) %>% unlist() %>% floor()

  } else {

    #perc_sum_rounded <- 0
    group_sizes <- n

  }

  # n: vector or list of group sizes

  elements <- plyr::llply(1:length(group_sizes), function(s){

    #print(group_sizes[s])
    return(rep(s, group_sizes[s]))

  }) %>% unlist()


  # Start group number at 1
  #
  # if (!isTRUE( all.equal(min(elements), 1))){
  #
  #   elements = elements -(min(elements)-1)
  #
  #   print('shifted group downwards')
  # }

  # If fixed is TRUE
  # . or we have the right number of elements
  # .. return elements
  # Else
  # .. Add a new group with missing elements


  if(isTRUE(fixed) || length(elements) == length(v)){

    return(elements)

  } else {

    missing_elements <- length(v) - length(elements)

    elements %>%
      append(rep(max(elements)+1, missing_elements)) %>%
      return()

  }


}

# system.time(
#
#   for (i in 5:100){
#
#     a <- l_sizes(c(1:i), c(0.1,0.4,0.5), fixed = FALSE)
#   }
# )
#
# system.time(l_sizes(c(1:20), c(0.2, 0.2, 0.2), fixed = TRUE))
#
# # Create a balanced partition() function

partition <- function(data, p, cat_col = NULL,
                    id_col = NULL, fixed = FALSE) {

  #
  # Balanced partitioning
  # data: dataframe or vector
  # p: list of partitions given as percentage (0-1) or group sizes (wholenumber)
  # cat_col: Categorical variable to balance by
  # id_col: ID column to keep rows with shared IDs in the same partition
  # fixed: Whether you only want the inputted partitions or the exceeding values gets a partition (logical)
  #        FALSE allows you to pass "p = 0.2" and get 2 partions - 0.2 and 0.8
  #


  # Currently not working

  return()



}



# test_that("c(1:7) in l_sizes with n = c(0.1,0.2,0.3,0.4)",{
#
#   expect_
#
#
# })

# system.time(
#   a <- l_sizes(c(1:100000),
#                c(0.2))
# )
#a %>% plyr::count()

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
