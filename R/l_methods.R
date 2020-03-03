## l_ methods ("list x")
# Methods where you specify n as a list or vector


# method: l_sizes
# Creates grouping factor from given list of window sizes
# Under development

l_sizes_group_factor_ <- function(v, n, force_equal = FALSE, descending = FALSE) {

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
  # ..... Unless of course, specified no to (force_equal = TRUE)
  # .. Create list of group sizes
  # .. Return grouping factor from group sizes
  # Else just return the given group sizes

  # If any values negative or 0
  # raise error.
  if (any(n <= 0)) {
    stop("Element <= 0 found in n")
  }

  stopifnot(
    is.logical(force_equal),
    is.logical(descending)
  )

  # If n is given as a list() object
  # unlist n to get a c() vector
  if (typeof(n) == "list" && !is.data.frame(n)) {
    n <- unlist(n)
  }

  # If n is longer than v
  # raise an error
  if (length(n) > length(v)) {
    stop("n contains more values than v")
  }

  # Check if any element in n is between 0-1
  n_perc <- lapply(n, function(x) is_between_(x, 0, 1)) %>%
    unlist()

  # If any percentage values in n
  if (any(n_perc)) {

    # Make a copy of n where all elements are in percentage (0-1)
    n_all_perc <- plyr::llply(seq_along(n), function(element) {
      ifelse(isTRUE(n_perc[element]),
        n[element],
        int_to_perc_(v, n[element])
      )
    }) %>% unlist()

    # To calculate the sum of percentages
    # We first want to zero out percentages leading to a
    # group size of 0 because of rounding

    perc_sum_rounded <- n_all_perc %>%
      {
        ifelse(round(. * length(v)) > 0, ., 0)
      } %>%
      sum() %>%
      round(digits = 2)

    # Check that sum of percentages
    # is above 0 and below or equal
    # to 1

    if (perc_sum_rounded > 1) {
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

    sum_n_integers <- plyr::llply(seq_along(n), function(element) {
      ifelse(!isTRUE(n_perc[element]),
        n[element],
        0
      )
    }) %>%
      unlist() %>%
      sum()

    # How many elements are left in percentage?
    perc_left <- 1 - (sum_n_integers / length(v))

    # How many elements are left?
    elements_for_perc <- length(v) - sum_n_integers

    # Find group_sizes
    group_sizes <- plyr::llply(seq_along(n), function(element) {
      if (isTRUE(n_perc[element])) {

        # perc_left can't be 0, because it's the divisor
        # elements_for_perc
        gs <- ifelse(perc_left < 1, 0,
                    (n[element] / perc_left) * elements_for_perc)
      } else {
        gs <- n[element]
      }
      gs
    }) %>%
      unlist() %>%
      floor()
  } else {

    # perc_sum_rounded <- 0
    group_sizes <- n
  }

  # n: vector or list of group sizes

  elements <- plyr::llply(seq_along(group_sizes), function(s) {
    rep(s, group_sizes[s])
  }) %>% unlist()


  # Check that we do not create more values than is in v
  if (length(elements) > length(v)) {
    stop("'n' creates more values than is in 'v'")
  }

  # Start group number at 1
  #
  # if (!isTRUE( all.equal(min(elements), 1))){
  #
  #   elements = elements -(min(elements)-1)
  #
  #   print('shifted group downwards')
  # }

  # If force_equal is TRUE
  # . or we have the right number of elements
  # .. return elements
  # Else
  # .. Add a new group with missing elements


  if (isTRUE(force_equal) || length(elements) == length(v)) {

    # If there are no elements, return NA
    if (length(elements) == 0) {
      warning("No groups. Returned NA.")
      gf <- as.factor(rep(NA, length(v)))
    } else {
      gf <- as.factor(elements)
    }
  } else {
    missing_elements <- length(v) - length(elements)

    # If there are no elements
    if (length(elements) == 0) {
      gf <- as.factor(rep(2, missing_elements))
    } else {
      gf <- elements %>%
        append(rep(max(elements) + 1, missing_elements)) %>%
        as.factor()
    }
  }

  gf
}




l_starts_group_factor_ <- function(v, n, force_equal = FALSE, descending = FALSE,
                                   remove_missing_starts = FALSE,
                                   return_missing_starts = FALSE,
                                   return_missing_starts_skip_numbers = FALSE) {

  #
  # method: l_starts
  # Takes values to start groups at
  # Allows skipping of values
  # Under development
  #

  stopifnot(
    is.logical(force_equal),
    is.logical(descending)
  )

  # If we want to return the missing starts,
  # We need to recursively remove them first
  if (isTRUE(return_missing_starts)) remove_missing_starts <- TRUE


  # Check if n is 'auto'

  # If n is not a list
  # N.b. because n == 'auto' on a list issues warning
  if (!is.list(n)) {

    # And n is 'auto'
    if (n[1] == "auto") {

      # Replace NAs
      tmp_na_val <- create_tmp_val(v)
      v[is.na(v)] <- tmp_na_val

      # Find starts
      n <- find_starts(v)
    }
  }

  # For each element in n
  # .. If it has length 1, return (element, 1)
  # .. Else if it has length 2, return element
  # .. Else raise error, as it has too many values

  n_list <- plyr::llply(seq_along(n), function(element) {
    if (length(n[[element]]) == 1) {
      return(c(n[[element]], 1))
    } else if (length(n[[element]]) == 2) {
      return(n[[element]])
    } else {
      stop("An element of n contains too many elements! Try: c(pattern, number)")
    }
  })

  # We need to start with 1 from the beginning,
  # so all values of v gets a group id
  # So if user does not list the first value of v as the first value
  # of n, we insert it.
  if (n_list[[1]][1] != v[1]) {

    # Insert first value of v in the beginning
    # of n
    n_list <- append(n_list, list(c(as.character(v[1]), 1)), 0)
  }

  n_list_backup <- n_list

  # We use tryCatch to catch the error if a start value is not found
  # TODO What's in start_indices here??? Nothing is returned right? !!!!!!!!!!!!!!
  start_indices <- tryCatch({
    # Find indices
    # Put in function to enable recursion when removing
    # start values not found
    found <- l_starts_find_indices_(
      v = v, n_list = n_list,
      remove_missing_starts = remove_missing_starts)
    n_list <- found[2]
    ind_next <- found[1]
    # If an error was caught
  },
  error = function(e) {
    # Raise error with
    stop(paste("group_factor: ", e$message, sep = ""))
  })

  if (isTRUE(return_missing_starts)) {

    #
    # If what we want is the missing starts,
    # we find the values that were recursively
    # removed in l_starts_find_indices_.
    # We can either return values with related skip numbers
    # or only values.
    #

    n_list_backup <- relist_starts_(n_list_backup)
    n_list <- relist_starts_(n_list)

    missing_starts <- setdiff(n_list_backup, n_list)
    if (isEmpty_(missing_starts)) {
      return(NULL)
    } else if (isTRUE(return_missing_starts_skip_numbers)) {
      return(missing_starts)
    } else {
      missing_values <- missing_starts %>% extract_start_values_()
      return(missing_values)
    }
  }


  # Get the group sizes by taking the difference
  # between each index (so indices 1,5,7,8 get group sizes 4,2,1)
  group_sizes <- diff(unlist(start_indices))

  # Now we're lacking group sizes for the last group,
  # because there is no end index to find difference with
  # So we find the number of missing values and insert it
  # at the end of group_sizes
  group_sizes <- append(group_sizes, (length(v) - sum(group_sizes)))

  # Return the grouping factor
  l_sizes_group_factor_(v = c(1:sum(group_sizes)),
                        n = group_sizes)
}
