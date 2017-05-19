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

l_starts_find_indices <- function(v, n_list, remove_missing_starts){

  # Initialize ind_prev
  # This is used to make sure that we get an index
  # further down in v, even if the value is also
  # found above the previously found index
  ind_prev <- 0


  tryCatch({

    # We iterate through n and find the index for each value
    plyr::llply(1:length(n_list), function(i){

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

  }, error = function(e){

    # Removed missing start value? Use recursion.
    if (grepl('Missing start value removed from n_list', e$message)){

      return(l_starts_find_indices(v, n_list, remove_missing_starts))

    } else {

      stop(e$message)

    }

  })

}
