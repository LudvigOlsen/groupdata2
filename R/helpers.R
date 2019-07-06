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

  x > a & x < b
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

  # "data" can be both a data frame or a vector

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

factor_to_num <- function(factor){

  #
  # Convert factor to numeric
  # Return maximum value
  #

  factor %>%
    as.character() %>%
    as.numeric() %>%
    return()

}

#' @importFrom dplyr %>%
max_num_factor <- function(factor){

  #
  # Convert factor to numeric
  # Return maximum value
  #

  factor %>%
    factor_to_num %>%
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
  # Returns data frame with grouping factor
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
  # TODO replace with dplyr join
  data <- merge(data, id_groups, by.x=c(id_col), by.y=c(colnames(id_groups)[1]))

  # Return data
  return(data)

}



replace_col_name <- function(data, old_name, new_name){

  #
  # Replaces name of column in data frame
  #
  colnames(data)[names(data) == old_name] <- new_name
  return(data)

}

get_column_index <- function(data, col){

  #
  # Finds column index in data frame given column name
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

      # Check if there is a column in data frame
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

      # Check if .index exists as column in data frame
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
  summ <- as.integer(round(summary(cat_sizes$n)))
  names(summ) <- c("min","1q","median","mean","3q","max")
  summ
}

get_target_size <- function(data, size, cat_col){
  group_sizes_summary <- find_group_sizes_summary(data, cat_col)
  if (is.character(size)) {
    to_size <- group_sizes_summary[[size]]
  } else {
    to_size <- size
  }
  to_size
}

add_rows_with_sampling <- function(data, to_size){
  extra_rows <- data %>%
    dplyr::sample_n(size=to_size-nrow(.), replace = TRUE) %>%
    dplyr::mutate(.TempNewRow = 1)
  data %>%
    dplyr::bind_rows(extra_rows)
}

select_rows_from_ids <- function(data, balanced_ids, cat_col, id_col,
                                 mark_new_rows, join_fn=dplyr::inner_join){
  # select the chosen ids in data and return
  balanced_data <- join_fn(data, balanced_ids,
                           by = c(cat_col, id_col)) %>%
    update_TempNewRow_from_ids_method()

  # if (!isTRUE(mark_new_rows)) {
  #   balanced_data$.TempNewRow <- NULL
  # }
  balanced_data
}

# TODO Add description of this function.
# I've forgotten what it's for and it's very specific ;)
# (used in select_rows_from_ids above, which is used in sampling_methods.R)
# TODO use create_tmp_var in sampling methods to create unique tmp var instead
update_TempNewRow_from_ids_method <- function(data){
  data %>%
    dplyr::mutate(.TempNewRow = dplyr::if_else(.data$.TempNewRow + .data$.ids_balanced_new_rows > 0, 1, 0)) %>%
    dplyr::select(-c(.data$.ids_balanced_new_rows))
}

## Finding and removing identical columns

# Find columns that are identical values-wise (or group-wise)
# Ignores names of columns
# Exclude comparisons by passing data frame with cols V1 and V2 - e.g. to avoid comparing columns multiple times.
# if return_all_comparisons is TRUE, it returns a list with 1. identical cols, 2. all comparisons
# If group_wise: 1,1,2,2 == 2,2,1,1 (identical groups with different names)
find_identical_cols <- function(data, cols=NULL, exclude_comparisons=NULL,
                                return_all_comparisons=FALSE, group_wise=FALSE,
                                parallel=FALSE){

  if (is.null(cols)){
    cols <- colnames(data)
  }

  column_combinations <- as.data.frame(t(combn(cols, 2)), stringsAsFactors=FALSE)

  # Exclude comparisons if specified
  if (!is.null(exclude_comparisons)){

    # Asserts for exclude_comparisons data frame
    stopifnot(is.data.frame(exclude_comparisons),
              "V1" %in% colnames(exclude_comparisons),
              "V2" %in% colnames(exclude_comparisons))

    column_combinations <- column_combinations %>%
      dplyr::anti_join(exclude_comparisons, by=c("V1", "V2"))
  }

  # To avoid starting parallel processes when they are unnecessary
  # (i.e. add more overhead than saved time)
  # We create some heuristics. TODO: optimize further based on experiments!
  parallel_heuristics <- (
    (nrow(column_combinations) >= 15 && nrow(data) >= 1000) ||
      (nrow(column_combinations) > 100 && nrow(data) > 100) ||
      nrow(column_combinations) > 150
  )

  parallel <- parallel && parallel_heuristics

  # Print statements for checking the effect of running in parallel
  if (FALSE){
    print(paste0("Rows in data frame: ", nrow(data)))
    print(paste0("Number of combinations: ", nrow(column_combinations)))
    print(paste0("Parallel heuristic (do parallel?): ", parallel_heuristics))
  }

  column_combinations[["identical"]] <- plyr::llply(1:nrow(column_combinations),
                                                    .parallel = parallel, function(r){
    col_1 <- data[[column_combinations[r, 1]]]
    col_2 <- data[[column_combinations[r, 2]]]
    if (isTRUE(group_wise)){
      d <- tibble::tibble("col_1" = as.character(col_1),
                          "col_2" = as.character(col_2)) %>%
        dplyr::arrange(.data$col_1) %>%
        group(n = "auto", method = "l_starts", col_name = ".groups_1",
              starts_col = "col_2") %>% dplyr::ungroup()
      if (nlevels(d[[".groups_1"]]) != length(unique(d[["col_1"]]))){
        return(FALSE)
      } else {
        d <- d %>%
          group(n = "auto", method = "l_starts", col_name = ".groups_2",
                starts_col = "col_1") %>% dplyr::ungroup()
      return(isTRUE(dplyr::all_equal(d[[".groups_1"]], d[[".groups_2"]], ignore_row_order = FALSE)))
      }
    } else {
      return(isTRUE(dplyr::all_equal(col_1, col_2, ignore_row_order = FALSE)))
    }

  }) %>% unlist()

  identicals <- column_combinations %>%
    dplyr::filter(identical) %>%
    dplyr::select(c(.data$V1,.data$V2))

  if (isTRUE(return_all_comparisons)){
    return(list(identicals, column_combinations))
  } else {
    return(identicals)
  }

}

# Find identical columns (based on values)
# Remove all but one of these identical columns
# If return_all_comparisons is TRUE, return list with 1. data, 2. all comparisons
# If group_wise: 1,1,2,2 == 2,2,1,1 (identical groups with different names)
remove_identical_cols <- function(data, cols=NULL, exclude_comparisons=NULL,
                                  return_all_comparisons=FALSE,
                                  group_wise=FALSE, parallel=FALSE){

  if (is.null(cols)){
    cols <- colnames(data)
  }

  # Find identicals
  identicals_and_comparisons <- find_identical_cols(data, cols, exclude_comparisons = exclude_comparisons,
                                                    return_all_comparisons = TRUE, group_wise=group_wise,
                                                    parallel=parallel)

  identicals <- identicals_and_comparisons[[1]]
  comparisons <- identicals_and_comparisons[[2]]

  # Find the columns to remove
  to_remove <- unique(identicals[[2]])

  # Remove
  if (is.character(to_remove)){
    data <- data %>% dplyr::select(-dplyr::one_of(to_remove))
  } else if (is.integer(to_remove)){
    data <- data %>% dplyr::select(-to_remove)
  }

  if (isTRUE(return_all_comparisons)){
    return(list("updated_data"=data, "comparisons"=comparisons, "removed_cols"=to_remove))
  } else {
    return(data)
  }


}


rename_with_consecutive_numbering <- function(data, cols, base_name){

  if (is.integer(cols)){
    cols <- colnames(data)[cols]
  }

  num_names_to_create <- length(cols)
  new_names <- paste0(base_name, 1:num_names_to_create)

  data %>%
    dplyr::rename_at(dplyr::vars(cols), ~ new_names)

}

# Add underscore until var name is unique
create_tmp_var <- function(data, tmp_var = ".tmp_index_"){
  while (tmp_var %in% colnames(data)){
    tmp_var <- paste0(tmp_var, "_")
  }
  tmp_var
}

# Used in create_num_col_groups
rename_levels_by_reverse_rank_summary <- function(data, rank_summary, levels_col, num_col){

  current_rank_summary <- create_rank_summary(data, levels_col, num_col)

  reverse_rank_bind <- rank_summary %>%
    dplyr::arrange(dplyr::desc(.data$sum_)) %>%
    dplyr::bind_cols(current_rank_summary)

  pattern_and_replacement <- reverse_rank_bind %>%
    dplyr::select(c(!!as.name(levels_col),
                    !!as.name(paste0(levels_col,"1"))))

  data <- data %>%
    dplyr::left_join(pattern_and_replacement, by=levels_col) %>%
    dplyr::select(-!!as.name(levels_col)) %>%
    dplyr::rename_at(paste0(levels_col,"1"), ~c(levels_col))

  updated_rank_summary <- reverse_rank_bind %>%
    dplyr::mutate(sum_ = .data$sum_ + .data$sum_1) %>%
    dplyr::select(!!as.name(levels_col), .data$sum_)

  list("updated_rank_summary"=updated_rank_summary, "updated_data"=data)

}

create_rank_summary <- function(data, levels_col, num_col){
  data %>%
    dplyr::group_by(!!as.name(levels_col)) %>%
    dplyr::summarize(sum_ = sum(!!as.name(num_col))) %>%
    dplyr::arrange(.data$sum_)
}



# Extracts the major and minor version numbers. E.g. 3.5.
check_R_version <- function(){
  as.numeric(substring(getRversion(), 1, 3))
}

# Skips testthat test, if the R version is below 3.6.0
# WHY? Due to the change in the random sampling generator
# tests fail on R versions below 3.6.0.
# It is possible to fix this by using the old generator for
# unit tests, but that would take a long time to convert,
# and most likely the code works the same on v3.5
skip_test_if_old_R_version <- function(min_R_version = 3.6){
  if(check_R_version() < min_R_version){
    testthat::skip(message = paste0("Skipping test as R version is < ", min_R_version, "."))
  }
}

# Wrapper for setting seed with the sample generator for R versions <3.6
# Used for unittests
# Contributed by R. Mark Sharp
set_seed_for_R_compatibility <- function(seed = 1) {
  version <- as.integer(R.Version()$major) + (as.numeric(R.Version()$minor) / 10.0)
  if (version >= 3.6) {
    args <- list(seed, sample.kind = "Rounding")
  } else {
    args <- list(seed)
  }
  suppressWarnings(do.call(set.seed, args))
}
