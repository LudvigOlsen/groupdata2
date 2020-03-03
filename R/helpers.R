# Helper functions

is_wholenumber_ <- function(n) {
  floor(n) == n
}

arg_is_wholenumber_ <- function(n) {

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

  is.integer(n) || (is.numeric(n) && is_wholenumber_(n))
}

# TODO Replace with simply is.numeric
# integer is numeric
arg_is_number_ <- function(n) {

  # Checks if n is either an integer or a numeric
  # Returns TRUE if yes, FALSE if no

  is.integer(n) || is.numeric(n)
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
  first_count_value <- count_values[1, ]$freq

  # Get the count of values in the last window
  last_count_value <- count_values[n_windows, ]$freq

  # Get the difference of count values
  difference <- abs(first_count_value - last_count_value)

  # If we had one element less in the first windows
  # and added those to the last window instead,
  # would the last window be closer or further from the others?
  # .. So would the difference be smaller?

  # Remove 1 value from the first window value count
  f2 <- first_count_value - 1

  # Add the amount of values that would have been removed
  # to the last window value count
  l2 <- last_count_value + n_windows - 1

  # Get the difference between these
  difference2 <- abs(f2 - l2)

  # If difference is smaller than difference 2,
  # it means that the original distribution of
  # values was optimal.
  difference < difference2
}

convert_percentage_ <- function(per, data) {

  # Converts a percentage of vector elements
  # into a count of elements

  # Example:
  # A vector with 100 elements
  # A percentage given as 0.1 (so 10 percent)
  # Returns 10

  size_fn <- length
  if (is.data.frame(data)) {
    size_fn <- nrow
  }

  floor(size_fn(data) * per)
}

int_to_perc_ <- function(data, int) {

  # Converts an integer to percentage of vector elements

  # Example:
  # A vector with 100 elements
  # An integer given as 10
  # returns 0.1 (so 10 percent)
  # Percentage is NOT rounded

  if (is.data.frame(data)) {
    return(int / nrow(data))
  } else {
    return(int / length(data))
  }
}

is_between_ <- function(x, a, b) {

  # Checks if x is between a and b
  x > a & x < b
}

`%ni%` <- function(x, table) {
  !(x %in% table)
}

isEmpty_ <- function(x) {
  length(x) == 0
}

check_arguments_ <- function(data, n, method, force_equal,
                             allow_zero, descending,
                             remove_missing_starts) {

  # Checks if the given arguments live up to certain rules,
  # which allow them to be used in the function

  # "data" can be both a data frame or a vector

  stopifnot(method %in% c(
    "greedy",
    "n_dist",
    "n_last",
    "n_fill",
    "n_rand",
    "l_sizes",
    "l_starts",
    "staircase",
    "primes"
  ))

  if (!(method %in% c("l_starts", "l_sizes"))) {
    stopifnot(
      arg_is_number_(n),
      n > 0
    )
  } else if (method == "l_starts") {

    # Check n for l_starts
    stopifnot(is.list(n) || is.vector(n) || n == "auto")
    stopifnot(is.logical(remove_missing_starts))
  } else if (method == "l_sizes") {
    stopifnot(is.list(n) || is.vector(n) && !is.character(n))
  }

  # Stop execution if input variables aren't what we expect / can handle
  stopifnot(
    (!is.null(n)),
    is.logical(force_equal),
    is.logical(allow_zero),
    is.logical(descending)
  )

  if (is.data.frame(data)) {

    # Stop execution if input variables aren't what we expect / can handle
    stopifnot(nrow(data) > 0)
  } else {

    # Stop execution if input variables aren't what we expect / can handle
    stopifnot(
      (!is.null(data)),
      is.vector(data) || is.factor(data),
      length(data) > 0
    )
  }
}

check_convert_check_ <- function(data, n, method, force_equal,
                                 allow_zero, descending,
                                 remove_missing_starts,
                                 starts_col = NULL) {

  # Checks arguments
  # Converts n if given as percentage
  # Checks more arguments
  # Returns the converted/non-converted n

  # Notice: This is used in more than one of the main functions
  # so I put it in a function to make those functions more readable

  ### Check arguments

  # Check if given arguments are allowed
  # If not -> stop execution
  check_arguments_(
    data = data,
    n = n,
    method = method,
    force_equal = force_equal,
    allow_zero = allow_zero,
    descending = descending,
    remove_missing_starts = remove_missing_starts
  )

  if (!(method %in% c("l_starts", "l_sizes"))) {

    ### Convert from percentage

    # We check if n is given as percentage
    # This would be done by giving a number between
    # 0 and 1
    # If it is, we convert it to the actual number
    # of windows

    if (is_between_(n, 0, 1)) {
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


    if (is.data.frame(data)) {
      stopifnot(nrow(data) >= n)
    } else {
      stopifnot(length(data) >= n)
    }
  } else {
    if (is.data.frame(data)) {
      stopifnot(nrow(data) >= length(n))

      if (method == "l_starts" && is.null(starts_col)) {
        stop("'starts_col' cannot be NULL when using method 'l_starts' with a data.frame.")
      }
    } else {
      stopifnot(length(data) >= length(n))
    }
  }

  return(n)
}

factor_to_num <- function(f) {

  #
  # Convert factor to numeric
  # Return maximum value
  #
  as.numeric(as.character(f))
}

#' @importFrom dplyr %>%
max_num_factor <- function(f) {

  #
  # Convert factor to numeric
  # Return maximum value
  #
  max(factor_to_num(f))
}

replace_level <- function(f, match, replace) {

  #
  # Replace the value (match) of a factor level
  # with another value (replace)
  #

  levels(f)[match(match, levels(f))] <- replace

  f
}

group_uniques_ <- function(data, n, id_col, method, starts_col = NULL,
                           col_name = ".groups", force_equal = FALSE,
                           remove_missing_starts = FALSE) {

  #
  # Creates groups of unique IDs (e.g. subjects)
  # Returns data frame with grouping factor
  #

  # Get list of unique IDs in id_col
  unique_ids <- unique(data[[id_col]])

  # Create groups of IDs
  id_groups <- group(
    data = unique_ids,
    n = n,
    method = method,
    starts_col = starts_col,
    randomize = TRUE,
    col_name = col_name,
    force_equal = force_equal,
    remove_missing_starts = remove_missing_starts
  )

  # Add grouping factor to data
  # TODO replace with dplyr join
  data <- merge(data, id_groups, by.x = c(id_col),
                by.y = c(colnames(id_groups)[1]))

  data
}


get_column_index <- function(data, col) {

  #
  # Finds column index in data frame given column name
  # Currently not in use
  #

  which(colnames(data) == col)
}

create_n_primes <- function(n, start_at = 2) {

  #
  # Create a specific number of primes
  # start_at: start prime numbers at (integer)
  #

  # Check if start_at is prime
  if (!numbers::isPrime(start_at)) {
    stop("start_at is not a prime number")
  }

  stopifnot(n > 1)

  # Initialize n_primes
  # Counter for created groups
  n_primes <- 0

  # Initialize exponent
  # Used to create a large set of primes to subset from
  exp <- 1

  while (n_primes < n) {

    # Generate a set of primes
    primes <- numbers::Primes(n * 100^exp)

    # Remove primes lower than start_at
    primes <- primes[primes >= start_at]

    # Get number of generated primes
    n_primes <- length(primes)

    # Add 1 to exp
    exp <- exp + 1
  }

  # Return n primes
  primes[0:n]
}


# l_starts helpers

relist_starts_ <- function(l) {
  l %>%
    unlist() %>%
    splt(n = 2, method = "greedy")
}

extract_start_values_ <- function(nested_list) {
  unlisted <- nested_list %>% unlist()
  unlisted[seq(1, length(unlisted), 2)]
}

assign_starts_col <- function(data, starts_col) {
  if (is.data.frame(data) && !is.null(starts_col)) {

    # If starts_col is 'index', create column with row names for matching values
    if (starts_col == "index") {

      # Check if there is a column in data frame
      # called 'index'
      # If so, throw warning that the index column in
      # data will be used.
      # Use the 'index' colum present in data.

      if ("index" %in% colnames(data)) {
        warning(
          paste0(
            "'data' contains column named 'index'. This is used as starts_",
            "col instead of row names. Change 'starts_col' to '.index' to",
            " use row names - no matter if '.index' exists in data."
          )
        )

        starts_col <- data[[starts_col]]

        # Else get the row names of data to use as starts_col
      } else {
        starts_col <- rownames(data)
      }

      # Else if starts_col is '.index'
      # get row names no matter if it exists already
      # in data
    } else if (starts_col == ".index") {

      # Check if .index exists as column in data frame
      # If so, warn that it will not be used.
      if (".index" %in% colnames(data)) {
        warning(paste0("data contains column named '.index' but this is ignored. Us",
                       "ing row names as starts_col instead."))
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
      if (starts_col %ni% colnames(data) && !is.integer(starts_col)) {
        stop(paste("starts_col '", starts_col,
          "' not found in data.frame.",
          sep = ""
        ))

        # Else if starts_col is given as integer (col index)
        # Check if the number is in the column indices list
      } else if (is.integer(starts_col) && starts_col %ni% col(data)[1, ]) {
        stop(paste("starts_col with index '", starts_col,
          "' not found in data.frame.",
          sep = ""
        ))
      } else {
        starts_col <- data[[starts_col]]
      }
    }
  }

  starts_col
}

l_starts_find_indices_ <- function(v, n_list, remove_missing_starts) {

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


  tryCatch(
    {

      # We iterate through n and find the index for each value
      indices <- plyr::llply(seq_along(n_list), function(i) {

        # Get all indices of v where it has the current value of n
        indices <- which(v == n_list[[i]][1])

        # Get all the indices that are larger the the index found in
        # the previous iteration
        indices_larger_than_prev <- indices[which(indices > ind_prev)]

        # Get the wanted index
        ind_next <- indices_larger_than_prev[as.integer(n_list[[i]][2])]

        # Set ind_prev to the index we just found for use in the
        # next iteration
        # <<- saves to parent scope (outer function)
        ind_prev <<- ind_next

        # If a value is not found
        # ind_next will be NA
        # In this case we remove the start_value
        # or raise an error
        if (is.na(ind_next)) {
          if (isTRUE(remove_missing_starts)) {

            # Delete the start value that wasn't found
            # We delete it in the parent scope, so it
            # is used when calling the function again
            # recursively
            n_list[[i]] <<- NULL

            stop(paste0("Missing start value removed from n_list. You should not be ",
                        "seeing this error. Please contact the author."))
          } else {

            # Raise error
            stop(paste("Start value \"", n_list[[i]][1], "\" not found in vector.", sep = ""))
          }
        }

        # Return the found index
        return(ind_next)
      })

      return(list(indices, n_list))
    },
    error = function(e) {

      # Removed missing start value? Use recursion.
      if (grepl("Missing start value removed from n_list", e$message)) {
        return(
          l_starts_find_indices_(
            v = v,
            n_list = n_list,
            remove_missing_starts = remove_missing_starts
          )
        )
      } else {
        stop(e$message)
      }
    }
  )
}

# Sampling

find_group_sizes_summary <- function(data, cat_col) {
  cat_sizes <- data %>%
    dplyr::count(!!as.name(cat_col))
  summ <- as.integer(round(summary(cat_sizes$n)))
  names(summ) <- c("min", "1q", "median", "mean", "3q", "max")
  summ
}

get_target_size <- function(data, size, cat_col) {
  group_sizes_summary <- find_group_sizes_summary(data, cat_col)
  if (is.character(size)) {
    to_size <- group_sizes_summary[[size]]
  } else {
    to_size <- size
  }
  to_size
}

add_rows_with_sampling <- function(data, to_size, new_rows_col_name) {
  extra_rows <- data %>%
    dplyr::sample_n(size = to_size - nrow(.), replace = TRUE)
  extra_rows[[new_rows_col_name]] <- 1
  data %>%
    dplyr::bind_rows(extra_rows)
}

select_rows_from_ids <- function(data, balanced_ids, cat_col, id_col,
                                 mark_new_rows, join_fn = dplyr::inner_join,
                                 new_rows_col_name, ids_new_rows_col_name) {
  # select the chosen ids in data and return
  balanced_data <- join_fn(data, balanced_ids,
    by = c(cat_col, id_col)
  ) %>%
    update_TempNewRow_from_ids_method(
      new_rows_col_name = new_rows_col_name,
      ids_new_rows_col_name = ids_new_rows_col_name
    )

  # if (!isTRUE(mark_new_rows)) {
  #   balanced_data$.TempNewRow <- NULL
  # }
  balanced_data
}

# TODO Add description of this function.
# TODO use create_tmp_var in sampling methods to create unique tmp var instead
update_TempNewRow_from_ids_method <- function(data, new_rows_col_name, ids_new_rows_col_name) {
  data[[new_rows_col_name]] <- dplyr::if_else(
    data[[new_rows_col_name]] + data[[ids_new_rows_col_name]] > 0, 1, 0
  )
  data[[ids_new_rows_col_name]] <- NULL
  data
}

## Finding and removing identical columns

# Find columns that are identical values-wise (or group-wise)
# Ignores names of columns
# Exclude comparisons by passing data frame with cols V1 and V2 - e.g. to avoid comparing columns multiple times.
# if return_all_comparisons is TRUE, it returns a list with 1. identical cols, 2. all comparisons
# If group_wise: 1,1,2,2 == 2,2,1,1 (identical groups with different names)
find_identical_cols <- function(data, cols = NULL, exclude_comparisons = NULL,
                                return_all_comparisons = FALSE,
                                group_wise = FALSE, parallel = FALSE) {
  if (is.null(cols)) {
    cols <- colnames(data)
  }

  column_combinations <- as.data.frame(t(combn(cols, 2)), stringsAsFactors = FALSE)

  # Exclude comparisons if specified
  if (!is.null(exclude_comparisons)) {

    # Asserts for exclude_comparisons data frame
    stopifnot(
      is.data.frame(exclude_comparisons),
      "V1" %in% colnames(exclude_comparisons),
      "V2" %in% colnames(exclude_comparisons)
    )

    column_combinations <- column_combinations %>%
      dplyr::anti_join(exclude_comparisons, by = c("V1", "V2"))
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
  if (FALSE) {
    print(paste0("Rows in data frame: ", nrow(data)))
    print(paste0("Number of combinations: ", nrow(column_combinations)))
    print(paste0("Parallel heuristic (do parallel?): ", parallel_heuristics))
  }

  column_combinations[["identical"]] <- plyr::llply(seq_len(nrow(column_combinations)),
    .parallel = parallel, function(r) {
      col_1 <- data[[column_combinations[r, 1]]]
      col_2 <- data[[column_combinations[r, 2]]]
      if (isTRUE(group_wise)) {
        return(all_groups_identical(col_1, col_2))
      } else {
        return(isTRUE(dplyr::all_equal(col_1, col_2, ignore_row_order = FALSE)))
      }
    }
  ) %>% unlist()

  # Convert column combinations to tibble
  column_combinations <- dplyr::as_tibble(column_combinations)

  # Extract V1 and V2 where 'identical' is TRUE
  identicals <- column_combinations[
    column_combinations[["identical"]],
    c("V1", "V2")
  ]

  if (isTRUE(return_all_comparisons)) {
    return(list(identicals, column_combinations))
  }

  identicals
}

# Find identical columns (based on values)
# Remove all but one of these identical columns
# If return_all_comparisons is TRUE, return list with 1. data, 2. all comparisons
# If group_wise: 1,1,2,2 == 2,2,1,1 (identical groups with different names)
remove_identical_cols <- function(data, cols = NULL, exclude_comparisons = NULL,
                                  return_all_comparisons = FALSE,
                                  group_wise = FALSE, parallel = FALSE) {
  if (is.null(cols)) {
    cols <- colnames(data)
  }

  # Convert to tibble
  data <- dplyr::as_tibble(data)

  # Find identicals
  identicals_and_comparisons <- find_identical_cols(
    data,
    cols,
    exclude_comparisons = exclude_comparisons,
    return_all_comparisons = TRUE,
    group_wise = group_wise,
    parallel = parallel
  )

  identicals <- identicals_and_comparisons[[1]]
  comparisons <- identicals_and_comparisons[[2]]

  # Find the columns to remove
  to_remove <- unique(identicals[[2]])

  # Remove
  if (is.character(to_remove)) {
    data <- base_deselect(data, cols = to_remove)
  } else if (is.integer(to_remove)) {
    data <- dplyr::select(data, -to_remove)
  }

  if (isTRUE(return_all_comparisons)) {
    return(list(
      "updated_data" = data,
      "comparisons" = comparisons,
      "removed_cols" = to_remove
    ))
  }

  data
}


rename_with_consecutive_numbering <- function(data, cols, base_name) {
  if (is.integer(cols)) {
    cols <- colnames(data)[cols]
  }

  num_names_to_create <- length(cols)
  new_names <- paste0(base_name, seq_len(num_names_to_create))

  dplyr::rename_at(data,
                   dplyr::vars(cols),
                   ~new_names)
}

# Add underscore until var name is unique
# arg disallowed can add extra things not to be named as
create_tmp_var <- function(data, tmp_var = ".tmp_index_", disallowed = NULL) {

  # Extract the disallowed names
  disallowed <- c(colnames(data), disallowed)

  while (tmp_var %in% disallowed) {
    tmp_var <- paste0(tmp_var, "_")
  }
  tmp_var
}

# Add underscore until value is unique in the vector
# arg disallowed can add extra things not to be named as
create_tmp_val <- function(v, tmp_val = ".tmp_val_", disallowed = NULL) {

  # Extract the disallowed names
  disallowed <- c(unique(v), disallowed)

  while (tmp_val %in% disallowed) {
    tmp_val <- paste0(tmp_val, "_")
  }
  tmp_val
}

# Used in create_num_col_groups
rename_levels_by_reverse_rank_summary <- function(data, rank_summary, levels_col, num_col) {
  current_rank_summary <- create_rank_summary(data, levels_col, num_col)

  reverse_rank_bind <- rank_summary %>%
    dplyr::arrange(dplyr::desc(.data$sum_)) %>%
    dplyr::bind_cols(current_rank_summary)

  pattern_and_replacement <- reverse_rank_bind %>%
    base_select(cols = c(levels_col, paste0(levels_col, "1")))

  data <- data %>%
    dplyr::left_join(pattern_and_replacement, by = levels_col) %>%
    base_deselect(cols = levels_col) %>%
    base_rename(before = paste0(levels_col, "1"), after = levels_col)

  updated_rank_summary <- reverse_rank_bind %>%
    dplyr::mutate(sum_ = .data$sum_ + .data$sum_1) %>%
    base_select(cols = c(levels_col, "sum_"))

  list(
    "updated_rank_summary" = updated_rank_summary,
    "updated_data" = data
  )
}

create_rank_summary <- function(data, levels_col, num_col) {
  data %>%
    dplyr::group_by(!!as.name(levels_col)) %>%
    dplyr::summarize(sum_ = sum(!!as.name(num_col))) %>%
    dplyr::arrange(.data$sum_)
}


# Extracts the major and minor version numbers.
check_R_version <- function() {
  major <- as.integer(R.Version()$major)
  minor <- as.numeric(strsplit(R.Version()$minor, ".", fixed = TRUE)[[1]][[1]])
  list("major" = major, "minor" = minor)
}

# Skips testthat test, if the R version is below 3.6.0
# WHY? Due to the change in the random sampling generator
# tests fail on R versions below 3.6.0.
# It is possible to fix this by using the old generator for
# unit tests, but that would take a long time to convert,
# and most likely the code works the same on v3.5
skip_test_if_old_R_version <- function(min_R_version = "3.6") {
  if (check_R_version()[["minor"]] < strsplit(min_R_version, ".", fixed = TRUE)[[1]][[2]]) {
    testthat::skip(message = paste0("Skipping test as R version is < ", min_R_version, "."))
  }
}

base_rename <- function(data, before, after, warn_at_overwrite = FALSE) {

  #
  # Replaces name of column in data frame
  #

  # Check names
  if (!is.character(before) || !is.character(after)) {
    stop("'before' and 'after' must both be of type character.")
  }
  if (length(before) != 1 || length(before) != 1) {
    stop("'before' and 'after' must both have length 1.")
  }

  if (before == after) {
    message("'before' and 'after' were identical.")
    return(data)
  }
  # If after is already a column in data
  # remove it, so we don't have duplicate column names
  if (after %in% colnames(data)) {
    if (isTRUE(warn_at_overwrite)) {
      warning("'after' already existed in 'data' and will be replaced.")
    }
    data[[after]] <- NULL
  }
  colnames(data)[names(data) == before] <- after
  return(data)
}

# Cols should be col names
base_select <- function(data, cols) {
  subset(data, select = cols)
}

# Cols should be col names
base_deselect <- function(data, cols) {
  if (!is.character(cols)) stop("cols must be names")
  base_select(data = data, cols = setdiff(names(data), cols))
}

# Col should be col name
position_first <- function(data, col) {
  if (is.numeric(col)) stop("col must be name")
  # if(is.data.table(data)){
  #   return(data[, c(col, setdiff(names(data), col)), with = FALSE])
  # }

  base_select(data = data, cols = c(col, setdiff(names(data), col)))
}
