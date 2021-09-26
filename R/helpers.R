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

nth_root <- function(x, root) {
  (abs(x) ^ (1 / root)) * sign(x)
}

is_between_ <- function(x, a, b) {

  # Checks if x is between a and b
  x > a & x < b
}

`%ni%` <- function(x, table) {
  !(x %in% table)
}

# Get all lists in a list with a certain name
# Use: list_of_lists %c% 'list_name'
# From http://stackoverflow.com/questions/5935673/accessing-same-named-list-elements-of-the-list-of-lists-in-r/5936077#5936077
`%c%` <- function(x, n) {
  lapply(x, `[[`, n)
}


isEmpty_ <- function(x) {
  length(x) == 0
}

# Center and scale x
standardize_ <- function(x){
  std <- sd(x)
  if (std == 0) std <- 1
  (x - mean(x)) / std
}

convert_n <- function(data, n, method, allow_zero) {

  if (method %ni% c("l_starts", "l_sizes")) {

    # Sanity check
    if (!checkmate::test_number(x = n)){
      stop(paste0("when 'method' is '", method, "', 'n' must be numeric scalar."))
    }

    ### Convert from percentage

    # We check if n is given as percentage
    # This would be done by giving a number between
    # 0 and 1
    # If it is, we convert it to the actual number
    # of windows

    if (is_between_(n, 0, 1)) {
      n <- convert_percentage_(n, data)
    }

    # Sanity check
    checkmate::assert_count(x = n,
                            positive = !allow_zero,
                            .var.name = "n converted to whole number")
    if (is.data.frame(data)) {
      checkmate::assert_true(nrow(data) >= n)
    } else {
      checkmate::assert_true(length(data) >= n)
    }
  }

  n
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

group_uniques_randomly_ <- function(
  data,
  n,
  id_col,
  method,
  starts_col = NULL,
  col_name = ".groups",
  force_equal = FALSE,
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
  # TODO replace with dplyr join (didn't seem to work)
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

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_count(x = n, add = assert_collection)
  checkmate::assert_count(x = start_at, positive = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (!numbers::isPrime(start_at)) {
    assert_collection$push("'start_at' is not a prime number.")
  }
  if (n <= 1){
    assert_collection$push("'n' must be larger than 1.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

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
  unlisted <- nested_list %>% unlist(use.names = FALSE)
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
                       "ing row names as 'starts_col' instead."))
      }

      # Get the row names of data to use as starts_col
      starts_col <- rownames(data)

      # If starts_col is not NULL (and not 'index')
      # Check that the column exists in data
      # and get the column from data
    } else {
      # Checks made in parent function
      starts_col <- data[[starts_col]]
    }
  }

  if (is.factor(starts_col)){
    warning("'data[[starts_col]]' is factor. Converting to character.")
    starts_col <- as.character(starts_col)
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


  tryCatch({

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
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, min.cols = 2, add = assert_collection)
  checkmate::assert_data_frame(x = exclude_comparisons, null.ok = TRUE, add = assert_collection)
  checkmate::assert_flag(x = return_all_comparisons, add = assert_collection)
  checkmate::assert_flag(x = group_wise , add = assert_collection)
  checkmate::assert_flag(x = parallel, add = assert_collection)
  checkmate::assert(
    checkmate::check_character(x = cols, min.len = 2, any.missing = FALSE, null.ok = TRUE),
    checkmate::check_integerish(x = cols, min.len = 2, lower = 1, any.missing = FALSE, null.ok = TRUE),
    .var.name = "cols"
  )
  checkmate::reportAssertions(assert_collection)
  if (!is.null(exclude_comparisons))
    checkmate::assert_names(x = colnames(exclude_comparisons),
                            must.include = c("V1", "V2"),
                            add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (is.null(cols)) {
    cols <- colnames(data)
  }

  column_combinations <- as.data.frame(t(combn(cols, 2)),
                                       stringsAsFactors = FALSE)

  # Exclude comparisons if specified
  if (!is.null(exclude_comparisons)) {
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
        return(all(as.character(col_1) == as.character(col_2)))
      }
    }
  ) %>% unlist()

  # Convert column combinations to tibble
  column_combinations <- dplyr::as_tibble(column_combinations)

  # Extract V1 and V2 where 'identical' is TRUE
  identicals <- column_combinations[
    column_combinations[["identical"]],
    c("V1", "V2")
  ] %>% dplyr::as_tibble()

  if (isTRUE(return_all_comparisons)) {
    return(list(identicals, column_combinations))
  }

  identicals
}

# Find identical columns (based on values)
# Remove all but one of these identical columns
# If return_all_comparisons is TRUE, return list with 1. data, 2. all comparisons
# If group_wise: 1,1,2,2 == 2,2,1,1 (identical groups with different names)
# When keep_cols is a character vector, those columns will not be removed
remove_identical_cols <- function(data, cols = NULL, exclude_comparisons = NULL,
                                  return_all_comparisons = FALSE, keep_cols = NULL,
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
  if (!is.null(keep_cols)){
    # In this case, we might know which of the two to keep
    identicals <- identicals %>%
      dplyr::mutate(remove = dplyr::case_when(
        V1 %in% keep_cols & V2 %in% keep_cols ~ ".__NA__",
        V2 %in% keep_cols ~ V1,
        TRUE ~ V2
      ))
    to_remove <- unique(identicals[["remove"]])
    to_remove <- to_remove[to_remove != ".__NA__"]
  } else {
    to_remove <- unique(identicals[[2]])
  }

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


rename_with_consecutive_numbering <- function(data, cols, base_name, warn_at_rename=FALSE, warning_msg=NULL) {

  if (isTRUE(warn_at_rename) && is.null(warning_msg))
    stop("please supply `warning_msg` when `warn_at_rename` is enabled.")

  if (is.integer(cols)) {
    cols <- colnames(data)[cols]
  }

  num_names_to_create <- length(cols)
  new_names <- paste0(base_name, seq_len(num_names_to_create))

  if (isTRUE(warn_at_rename) && !all(cols == new_names)){
    warning(warning_msg)
  }

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

# Rename groups to the names in the given rank summary
# The largest group becomes part of the smallest group in the given rank summary
# Used in create_num_col_groups and numerically_balanced_group_factor_
rename_levels_by_reverse_rank_summary <- function(data, rank_summary, levels_col, num_col) {
  # Calculate current rank summary
  current_rank_summary <- create_rank_summary(data, levels_col, num_col)
  colnames(current_rank_summary) <- paste0(colnames(current_rank_summary), "_current")

  # Reverse the given rank summary
  # and combine with rank summary for the given data
  reverse_rank_bind <- rank_summary %>%
    dplyr::arrange(dplyr::desc(.data$sum_)) %>%
    dplyr::bind_cols(current_rank_summary)

  # Find mapping for groups in the two rank summaries
  pattern_and_replacement <- reverse_rank_bind %>%
    base_select(cols = c(levels_col, paste0(levels_col, "_current")))

  # Add the mapping to the given data
  data <- data %>%
    dplyr::left_join(pattern_and_replacement, by = levels_col) %>%
    base_deselect(cols = levels_col) %>%
    base_rename(before = paste0(levels_col, "_current"), after = levels_col)

  # Update the given rank summary with the sums in the rank summary for the given data
  updated_rank_summary <- reverse_rank_bind %>%
    dplyr::mutate(sum_ = .data$sum_ + .data$sum__current) %>%
    base_select(cols = c(levels_col, "sum_"))

  # Return updated rank summary and the regrouped data
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
  data
}

# Cols should be col names
base_select <- function(data, cols) {
  tryCatch(subset(data, select = cols), error = function(e){
    if (grepl("is missing", e)){
      stop("base_select() only work on data frames.")
    } else {
      stop(paste0("base_select() got error from subset(): ", e))
    }
  })
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

# insertRow2 from https://stackoverflow.com/a/11587051/11832955
# Note: May not work with rownames!
insert_row <- function(data, new_row, after) {
  data <- rbind(data, new_row)
  data <- data[order(c(seq_len(nrow(data) - 1), after + 0.5)),
               , drop = FALSE] # extra comma on purpose
  row.names(data) <- NULL
  data
}

# Warn user once per session
warn_once <- function(msg,
                     id = msg,
                     sys.parent.n = 0L) {
  stopifnot(rlang::is_string(id))
  # If we already threw the warning, ignore
  if (rlang::env_has(warning_env, id)) {
    return(invisible(NULL))
  }
  # Register the ID
  warning_env[[id]] <- TRUE
  # Throw warning
  msg <- paste0(msg,
                "\nNOTE: This message is displayed once per session.")
  warning(simpleWarning(msg,
                        call = if (p <- sys.parent(sys.parent.n + 1))
                          sys.call(p)))
}
# Create warning environment
warning_env <- rlang::env()

# message user once per session
message_once <- function(msg,
                     id = msg,
                     sys.parent.n = 0L) {
  stopifnot(rlang::is_string(id))
  # If we already threw the message, ignore
  if (rlang::env_has(message_env, id)) {
    return(invisible(NULL))
  }
  # Register the ID
  message_env[[id]] <- TRUE
  # Throw warning
  msg <- paste0(msg,
                "\nNOTE: This message is displayed once per session.")
  message(simpleMessage(msg,
                        call = if (p <- sys.parent(sys.parent.n + 1))
                          sys.call(p)))
}
# Create warning environment
message_env <- rlang::env()

message_once_about_group_by <- function(fn_name, sys.parent.n = 1L) {
  fn_name <- paste0("'", fn_name, "()'")
  message_once(
    paste0(
      fn_name,
      " now detects grouped data.frames and is applied group-wise (since v1.3.0). ",
      "If this is unwanted, use 'dplyr::ungroup()' before ",
      fn_name,
      "."
    ),
    sys.parent.n = sys.parent.n
  )
}

get_pkg_version <- function(pkg_name){
  vs <- unlist(utils::packageVersion(pkg_name))
  list("major" = vs[[1]],
       "minor" = vs[[2]],
       "patch" = vs[[3]],
       "dev" = ifelse(length(vs) > 3, vs[[4]], integer(0)))
}

is_checkmate_v2_1 <- function(){
  v <- get_pkg_version("checkmate")
  v$major == 2 && v$minor >= 1
}
