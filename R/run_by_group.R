

# Apply function to groups
run_by_group_df <- function(data, .fn, ...){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_function(.fn, add = assert_collection)
  checkmate::reportAssertions(assert_collection)

  # If not a grouped data frame, just call the function directly
  if (!dplyr::is_grouped_df(data)){
    return(.fn(data, ...))
  }

  checkmate::assert_data_frame(data, min.rows = 1, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  purrr::map_dfr(.x = split(x = dplyr::ungroup(data),
                            f = dplyr::group_indices(data)),
                 .f = .fn, ...)
}

# Apply function to groups
run_by_group_list <- function(data, .fn, ...){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_function(.fn, add = assert_collection)
  checkmate::reportAssertions(assert_collection)

  # If not a grouped data frame, just call the function directly
  if (!dplyr::is_grouped_df(data)){
    return(.fn(data, ...))
  }

  checkmate::assert_data_frame(data, min.rows = 1, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  purrr::map(.x = split(x = dplyr::ungroup(data),
                        f = dplyr::group_indices(data)),
                 .f = .fn, ...)
}


run_by_group_col <- function(data, .fn, .col_name, ...){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_function(.fn, add = assert_collection)
  checkmate::reportAssertions(assert_collection)

  # If not a grouped data frame, just call the function directly
  if (!dplyr::is_grouped_df(data)){
    return(.fn(data, ...))
  }

  checkmate::assert_data_frame(data, min.rows = 1, add = assert_collection)
  checkmate::assert_string(.col_name, min.chars = 1, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Add index to later restore order
  tmp_ind_col <- create_tmp_var(data = data)
  data[[tmp_ind_col]] <- seq_len(nrow(data))

  # Save groups
  groups <- data[, dplyr::group_vars(data), drop = FALSE] %>%
    dplyr::ungroup()

  fn_df <- function(.x, ...){
    tibble::tibble("tmp" = .fn(.x, ...),
                   "index" = .x[[tmp_ind_col]])
  }

  # Is likely a list
  out <- purrr::map_dfr(.x = split(x = dplyr::ungroup(data),
                    f = dplyr::group_indices(data)),
                    .f = fn_df, ...) %>%
    dplyr::arrange(.data$index) %>%
    dplyr::select(-"index")

  colnames(out) <- .col_name

  dplyr::bind_cols(groups, out)
}

