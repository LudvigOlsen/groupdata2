
# Scales each selected column
# weights them
# and combines them to a single column
# This column is added to `data`
# @param handle_no_cols Either `'stop'` or `'1'`.
# @param na.rm Whether to ignore NAs in rowSum.
scale_and_combine_ <- function(data,
                               weights = NULL,
                               include_cols = NULL,
                               exclude_cols = NULL,
                               scale_fn = standardize_,
                               col_name = "combined",
                               handle_no_cols = "stop",
                               na.rm = TRUE) {

  if (!is.null(exclude_cols)) {
    if (!is.null(include_cols)) {
      stop("Either supply `include_cols` or `exclude_cols`, not both.")
    }
    include_cols <- colnames(data)[colnames(data) %ni% exclude_cols]
  }
  if (is.null(include_cols)){
    stop("Either `include_cols` or `exclude_cols` must be supplied.")
  }

  if (length(include_cols) == 0) {
    if (handle_no_cols == "stop") {
      stop("Found 0 columns to scale and combine.")
    } else if (handle_no_cols == "1") {
      data[[col_name]] <- 1
      return(data)
    } else {
      stop(paste0(
        "`",
        handle_no_cols,
        "` is not an acceptable `handle_no_cols` value."
      ))
    }
  }

  # Check we have the columns in `data`
  checkmate::assert_names(names(data), must.include = include_cols)

  if (!is.null(weights)) {
    checkmate::assert_names(names(weights), permutation.of = include_cols)
  } else {
    # Create weights if not specified
    weights <- rep(1, times = length(include_cols)) %>%
      setNames(nm = include_cols)
  }

  # Order weights by their names
  weights <- weights[order(names(weights))]

  # Scale weights to sum to 1
  weights <- weights / sum(weights)

  # Scale each column
  scaled_cols <- data %>%
    # Select the cat_levels columns *in the same order as weights*
    base_select(cols = names(weights)) %>%
    # Scale all selected columns
    dplyr::mutate(dplyr::across(dplyr::everything(), scale_fn))

  # Multiply by weight
  scaled_cols <- sweep(
    x = scaled_cols,
    MARGIN = 2,
    STATS = weights,
    FUN = "*"
  ) %>%
    dplyr::as_tibble()

  # Combine with row sums
  data[[col_name]] <- rowSums(scaled_cols, na.rm = na.rm)

  data

}
