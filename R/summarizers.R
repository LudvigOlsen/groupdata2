

#   __________________ #< 60cfc78f594e5611a6eaaf34a2b212ae ># __________________
#   Summarization functions                                                 ####


##  .................. #< eb074f5b5a560bb19ff907907f958da2 ># ..................
##  Empty summaries                                                         ####


# Empty summary
create_empty_summary_ <- function(data, group_col){

  checkmate::assert_string(group_col, min.chars = 1)

  data %>%
    dplyr::ungroup() %>%
    dplyr::select(!!as.name(group_col)) %>%
    dplyr::group_by(!!as.name(group_col)) %>%
    dplyr::group_keys()
}


##  .................. #< 79a1a50b2f6fd2bc4a62237a6399b27c ># ..................
##  Size summaries                                                          ####


# Size summary
create_size_summary_ <- function(data, group_col, name="size"){

  checkmate::assert_string(group_col, min.chars = 1)
  checkmate::assert_string(name, min.chars = 1)

  data %>%
    dplyr::ungroup() %>%
    dplyr::select(!!as.name(group_col)) %>%
    dplyr::group_by(!!as.name(group_col)) %>%
    dplyr::summarise(!!name := dplyr::n(),
                     .groups = "drop")
}


##  .................. #< cad874f32d32a8eae09090d2894d7ad5 ># ..................
##  ID column summaries                                                     ####


# Number of IDs/levels summary
create_id_summaries_ <- function(data, group_col, id_cols, name_prefix="# "){

  checkmate::assert_string(group_col, min.chars = 1)
  checkmate::assert_string(name_prefix, null.ok = TRUE)
  checkmate::assert_character(
    id_cols,
    min.chars = 1,
    any.missing = FALSE,
    min.len = 1
  )

  summary <- data %>%
    dplyr::ungroup() %>%
    dplyr::select(!!!rlang::syms(c(group_col, id_cols))) %>%
    dplyr::group_by(!!as.name(group_col)) %>%
    dplyr::summarise(
      dplyr::across(dplyr::one_of(id_cols), function(x){length(unique(x))}),
      .groups = "drop"
    )

  if (!is.null(name_prefix) &&
      nchar(name_prefix) > 0){
    summary <- summary %>%
      dplyr::rename_with(
      ~ paste0(name_prefix, .),
      -dplyr::one_of(group_col)
    )
  }

  summary

}


##  .................. #< abd5e8f22decefad01cca729a155076c ># ..................
##  Numerical summaries                                                     ####


# Numeric column summary
create_num_summaries_ <- function(data,
                                  group_col,
                                  num_cols,
                                  fns = list("mean" = mean, "sum" = sum),
                                  rename = FALSE) {

  checkmate::assert_string(group_col, min.chars = 1)
  checkmate::assert_character(
    num_cols,
    min.chars = 1,
    any.missing = FALSE,
    min.len = 1
  )
  checkmate::assert_flag(rename)
  checkmate::assert_list(fns, types = "function")
  if (length(fns) > 1 && is.null(names(fns))){
    stop("When `fns` has length > 1, it must be named.")
  }
  if (length(fns) == 1 && is.null(names(fns))){
    fns <- fns[[1]]
  }

  summary <- data %>%
    dplyr::ungroup() %>%
    dplyr::select(!!!rlang::syms(c(group_col, num_cols))) %>%
    dplyr::group_by(!!as.name(group_col)) %>%
    dplyr::summarise(dplyr::across(dplyr::one_of(num_cols),
                                   fns),
                     .groups = "drop")

  if (isTRUE(rename)){
    summary <- summary %>%
      # Rename so `xxx_mean <- mean(xxx)` and `xxx_sum <- sum(xxx)`
      dplyr::rename_with(
        ~ gsub(
          pattern = "(^.*)_([[:alnum:]]*)$",
          replacement = "\\2(\\1)",
          x = .
        ),
        where(is.numeric)
      )
  }

  summary

}


##  .................. #< 440b147b963f8a7fd202661bfc3b068e ># ..................
##  Categorical summaries                                                   ####


# Categorical summary
# One column per level per cat_col
create_cat_summaries_ <- function(data,
                                  group_col,
                                  cat_cols,
                                  max_cat_prefix_chars = 5,
                                  name_prefix = "# ") {

  checkmate::assert_string(group_col, min.chars = 1)
  checkmate::assert_character(
    cat_cols,
    min.chars = 1,
    any.missing = FALSE,
    min.len = 1
  )

  tmp_n_var <- create_tmp_var(data, tmp_var = ".n")

  summary <- data %>%
    dplyr::ungroup() %>%
    dplyr::select(!!!rlang::syms(c(group_col, cat_cols))) %>%
    dplyr::mutate(dplyr::across(dplyr::one_of(cat_cols), as.character)) %>%
    tidyr::gather(key = "cat_col", value = "cat_val", cat_cols) %>%
    dplyr::count(!!as.name(group_col), .data$cat_col, .data$cat_val,
                 name = tmp_n_var) %>%
    rename_cat_levels_for_summary_(
      max_cat_prefix_chars = max_cat_prefix_chars,
      name_prefix = name_prefix
    ) %>%
    dplyr::select(dplyr::one_of(group_col, "cat_name", tmp_n_var)) %>%
    tidyr::spread(key = .data$cat_name,
                  value = !!as.name(tmp_n_var),
                  fill = 0)

  summary

}


# NOTE: Must ensure the same names are produced in this
# and in create_cat_summaries_
create_cat_name_map_ <- function(data,
                                 cat_cols,
                                 max_cat_prefix_chars = 5,
                                 name_prefix = "# "){

  checkmate::assert_character(
    cat_cols,
    min.chars = 1,
    any.missing = FALSE,
    min.len = 1
  )

  data %>%
    dplyr::ungroup() %>%
    dplyr::select(!!!rlang::syms(c(cat_cols))) %>%
    dplyr::mutate(dplyr::across(dplyr::one_of(cat_cols), as.character)) %>%
    dplyr::distinct() %>%
    tidyr::gather(key = "cat_col", value = "cat_val", cat_cols) %>%
    dplyr::distinct() %>%
    dplyr::mutate(orig_cat_col = .data$cat_col) %>%
    position_first(col = "orig_cat_col") %>%
    rename_cat_levels_for_summary_(
      max_cat_prefix_chars = max_cat_prefix_chars,
      name_prefix = name_prefix
    ) %>%
    dplyr::arrange(.data$cat_col, .data$cat_val) %>%
    dplyr::select(-.data$cat_col) %>%
    split(f = .$orig_cat_col, drop = TRUE) %>%
    purrr::map(.f = ~{
      vals <- .x[["cat_name"]]
      nms <- .x[["cat_val"]]
      vals %>%
        setNames(nms)
    })

}

rename_cat_levels_for_summary_ <- function(cat_levels_map,
                                           max_cat_prefix_chars = 5,
                                           name_prefix = "# "){
  checkmate::assert_data_frame(cat_levels_map)
  checkmate::assert_count(max_cat_prefix_chars)
  checkmate::assert_string(name_prefix, null.ok = TRUE)

  cat_levels_map <- cat_levels_map %>%
    dplyr::mutate(
      cat_col = tolower(.data$cat_col),
      cat_val = tolower(.data$cat_val),
      cat_col = substr(.data$cat_col, start = 1, stop = max_cat_prefix_chars),
      cat_name = paste0(.data$cat_col, "_", .data$cat_val),
      cat_name = gsub("^_", "", .data$cat_name)
    )

  if (!is.null(name_prefix) &&
      nchar(name_prefix) > 0){
    cat_levels_map <- cat_levels_map %>%
      dplyr::mutate(cat_name = paste0(name_prefix, .data$cat_name))
  }
}

# Combined categorical summaries
# I.e. one single column per cat_col
create_combined_cat_summaries_ <- function(
  data,
  group_cols,
  cat_cols,
  cat_levels,
  warn_zero_variance = TRUE
) {

  checkmate::assert_character(
    group_cols,
    min.chars = 1,
    any.missing = FALSE,
    min.len = 1
  )
  checkmate::assert_character(
    cat_cols,
    min.chars = 1,
    any.missing = FALSE,
    min.len = 1
  )
  checkmate::assert_flag(warn_zero_variance)

  purrr::map(.x = cat_cols, .f = ~ {
    create_combined_cat_summary_(
      data = data,
      group_cols = group_cols,
      cat_col = .x,
      cat_levels = cat_levels,
      warn_zero_variance = warn_zero_variance
    )
  }) %>%
    purrr::reduce(dplyr::full_join, by = group_cols)
}

# Combined summary for single cat_col
create_combined_cat_summary_ <- function(data, group_cols, cat_col, cat_levels, warn_zero_variance=TRUE){
  if (is.list(cat_levels)){
    # When cat_col is not in cat_levels,
    # this returns NULL
    cat_levels <- cat_levels[[cat_col]]
  }

  cat_summary <- NULL
  if (!is.null(cat_col)){
    cat_summary <- data %>%
      dplyr::count(!!!rlang::syms(c(group_cols, cat_col)))

    if (is.null(cat_levels)){
      checkmate::assert_factor(data[[cat_col]])
      cat_levels <- levels(data[[cat_col]])
    }

    if (length(cat_levels) == 1 && cat_levels %in% c(".majority", ".minority")){
      slice_fn <- ifelse(cat_levels == ".majority", which.max, which.min)
      cat_levels <- data %>%
        # TODO time whether summarize with the existing counts is faster
        dplyr::count(!!as.name(cat_col)) %>%
        dplyr::slice(slice_fn(n)) %>%
        dplyr::pull(!!as.name(cat_col)) %>%
        as.character()
    }

    # Convert to always be a named numeric vector (weights)
    if (!is.numeric(cat_levels)) {
      cat_levels <- rep(1, times = length(cat_levels)) %>%
        setNames(nm = cat_levels)
    }

    # Get counts for the selected cat_levels
    cat_summary <- cat_summary %>%
      tidyr::spread(key = !!as.name(cat_col),
                    value = .data$n,
                    fill = 0) %>%
      dplyr::select(!!!rlang::syms(c(group_cols, names(cat_levels))))

    # Scale, weight and combine the `cat_levels` columns
    cat_summary <- scale_and_combine_(
      data = cat_summary,
      weights = cat_levels,
      include_cols = names(cat_levels),
      scale_fn = standardize_,
      col_name = cat_col,
      handle_no_cols = "stop"
    )  %>%
      dplyr::select(dplyr::one_of(group_cols, cat_col))

    if (isTRUE(warn_zero_variance) &&
        sd(cat_summary[[cat_col]]) == 0){
      warning(simpleWarning(
        paste0(
          "Combining the standardized level counts",
          " for the `cat_cols` column '",
          cat_col,
          "' led to a zero-variance vector. ",
          "Consider not balancing this column or change the included `cat_levels`."
        ),
        call = if (p <- sys.parent(5 + 1))
          sys.call(p)
      ))
    }
  }

  cat_summary
}


##  .................. #< 60cfc78f594e5611a6eaaf34a2b212ae ># ..................
##  Ranking utils                                                           ####


# Apply rank() to columns
rank_numeric_cols_ <- function(data, cols = NULL){

  checkmate::assert_character(
    cols,
    any.missing = FALSE,
    null.ok = TRUE,
    min.chars = 1,
    add = assert_collection
  )

  if (is.null(cols)){
    cols <- data %>%
      dplyr::select(where(is.numeric)) %>%
      colnames()
  } else {
    unknown_cols <- setdiff(cols, colnames(data))
    if (length(unknown_cols) > 0){
      stop(paste0("`cols` had unknown names: ", paste0(unknown_cols, collapse = ", ")))
    }
  }

  data %>%
    dplyr::mutate(dplyr::across(dplyr::one_of(cols), rank))
}


# Create column with weighted-average rank of numeric columns
mean_rank_numeric_cols_ <- function(
  data,
  cols = NULL,
  col_name = "mean_rank",
  rank_weights = NULL,
  already_rank_cols = character(0)) {


  if (is.null(cols)){
    cols <- data %>%
      dplyr::select(where(is.numeric)) %>%
      colnames()
  }

  if (is.null(rank_weights)){
    rank_weights <- rep(1, times = length(cols)) %>%
      setNames(nm = cols)
  }

  # Calculate average SD rank
  sd_ranks <- data %>%
    rank_numeric_cols_(cols = setdiff(cols, already_rank_cols))
  sd_ranks[[col_name]] <- sd_ranks %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(cols)) %>%
    purrr::pmap_dbl(.f = weighted_mean_, weights = rank_weights)

  sd_ranks
}


# Calculates a weighted mean of named numbers
# ... and weights must have same names and length
weighted_mean_ <- function(..., weights){
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(x = weights, lower = 0, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Order the row values and the weights
  r <- c(...)
  r <- r[order(names(r))]
  weights <- weights[order(names(weights))]
  if (!all(names(r) == names(weights))){
    stop("`...` and `weights` must have the exact same names.")
  }

  # Sum to one
  weights <- weights / sum(weights)

  sum(r * weights)
}


