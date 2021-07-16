
# Make an actual function with all sorts of features for comparing
# and inspecting the created groups
# This could be used for other group functions as well?
# Should be advertised next to summarize_group_cols (only has row counts)

# summarize_group_balances(df_collapsed, group_col = ".coll_groups",
#                          cat_col="diagnosis", num_col="age")

# TODO should work when cat_col or num_col are NULL
summarize_group_balances <- function(data, group_cols, cat_col = NULL, num_col = NULL){

  summarize_group_balances_single_ <- function(data, group_col, cat_col, num_col){
    summaries <- dplyr::tibble(
      !!group_col := unique(data[[group_col]])
    )

    size_summary <- data %>%
      dplyr::group_by(!!as.name(group_col)) %>%
      dplyr::summarise(size = dplyr::n())

    summaries <- summaries %>%
      dplyr::left_join(size_summary, by = group_col)

    if (!is.null(num_col)){
      numeric_summaries <- data %>%
        dplyr::group_by(!!as.name(group_col)) %>%
        dplyr::summarise(avg_num = mean(!!as.name(num_col)),
                         sum_num = sum(!!as.name(num_col)))
      summaries <- summaries %>%
        dplyr::left_join(numeric_summaries, by = group_col)
    }

    if (!is.null(cat_col)){
      cat_summary <- data %>%
        dplyr::count(!!!rlang::syms(c(group_col, cat_col))) %>%
        tidyr::spread(key = !!as.name(cat_col),
                      value = .data$n,
                      fill = 0)
      summaries <- summaries %>%
        dplyr::left_join(cat_summary, by = group_col)
    }

    summaries %>%
      dplyr::rename(group = !!as.name(group_col))
  }

  purrr::map_df(.x = group_cols, .f = ~ {
    summarize_group_balances_single_(
      data = data,
      group_col = .x,
      cat_col = cat_col,
      num_col = num_col
    ) %>% dplyr::mutate(group_col = .x)
  }) %>%
    dplyr::group_by(.data$group_col) %>%
    # Ensure right types
    dplyr::mutate(
      group = factor(.data$group),
      group_col = factor(.data$group_col)
    ) %>%
    position_first(col = "group_col")
}

score_group_balances <- function(data, group_cols, by_size = TRUE, cat_col=NULL, num_col=NULL){
                                 #score_exponent = 2){

  # Create summary
  summary <-
    summarize_group_balances(
      data = data,
      group_cols = group_cols,
      cat_col = cat_col,
      num_col = num_col
    ) %>%
    dplyr::ungroup()

  # Prepare normalized summary
  normalized_summary <- summary %>%
    dplyr::select(.data$group_col, .data$size, !dplyr::contains("_num"))

  # Create summary of the normalized (MinMax scaled) num_col

  if (!is.null(num_col)){

    # Normalize numeric column first
    data <- data %>%
      dplyr::mutate(!!num_col := rearrr::min_max_scale(
        !!as.name(num_col), new_min = 0, new_max = 1))

    # Summarise normalized num_col
    normalized_num_summary <-
      summarize_group_balances(data = data,
                               group_cols = group_cols,
                               num_col = num_col) %>%
      dplyr::ungroup() %>%
      dplyr::rename(avg_norm_num = .data$avg_num,
                    sum_norm_num = .data$sum_num) %>%
      dplyr::select(.data$avg_norm_num, .data$sum_norm_num)

    # Add normalized summaries
    normalized_summary <- normalized_summary %>%
      dplyr::bind_cols(normalized_num_summary)
  }

  # Find columns with cat_col levels
  cat_columns <- setdiff(
    colnames(summary),
    c("group", "group_col", "avg_num", "sum_num",
      "avg_norm_num", "sum_norm_num", "size")
  )

  # Calculate standard deviations for all numeric columns
  summary <- summary %>%
    dplyr::group_by(.data$group_col) %>%
    dplyr::summarize(dplyr::across(where(is.numeric), sd))

  # Apply log10 to counts and find standard deviation of all numeric columns
  normalized_summary <- normalized_summary %>%
    dplyr::mutate(dplyr::across(dplyr::one_of(c("size", cat_columns)), function(x) {
      # In case of zero-frequencies
      log10(1 + x)
    })) %>%
    dplyr::group_by(.data$group_col) %>%
    dplyr::summarize(dplyr::across(where(is.numeric), sd))

  # Move cat columns last
  normalized_summary <- normalized_summary %>%
    dplyr::select(!dplyr::one_of(cat_columns), dplyr::one_of(cat_columns))

  # Fix names
  colnames(normalized_summary)[
    colnames(normalized_summary) %in% c("size", cat_columns)] <-
    paste0("log_", colnames(normalized_summary)[
      colnames(normalized_summary) %in% c("size", cat_columns)])

  colnames(normalized_summary)[
    colnames(normalized_summary) %ni% c("group_col", "score")] <-
    paste0("sd_", colnames(normalized_summary)[
      colnames(normalized_summary) %ni% c("group_col", "score")])

  colnames(summary)[colnames(summary) %ni% c("group_col")] <-
    paste0("sd_", colnames(summary)[colnames(summary) %ni% c("group_col")])

  if (!is.null(num_col)){
    colnames(summary) <- gsub("num", tolower(num_col), colnames(summary))
    colnames(normalized_summary) <- gsub("num", tolower(num_col), colnames(normalized_summary))
  }


  list("Normalized Summary" = normalized_summary,
       "Summary" = summary)
}

# Calculate score

# A meaningful single score is seemingly hard to create
# so it's probably better to leave it out
# score <- 0
#
# score_weights <- c("size" = 2, "num" = 1, "cat" = 1)
# score_weights <- score_weights / sum(score_weights)
#
# if (isTRUE(by_size)){
#   score <- score + (normalized_summary[["size"]] ^ score_exponent) * score_weights[["size"]]
# } else {
#   normalized_summary[["size"]] <- NULL
#   summary[["size"]] <- NULL
# }
#
# if (!is.null(cat_col)){
#   cat_score <- base_select(normalized_summary, cols = cat_columns) %>%
#     unlist(recursive = TRUE) %>%
#     mean()
#   score <- score + (cat_score ^ score_exponent) * score_weights[["cat"]]
# }
#
# if (!is.null(num_col)){
#   num_score <-
#     mean(c(normalized_summary[["avg_norm_num"]] ^ score_exponent,
#          (normalized_summary[["sum_norm_num"]] / 4) ^ score_exponent))
#   score <- score + num_score * score_weights[["num"]]
# }
#
# # Root the part-wise exponentiated score
# normalized_summary["score"] <- nth_root(score, root = score_exponent)
#
# best_by_score <- normalized_summary %>%
#   dplyr::slice(which.min(.data$score)) %>%
#   dplyr::pull(.data$group_col) %>%
#   as.character()
