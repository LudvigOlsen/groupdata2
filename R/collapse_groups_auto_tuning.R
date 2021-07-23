
auto_tune_collapsings <- function(
  data,
  summaries,
  n,
  tmp_old_group_var,
  num_cols,
  cat_col,
  id_col,
  balance_size,
  weights,
  scale_fn,
  extreme_pairing_levels,
  num_new_group_cols,
  unique_new_group_cols_only,
  max_iters,
  col_name,
  parallel) {


  ### . . . . . . . . .. #< dbf756c960b3ef36d283c12e6a57cdbf ># . . . . . . . . ..
  ### Create all combinations of dimensions                                   ####


  # Create all combinations of the included balancing dimensions
  include_flags <-  c(
    "size" = isTRUE(balance_size),
    "cat" = !is.null(cat_col),
    "num" = !is.null(num_cols),
    "id" = !is.null(id_col)
  )

  # Get names of the TRUEs
  included_flag_names <- names(include_flags)[include_flags]

  # Make all relevant combinations
  combinations <- purrr::map(.x = seq_along(included_flag_names),
                             .f = ~ {
                               combn(included_flag_names, m = .x, simplify = F)
                             }) %>%
    unlist(recursive = F) %>%
    purrr::map(.f = ~ {
      include_vec <- rep(FALSE, times = length(include_flags)) %>%
        setNames(names(include_flags))
      include_vec[names(include_vec) %in% .x] <- TRUE
      include_vec
    }) %>%
    setNames(paste0(".atc_comb_", seq_len(length(.))))

  # We need to know the name of the main combination
  # So we can produce many columns with those and fewer with the other combos
  # to avoid exploding the number of column comparisons (in deduplication)
  main_combination <- purrr::map(.x = combinations, .f = ~{
    all(.x == include_flags)
  }) %>% unlist()
  main_combination <- names(main_combination)[main_combination]

  ### . . . . . . . . .. #< 40bb882cfc30cd1a93c9e99f28a9556e ># . . . . . . . . ..
  ### Combine and fold                                                        ####

  # Find number of group columns to generate
  # Minimum 10 should be attempted
  non_main_num_group_cols_to_check <- 5 # TODO Specifiable?
  if (num_new_group_cols < 5) {
    main_num_group_cols_to_check <- 10
  } else if (num_new_group_cols < 20) {
    main_num_group_cols_to_check <- num_new_group_cols * 2
  } else if (num_new_group_cols < 50) {
    main_num_group_cols_to_check <- ceiling(num_new_group_cols * 1.5)
  } else{
    main_num_group_cols_to_check <- num_new_group_cols + 15
  }

  # Ensure summaries are ordered by group column
  summaries <- summaries %>%
    dplyr::arrange(!!as.name(tmp_old_group_var))

  # Combine the balancing dimensions for each include combination
  # And create new group columns
  new_group_cols <- purrr::map2_dfc(.x = combinations, .y = names(combinations), .f = ~{
    num_new_group_cols <- ifelse(.y == main_combination, main_num_group_cols_to_check, non_main_num_group_cols_to_check)
    combine_and_fold_combination_(
      data = data,
      summaries = summaries,
      n = n,
      tmp_old_group_var = tmp_old_group_var,
      include_flags = .x,
      col_name = .y,
      weights = weights,
      scale_fn = scale_fn,
      extreme_pairing_levels = extreme_pairing_levels,
      num_new_group_cols = num_new_group_cols,
      unique_new_group_cols_only = unique_new_group_cols_only,
      max_iters = max_iters,
      parallel = parallel)
  }) %>%
    dplyr::mutate(!!tmp_old_group_var := summaries[[tmp_old_group_var]])

  # Names of the new group columns
  group_cols_names <- colnames(new_group_cols)[colnames(new_group_cols) != tmp_old_group_var]

  if (isTRUE(unique_new_group_cols_only)){

    # Confirmed unique combinations
    completed_comparisons <- as.data.frame(t(combn(group_cols_names, 2)),
                                         stringsAsFactors = FALSE) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        V1_base = gsub("\\d*$", "", .data$V1),
        V2_base = gsub("\\d*$", "", .data$V2)
      ) %>%
      dplyr::filter(.data$V1_base == .data$V2_base) %>%
      dplyr::select(-.data$V1_base, -.data$V2_base)

    # Remove duplicate group columns!
    new_group_cols <- new_group_cols %>%
      remove_identical_cols(
        cols = group_cols_names,
        exclude_comparisons = completed_comparisons,
        return_all_comparisons = FALSE,
        group_wise = TRUE,
        parallel = parallel
      )
  }

  # Add new group columns to data
  data <- data %>%
    dplyr::left_join(new_group_cols, by = tmp_old_group_var)

  # If <= `num_new_group_cols` after deduplication,
  # we don't need the expensive summarization steps below
  if (ncol(new_group_cols) <= num_new_group_cols) {
    return(dplyr::rename_with(
      .data = data,
      .fn = ~ paste0(col_name, "_", seq_len(ncol(new_group_cols))),
      .cols = dplyr::one_of(colnames(new_group_cols))
    ))
  }

  ranking_weights <- purrr::map(.x = names(weights), .f = ~{
    if (.x == "num" && !is.null(num_cols)){
      rep(weights[[.x]], times = length(num_cols)) %>%
        setNames(num_cols)
    } else if (.x == "cat" && !is.null(cat_col)){
      weights[[.x]] %>%
        setNames(cat_col)
    } else if (.x == "id" && !is.null(id_col)){
      weights[[.x]] %>%
        setNames(id_col)
    } else if (.x == "size" && isTRUE(balance_size)){
      weights[[.x]] %>%
        setNames("size")
    }
  }) %>%
    unlist(recursive = FALSE, use.names = TRUE)

  # Summarize the balances
  balance_summary <- summarize_balances(
    data = data,
    group_cols = group_cols_names,
    cat_cols = cat_col,
    num_cols = num_cols,
    id_cols = id_col,
    summarize_size = balance_size,
    ranking_weights = ranking_weights,
    include_normalized = TRUE
  )

  # Find the group column rankings
  # We average the rankings of the summary and the normalized summary
  # As they have been shown to sometimes differ
  # TODO Add weights to ranking averaging?!
  summary_ranks <- balance_summary[["Summary"]] %>%
    ranked_balances() %>%
    dplyr::select(.data$group_col, .data$SD_rank)
  normalized_summary_ranks <- balance_summary[["Normalized Summary"]] %>%
    ranked_balances() %>%
    dplyr::select(.data$group_col, .data$SD_rank) %>%
    dplyr::rename(norm_SD_rank = .data$SD_rank)
  summary_ranks <- summary_ranks %>%
    dplyr::left_join(normalized_summary_ranks, by = "group_col") %>%
    dplyr::mutate(avg_rank = (.data$SD_rank + .data$norm_SD_rank) / 2) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$avg_rank) %>%
    head(num_new_group_cols)

  # Find group columns to remove
  group_cols_to_keep <- as.character(summary_ranks[["group_col"]])
  group_cols_to_remove <- setdiff(group_cols_names, group_cols_to_keep)

  new_order_ <- c(
    colnames(data)[colnames(data) %ni% c(group_cols_names, tmp_old_group_var)],
    group_cols_to_keep
  )

  # Select the final columns
  # Order columns so group cols are last and in order of rank
  data <- data %>%
    base_select(cols = new_order_)

  # Rename to `col_name` + _1, _2 ...
  colnames(data)[colnames(data) %in% group_cols_to_keep] <-
    paste0(col_name, "_", seq_len(length(group_cols_to_keep)))

  data
}

combine_and_fold_combination_ <- function(
  data,
  summaries,
  tmp_old_group_var,
  n,
  include_flags,
  col_name,
  weights,
  scale_fn,
  extreme_pairing_levels,
  num_new_group_cols,
  unique_new_group_cols_only,
  max_iters,
  parallel) {

  # Scale, weight and combine
  summaries <- combine_scaled_cols_(
    summaries = summaries,
    weights = weights,
    include_flags = include_flags,
    scale_fn = scale_fn
  )

  # Fold the summary
  new_groups <- summaries %>%
    fold(
      k = n,
      num_col = "combined",
      extreme_pairing_levels = extreme_pairing_levels,
      num_fold_cols = num_new_group_cols,
      unique_fold_cols_only =  unique_new_group_cols_only,
      max_iters = max_iters,
      parallel = parallel
    ) %>%
    # Safety check - should already be arrange by this
    dplyr::arrange(!!as.name(tmp_old_group_var))

  # Replace .folds with the col_name
  # By doing it this way, it works with multiple fold columns
  colnames(new_groups) <- gsub(pattern = ".folds",
                               replacement = col_name,
                               x = colnames(new_groups))

  # Select new group columns
  new_col_names <- setdiff(colnames(new_groups), colnames(summaries))
  new_groups %>%
    base_select(cols = new_col_names)

}


