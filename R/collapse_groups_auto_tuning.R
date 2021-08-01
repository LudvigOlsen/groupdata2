

#   __________________ #< 5a2b738ccf085ae82add326faadd209b ># __________________
#   Auto-tuning collapse_groups                                             ####


auto_tune_collapsings <- function(
  data,
  summaries,
  n,
  tmp_old_group_var,
  num_cols,
  cat_cols,
  id_cols,
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
  all_balance_cols <- c(cat_cols, num_cols, id_cols)
  if (isTRUE(balance_size))
    all_balance_cols <- c(all_balance_cols, "size")

  if (length(all_balance_cols) > 6) {
    warning(simpleWarning(
      paste0(
        "auto-tuning with >6 balancing columns may be slow. Increase",
        " in running time per additional balancing column is exponential."
      ),
      call = if (p <- sys.parent(2 + 1))
        sys.call(p)
    ))
  }

  # Make all relevant combinations
  combinations <- purrr::map(.x = seq_along(all_balance_cols),
                             .f = ~ {
                               combn(all_balance_cols, m = .x, simplify = F)
                             }) %>%
    unlist(recursive = F)

  # Add names
  names(combinations) <- paste0(".atcg_", seq_len(length(combinations)))

  # The main combination has all the balance cols
  main_combination_name <- tail(names(combinations), n = 1)

  ### . . . . . . . . .. #< 40bb882cfc30cd1a93c9e99f28a9556e ># . . . . . . . . ..
  ### Combine and fold                                                        ####

  # Find number of group columns to generate
  # Must vary with number of combinations or this
  # could explode!
  non_main_num_group_cols_to_check <- dplyr::case_when(
    length(combinations) > 30 ~ 1,
    length(combinations) > 20 ~ 2,
    length(combinations) > 10 ~ 4,
    TRUE ~ 5
  )
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
    num_new_group_cols <- ifelse(.y == main_combination_name,
                                 main_num_group_cols_to_check,
                                 non_main_num_group_cols_to_check)
    combine_and_fold_combination_(
      data = data,
      summaries = summaries,
      n = n,
      tmp_old_group_var = tmp_old_group_var,
      balance_cols = .x,
      col_name = .y,
      weights = weights[names(weights) %in% .x],
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

    # Names of deduplicated new group columns
    group_cols_names <- colnames(new_group_cols)[colnames(new_group_cols) != tmp_old_group_var]
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

  # Finding the most balanced group columns
  # As average ranking is vulnerable with many competitors
  # where a bad position on a single dimension can heavily impact avg rank
  # We first filter out the worst group cols and rank again

  # First summarization and filtering of group columns
  group_cols_to_keep <- group_cols_names
  if (length(group_cols_names) >= (num_new_group_cols * 2 + 3)) {
    # In case `num_new_group_cols` is very low (e.g. 1)
    # we add 3 to have at least 5 group cols when ordering
    group_cols_to_keep <- find_best_group_cols_(
      data = data,
      num_new_group_cols = num_new_group_cols * 2 + 3,
      group_cols_names = group_cols_names,
      cat_cols = cat_cols,
      num_cols = num_cols,
      id_cols = id_cols,
      balance_size = balance_size,
      weights = weights
    )
  }

  # Second summarization and filtering of group columns
  group_cols_to_keep <- find_best_group_cols_(
    data = data,
    num_new_group_cols = num_new_group_cols,
    group_cols_names = group_cols_to_keep,
    cat_cols = cat_cols,
    num_cols = num_cols,
    id_cols = id_cols,
    balance_size = balance_size,
    weights = weights
  )

  # Create the final order for selection of columns
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
  balance_cols,
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
    summaries = base_select(summaries, cols = c(tmp_old_group_var, balance_cols)),
    weights = weights,
    group_cols = tmp_old_group_var,
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
    dplyr::ungroup() %>%
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

find_best_group_cols_ <- function(data, num_new_group_cols, group_cols_names, cat_cols, num_cols, id_cols, weights, balance_size){

  # Summarize the balances
  balance_summary <- summarize_balances(
    data = data,
    group_cols = group_cols_names,
    cat_cols = cat_cols,
    num_cols = num_cols,
    id_cols = id_cols,
    summarize_size = balance_size,
    ranking_weights = weights,
    include_normalized = TRUE
  )

  # Find the group column rankings
  # We average the rankings of the summary and the normalized summary
  # As they have been shown to sometimes differ
  summary_ranks <- balance_summary[["Summary"]] %>%
    ranked_balances() %>%
    dplyr::select(.data$.group_col, .data$SD_rank)
  normalized_summary_ranks <- balance_summary[["Normalized Summary"]] %>%
    ranked_balances() %>%
    dplyr::select(.data$.group_col, .data$SD_rank) %>%
    dplyr::rename(norm_SD_rank = .data$SD_rank)
  summary_ranks <- summary_ranks %>%
    dplyr::left_join(normalized_summary_ranks, by = ".group_col") %>%
    dplyr::mutate(avg_rank = (.data$SD_rank + .data$norm_SD_rank) / 2) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$avg_rank) %>%
    head(num_new_group_cols)

  # Find group columns to remove
  group_cols_to_keep <- as.character(summary_ranks[[".group_col"]])
  group_cols_to_remove <- setdiff(group_cols_names, group_cols_to_keep)

  group_cols_to_keep
}
