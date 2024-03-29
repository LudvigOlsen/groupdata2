

#   __________________ #< 5a2b738ccf085ae82add326faadd209b ># __________________
#   Auto-tuning collapse_groups                                             ####


auto_tune_collapsings <- function(
  data,
  summaries,
  n,
  tmp_old_group_var,
  num_cols,
  cat_cols,
  cat_levels,
  id_cols,
  balance_size,
  weights,
  scale_fn,
  extreme_pairing_levels,
  num_new_group_cols,
  unique_new_group_cols_only,
  max_iters,
  col_name,
  parallel,
  verbose=TRUE) {


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

  # Add a combination for random splitting
  random_combination_name <- paste0(".atcg_", length(combinations) + 1)
  random_combination <- setNames(".__random__.", random_combination_name)
  combinations <- c(combinations, random_combination)


  ### . . . . . . . . .. #< 40bb882cfc30cd1a93c9e99f28a9556e ># . . . . . . . . ..
  ### Combine and fold                                                        ####


  # Find number of group columns to generate
  # Must vary with number of combinations or this
  # could explode!
  # NOTE: Remember that we also do extreme triplet grouping
  # so this number is ~doubled
  # Also informs user about
  num_cols_to_create_settings <- get_num_cols_to_create_(
    num_combinations = length(combinations),
    num_new_group_cols = num_new_group_cols
  )

  # Inform user about the number of columns to create
  if (isTRUE(verbose)) {
    inform_user_about_autotune_(
      num_cols_to_create_settings = num_cols_to_create_settings,
      num_combinations = length(combinations),
      num_new_group_cols = num_new_group_cols,
      all_balance_cols = all_balance_cols,
      parallel = parallel,
      unique_new_group_cols_only = unique_new_group_cols_only
    )
  }

  # Ensure summaries are ordered by group column
  # NOTE: This is a crucial step, otherwise
  # the returned folds won't match the order
  summaries <- summaries %>%
    dplyr::arrange(!!as.name(tmp_old_group_var))

  # Combine the balancing dimensions for each include combination
  # And create new group columns
  new_group_cols <- purrr::map2_dfc(.x = combinations, .y = names(combinations), .f = ~{
    current_num_new_group_cols <- dplyr::case_when(
      .y == main_combination_name ~ num_cols_to_create_settings[["main"]],
      .y == random_combination_name ~ num_cols_to_create_settings[["random"]],
      TRUE ~ num_cols_to_create_settings[["non_main"]])

    num_triplet_groupings_as_well <- 0
    if (.x[[1]] != '.__random__.'){
      num_triplet_groupings_as_well <- min(
        current_num_new_group_cols,
        num_new_group_cols
      )
    }

    # Get balance columns
    balance_cols <- .x

    # When performing random folding
    # we just don't provide any balance columns
    if (length(balance_cols) == 1 &&
        balance_cols == ".__random__."){
      balance_cols <- character(0)
    }

    # Combine the balancing columns
    # and fold with numerical balancing
    # of the combination
    # NOTE: summaries must be ordered by `tmp_old_group_var`
    combine_and_fold_combination_(
      data = data,
      summaries = summaries,
      n = n,
      tmp_old_group_var = tmp_old_group_var,
      balance_cols = balance_cols,
      col_name = .y,
      weights = weights[names(weights) %in% .x],
      scale_fn = scale_fn,
      extreme_pairing_levels = extreme_pairing_levels,
      num_new_group_cols = current_num_new_group_cols,
      num_triplet_groupings_as_well = num_triplet_groupings_as_well,
      unique_new_group_cols_only = unique_new_group_cols_only,
      max_iters = max_iters,
      parallel = parallel)

  }) %>%
    dplyr::mutate(!!tmp_old_group_var := summaries[[tmp_old_group_var]])

  # Names of the new group columns
  group_cols_names <- colnames(new_group_cols)[colnames(new_group_cols) != tmp_old_group_var]

  if (isTRUE(verbose)){
    cat(paste0("Succeeded in creating ", length(group_cols_names), " group columns\n"))
  }

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
      dplyr::select(-"V1_base", -"V2_base")

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

    if (isTRUE(verbose)){
      cat(paste0("Of which ", length(group_cols_names), " were unique member-wise\n"))
    }
  }

  if (isTRUE(verbose)){
    cat(paste_hyphens_(60, end_line=TRUE))
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
      cat_levels = cat_levels,
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
    cat_levels = cat_levels,
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
  num_triplet_groupings_as_well,
  unique_new_group_cols_only,
  max_iters,
  parallel) {

  if (is.unsorted(as.character(summaries[[tmp_old_group_var]]))){
    stop(paste0("`summaries` must be ordered by `tmp_old_group_var`. Otherwise",
                " the order of the returned group columns won't match the ",
                "summary order."))
  }

  # Scale, weight and combine
  summaries <- scale_and_combine_(
    data = base_select(summaries, cols = c(tmp_old_group_var, balance_cols)),
    weights = weights,
    exclude_cols = tmp_old_group_var,
    scale_fn = scale_fn,
    col_name = "combined",
    handle_no_cols = "1"
  )

  # Fold the summary
  new_groups <- summaries %>%
    fold(
      k = n,
      num_col = "combined",
      extreme_pairing_levels = extreme_pairing_levels,
      num_fold_cols = num_new_group_cols,
      unique_fold_cols_only = unique_new_group_cols_only,
      max_iters = max_iters,
      use_of_triplets = "fill",
      parallel = parallel
    ) %>%
    dplyr::ungroup()

  # Perform folding with extreme triplet grouping
  if (num_triplet_groupings_as_well>0) {
    new_groups <- new_groups %>%
      fold(
        k = n,
        num_col = "combined",
        extreme_pairing_levels = max(1, extreme_pairing_levels - 1),
        num_fold_cols = num_triplet_groupings_as_well,
        unique_fold_cols_only = unique_new_group_cols_only,
        max_iters = max_iters,
        use_of_triplets = "instead",
        handle_existing_fold_cols = "keep",
        parallel = parallel
      ) %>%
      dplyr::ungroup()
  }

  new_groups <- new_groups %>%
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


find_best_group_cols_ <- function(data, num_new_group_cols, group_cols_names,
                                  cat_cols, cat_levels, num_cols, id_cols,
                                  weights, balance_size){

  # Ensure cat_levels is a named list with named vectors
  # with zero-weights for non-specified levels
  if (!is.null(cat_cols) &&
      !is.null(cat_levels)){

    # Function to apply to levels from each cat_col
    add_missing_levels <- function(data, cat_col, cat_levels){
      if (is.character(cat_levels)){
        cat_levels <- setNames(rep(1, length(cat_levels)), cat_levels)
      }
      if (is.numeric(cat_levels)){
        no_weight_levels <- setdiff(
          levels(factor(as.character(data[[cat_cols]]))),
          names(cat_levels)
        )
        # Add the missing levels with weight 0
        cat_levels <- c(
          cat_levels,
          setNames(rep(0, length(no_weight_levels)), no_weight_levels)
        )
      }
      cat_levels
    }

    # Prepare cat_levels for each cat_col
    if (is.list(cat_levels)){
      cat_levels <- purrr::map(names(cat_levels), ~ {
        add_missing_levels(data = data,
                           cat_col = .x,
                           cat_levels = cat_levels[[.x]])
      }) %>% setNames(names(cat_levels))
    } else {
      if (length(cat_cols)>1){
        stop(paste0("When `cat_cols` has more than one element, `cat_cols` must ",
                    "be a named list with named vectors."))
      }
      cat_levels <- list(add_missing_levels(
        data = data,
        cat_col = cat_cols,
        cat_levels = cat_levels
      )) %>% setNames(cat_cols)
    }
  }

  # Summarize the balances
  balance_summary <- summarize_balances(
    data = data,
    group_cols = group_cols_names,
    cat_cols = cat_cols,
    num_cols = num_cols,
    id_cols = id_cols,
    summarize_size = balance_size,
    rank_weights = weights,
    cat_levels_rank_weights = cat_levels,
    include_normalized = TRUE
  )

  # Find the group column rankings
  # We average the rankings of the summary and the normalized summary
  # As they have been shown to sometimes differ
  summary_ranks <- balance_summary[["Summary"]] %>%
    ranked_balances() %>%
    dplyr::select(".group_col", "SD_rank")
  normalized_summary_ranks <- balance_summary[["Normalized Summary"]] %>%
    ranked_balances() %>%
    dplyr::select(".group_col", "SD_rank") %>%
    dplyr::rename(norm_SD_rank = "SD_rank")
  summary_ranks <- summary_ranks %>%
    dplyr::left_join(normalized_summary_ranks, by = ".group_col") %>%
    dplyr::mutate(avg_rank = (.data$SD_rank + .data$norm_SD_rank) / 2) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$avg_rank) %>%
    head(num_new_group_cols)

  # Find group columns to remove
  # Return names of best group columns
  as.character(summary_ranks[[".group_col"]])

}


# Set how many group columns of different types to create
get_num_cols_to_create_ <- function(num_combinations,
                                    num_new_group_cols) {

  # Check arguments
  checkmate::assert_count(num_combinations, positive = TRUE)
  checkmate::assert_count(num_new_group_cols, positive = TRUE)

  # Find number of group columns to generate
  # Must vary with number of combinations or this
  # could explode!
  # NOTE: Remember that we also do extreme triplet grouping
  # so this number is ~doubled
  non_main_num_group_cols_to_check <- dplyr::case_when(
    num_combinations > 25 ~ 1,
    num_combinations > 10 ~ 2,
    num_combinations > 5 ~ 3,
    TRUE ~ 4
  )

  # Number of group columns to create for the
  # main combination of balancing columns
  main_num_group_cols_to_check <- dplyr::case_when(
    num_new_group_cols < 6 ~ 9,
    num_new_group_cols < 20 ~ ceiling(num_new_group_cols * 1.8),
    num_new_group_cols < 50 ~ ceiling(num_new_group_cols * 1.4),
    TRUE ~ num_new_group_cols + 15
  )

  # Number of group columns to create
  # without any numeric balancing
  num_random_group_cols_to_check <- dplyr::case_when(
    num_new_group_cols < 20 ~ 15,
    num_new_group_cols < 50 ~ 30,
    num_new_group_cols < 75 ~ 40,
    TRUE ~ ceiling(num_new_group_cols / 2)
  )

  list(
    "main" = main_num_group_cols_to_check,
    "non_main" = non_main_num_group_cols_to_check,
    "random" = num_random_group_cols_to_check
  )

}


inform_user_about_autotune_ <- function(
  num_cols_to_create_settings,
  num_combinations,
  num_new_group_cols,
  all_balance_cols,
  parallel,
  unique_new_group_cols_only,
  return_string = FALSE) {

  # Check arguments
  checkmate::assert_list(
    num_cols_to_create_settings,
    names = "unique",
    any.missing = FALSE
  )
  checkmate::check_names(names(num_cols_to_create_settings),
                         permutation.of = c("main", "non_main", "random"))
  for (nm in c("main", "non_main", "random")){
    checkmate::assert_count(
      num_cols_to_create_settings[[nm]],
      positive = TRUE,
      .var.name = paste0("num_cols_to_create_settings[['", nm, "']]")
    )
  }
  checkmate::assert_character(all_balance_cols, any.missing = FALSE)
  checkmate::assert_count(num_combinations, positive = TRUE)
  checkmate::assert_count(num_new_group_cols, positive = TRUE)
  checkmate::assert_flag(return_string)

  # Inform the user about the process

  # Calculate number of total columns to create

  # Extreme pairing balancing
  num_total_checks_paired <- sum(c(
    num_cols_to_create_settings[["non_main"]] * (num_combinations - 2),  # Without main and random
    num_cols_to_create_settings[["main"]]
  ))

  # Extreme triplet grouping balancing
  num_total_checks_triplets <- sum(c(
    min(num_new_group_cols, num_cols_to_create_settings[["non_main"]]) * (num_combinations - 2), # Without main and random
    min(num_new_group_cols, num_cols_to_create_settings[["main"]])
  ))

  # In total
  num_total_checks <- sum(
    c(
      num_total_checks_paired,
      num_total_checks_triplets,
      num_cols_to_create_settings[["random"]]
    )
  )

  # Inform the user
  string <- inform_user_about_autotune_message_(
    num_new_group_cols = num_new_group_cols,
    balance_cols = all_balance_cols,
    total_checks = num_total_checks,
    total_checks_paired = num_total_checks_paired,
    total_checks_triplets = num_total_checks_triplets,
    num_random_group_cols = num_cols_to_create_settings[["random"]],
    parallel = parallel,
    unique_only = unique_new_group_cols_only,
    width = 60
  )

  # Either return string (for testing) or `cat` to user
  if (isTRUE(return_string)){
    return(string)
  }

  # Inform user
  cat(string)

}


inform_user_about_autotune_message_ <- function(
  num_new_group_cols,
  balance_cols,
  total_checks,
  total_checks_paired,
  total_checks_triplets,
  num_random_group_cols,
  parallel,
  unique_only,
  width = 60) {

  plural_s <- ifelse(num_new_group_cols > 1, "s", "")
  plural_no_s <- ifelse(num_new_group_cols > 1, "", "s")

  # Inform the user
  string <- paste0(
    paste_hyphens_(width), "\n",
    "  `collapse_groups()` auto-tuning\n",
    paste0(c(rep("-", width)), collapse = ""), "\n",
    "  Finding ", num_new_group_cols, ifelse(isTRUE(unique_only), " unique", ""),
    " group collapsing", plural_s, " that ",
    "best balance", plural_no_s, ":\n    ",
    paste0(strwrap(paste0(balance_cols, collapse=", "), width=width-6, exdent = 4), collapse = "\n"),
    "\n  Will attempt to create:",
    "\n    Extreme pairing balanced splits: ", total_checks_paired,
    "\n    Extreme triplets balanced splits: ", total_checks_triplets,
    "\n    Random splits: ", num_random_group_cols,
    "\n    Total number of grouping columns: ", total_checks,
    ifelse(!isTRUE(parallel) && total_checks > 20,  # Arbitrary threshold
           paste0(
             "\n", paste0(c(rep("-", width)), collapse = ""),
             "\n  Consider enabling parallelization. See ?collapse_groups"
           ),
           ""),
    "\n", paste_hyphens_(width), "\n"
  )

  string

}
