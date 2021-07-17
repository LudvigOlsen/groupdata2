
# Make an actual function with all sorts of features for comparing
# and inspecting the created groups
# This could be used for other group functions as well?
# Should be advertised next to summarize_group_cols (only has row counts)

# summarize_group_balances(df_collapsed, group_col = ".coll_groups",
#                          cat_col="diagnosis", num_col="age")


summarize_group_balances <- function(
  data,
  group_cols,
  cat_cols = NULL,
  num_cols = NULL,
  id_cols = NULL,
  include_normalized = TRUE,
  max_cat_prefix_chars = "auto") {

  #### Check arguments ####

  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, min.rows = 1, add = assert_collection)
  checkmate::assert_character(
    x = group_cols,
    min.len = 1,
    min.chars = 1,
    any.missing = FALSE,
    unique = TRUE,
    names = "unnamed",
    add = assert_collection
  )
  checkmate::assert_character(
    x = cat_cols,
    min.len = 1,
    min.chars = 1,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE,
    names = "unnamed",
    add = assert_collection
  )
  checkmate::assert_character(
    x = num_cols,
    min.len = 1,
    min.chars = 1,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE,
    names = "unnamed",
    add = assert_collection
  )
  checkmate::assert_character(
    x = id_cols,
    min.len = 1,
    min.chars = 1,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE,
    names = "unnamed",
    add = assert_collection
  )
  checkmate::assert_flag(x = include_normalized, add = assert_collection)
  checkmate::assert(
    checkmate::check_count(x = max_cat_prefix_chars, positive = TRUE),
    checkmate::check_string(x = max_cat_prefix_chars, fixed = "auto", ignore.case = TRUE)
  )
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(
    x = colnames(data),
    must.include = c(group_cols, cat_cols, num_cols, id_cols),
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Find the number of characters necessary to
  # distinguish between names in `cat_cols`
  if (!is.null(cat_cols) && max_cat_prefix_chars == "auto"){
    for (i in 5:max(nchar(cat_cols))){
      shorts <- substr(cat_cols, 1, i)
      if (length(unique(cat_cols)) == length(cat_cols)){
        max_cat_prefix_chars <- i
      }
    }
  }

  #### Create summaries ####

  # Create summaries
  summaries <-
    create_group_balance_summaries_(
      data = data,
      group_cols = group_cols,
      cat_cols = cat_cols,
      num_cols = num_cols,
      id_cols = id_cols,
      max_cat_prefix_chars = max_cat_prefix_chars
    )

  #### Combining summaries ####

  # We always have the size summary
  summary <- summaries[["size"]] %>%
    # When the arg is not NULL, add it's summary with a join
    purrr::when(!is.null(id_cols) ~
                  dplyr::left_join(., summaries[["id"]],
                                   by = c("group_col", "group")),
                ~ .) %>% # Else return the input
    purrr::when(!is.null(num_cols) ~
                  dplyr::left_join(., summaries[["num"]],
                                   by = c("group_col", "group")),
                ~ .) %>%
    purrr::when(!is.null(cat_cols) ~
                  dplyr::left_join(., summaries[["cat"]],
                                   by = c("group_col", "group")),
                ~ .)

  # Calculate standard deviations for all numeric columns
  st_deviations <- summary %>%
    dplyr::group_by(.data$group_col) %>%
    dplyr::summarize(dplyr::across(where(is.numeric), sd))

  #### Normalized summaries ####

  if (isTRUE(include_normalized)){

    # Start with summary of size
    normalized_summary <- summaries[["size"]] %>%
      # Apply log10 to counts
      dplyr::mutate(dplyr::across(where(is.numeric), function(x) {
        # In case of zero-frequencies
        log10(1 + x)
      })) %>%
      dplyr::rename_with( ~ paste0("log(", ., ")"), where(is.numeric))

    # Add summary of ID columns
    if (!is.null(id_cols)) {
      normalized_id_summary <- summaries[["id"]] %>%
        # Apply log10 to counts
        dplyr::mutate(dplyr::across(where(is.numeric), function(x) {
          # In case of zero-frequencies
          log10(1 + x)
        })) %>%
        dplyr::rename_with( ~ paste0("log(", ., ")"), where(is.numeric))

      normalized_summary <- normalized_summary %>%
        dplyr::left_join(normalized_id_summary, by = c("group_col", "group"))
    }

    # Add summary of normalized (MinMax scaled) numeric columns
    if (!is.null(num_cols)) {
      normalized_num_summary <- data %>%
        dplyr::mutate(dplyr::across(dplyr::one_of(num_cols), function(x) {
          rearrr::min_max_scale(x, new_min = 0, new_max = 1)
        })) %>% create_group_balance_summaries_(group_cols = group_cols,
                                                num_cols = num_cols) %>%
        .[["num"]] %>%
        dplyr::rename_with( ~ gsub(pattern = "(^.*\\()([[:alnum:]]*)\\)$" ,
                                   replacement = "\\1norm(\\2))", x = .), where(is.numeric))

      normalized_summary <- normalized_summary %>%
        dplyr::left_join(normalized_num_summary, by = c("group_col", "group"))
    }

    # Add summary of categorical columns
    if (!is.null(cat_cols)) {
      normalized_cat_summary <- summaries[["cat"]] %>%
        # Apply log10 to counts
        dplyr::mutate(dplyr::across(where(is.numeric), function(x) {
          # In case of zero-frequencies
          log10(1 + x)
        })) %>%
        dplyr::rename_with( ~ paste0("log(", ., ")"), where(is.numeric))
      # TODO perhaps add a row mean per cat_col after log?

      normalized_summary <- normalized_summary %>%
        dplyr::left_join(normalized_cat_summary, by = c("group_col", "group"))
    }

    # Calculate standard deviations
    normalized_st_deviations <- normalized_summary %>%
      dplyr::group_by(.data$group_col) %>%
      dplyr::summarize(dplyr::across(where(is.numeric), sd))
  }

  #### Preparing output ####

  # Prepare output list
  out <- list(
    "Summary" = summary,
    "Standard Deviations" = st_deviations
  )

  # Add normalized standard deviations
  if (isTRUE(include_normalized)){
    out[["Normalized Standard Deviations"]] <- normalized_st_deviations
  }

  out
}

create_group_balance_summaries_ <-
  function(data,
           group_cols,
           cat_cols = NULL,
           num_cols = NULL,
           id_cols = NULL,
           max_cat_prefix_chars = 5) {

  format_summary_ <- function(data){
    data %>%
      # Ensure right types
      dplyr::mutate(
        group = factor(.data$group),
        group_col = factor(.data$group_col)
      ) %>%
      position_first(col = "group_col") %>%
      dplyr::arrange(.data$group_col, .data$group)
  }

  out <- list("size" = NULL, "id" = NULL, "num" = NULL, "cat" = NULL)

  out[["size"]] <- purrr::map_df(.x = group_cols, .f = ~ {
    create_size_summary_(data = data, group_col = .x) %>%
      dplyr::rename(group = !!as.name(.x)) %>%
      dplyr::mutate(group_col = .x)
  }) %>% format_summary_()

  if (!is.null(id_cols)){
    out[["id"]] <- purrr::map_df(.x = group_cols, .f = ~ {
      create_id_summaries_(data = data,
                           group_col = .x,
                           id_cols = id_cols) %>%
        dplyr::rename(group = !!as.name(.x)) %>%
        dplyr::mutate(group_col = .x)
    }) %>% format_summary_()
  }

  if (!is.null(num_cols)){
    out[["num"]] <- purrr::map_df(.x = group_cols, .f = ~ {
      create_num_summaries_(data = data,
                            group_col = .x,
                            num_cols = num_cols) %>%
        dplyr::rename(group = !!as.name(.x)) %>%
        dplyr::mutate(group_col = .x)
    }) %>% format_summary_()
  }

  if (!is.null(cat_cols)){
    out[["cat"]] <- purrr::map_df(.x = group_cols, .f = ~ {
      create_cat_summaries_(
        data = data,
        group_col = .x,
        cat_cols = cat_cols,
        max_cat_prefix_chars = max_cat_prefix_chars # TODO allow user to specify this
      ) %>%
        dplyr::rename(group = !!as.name(.x)) %>%
        dplyr::mutate(group_col = .x)
    }) %>% format_summary_()
  }

  out
}


create_size_summary_ <- function(data, group_col){
  data %>%
    dplyr::group_by(!!as.name(group_col)) %>%
    dplyr::summarise(`# rows` = dplyr::n())
}

create_id_summaries_ <- function(data, group_col, id_cols){
  summary <- data %>%
    dplyr::group_by(!!as.name(group_col)) %>%
    dplyr::summarise(dplyr::across(dplyr::one_of(id_cols), function(x){length(unique(x))}))
  colnames(summary)[colnames(summary) != group_col] <-
    paste0("# ", colnames(summary)[colnames(summary) != group_col])
  summary
}

create_num_summaries_ <- function(data, group_col, num_cols){
  data %>%
    dplyr::group_by(!!as.name(group_col)) %>%
    dplyr::summarise(dplyr::across(dplyr::one_of(num_cols),
                                   list("mean" = mean, "sum" = sum))) %>%
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

create_cat_summaries_ <- function(data, group_col, cat_cols, max_cat_prefix_chars = 5){
  data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(dplyr::one_of(cat_cols), as.character)) %>%
    tidyr::gather(key = "cat_col", value = "cat_val", cat_cols) %>%
    dplyr::count(!!as.name(group_col), .data$cat_col, .data$cat_val) %>%
    dplyr::mutate(
      cat_col = tolower(.data$cat_col),
      cat_val = tolower(.data$cat_val),
      cat_col = substr(.data$cat_col, start = 1, stop = max_cat_prefix_chars),
      cat_name = paste0(.data$cat_col, "_", .data$cat_val),
      cat_name = gsub("^_", "", .data$cat_name)
    ) %>%
    dplyr::select(dplyr::one_of(group_col, "cat_name", "n")) %>%
    tidyr::spread(key = .data$cat_name,
                  value = .data$n,
                  fill = 0)
}
