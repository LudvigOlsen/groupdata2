
# Make an actual function with all sorts of features for comparing
# and inspecting the created groups
# This could be used for other group functions as well?
# Should be advertised next to summarize_group_cols (only has row counts)

# summarize_group_balances(df_collapsed, group_col = ".coll_groups",
#                          cat_col="diagnosis", num_col="age")

# TODO should work when cat_col or num_col are NULL
summarize_group_balances <- function(data, group_col, cat_col, num_col){

  summaries <- dplyr::tibble(
    !!group_col := unique(data[[group_col]])
  )
  numeric_summaries <- data %>%
    dplyr::group_by(!!as.name(group_col)) %>%
    dplyr::summarise(mean_num = mean(!!as.name(num_col)),
                     sum_num = sum(!!as.name(num_col)),
                     size = dplyr::n())
  cat_summary <- data %>%
    dplyr::count(!!!rlang::syms(c(group_col, cat_col))) %>%
    tidyr::spread(key = !!as.name(cat_col),
                  value = .data$n,
                  fill = 0)
  summaries %>%
    dplyr::left_join(numeric_summaries, by = group_col) %>%
    dplyr::left_join(cat_summary, by = group_col)

}

score_group_balances <- function(data, group_col, cat_col, num_col){

  # Standardize numeric column first
  #data <- data %>%
  #  dplyr::mutate(!!num_col := standardize(!!as.name(num_col)))

  summary <-
    summarize_group_balances(
      data = data,
      group_col = group_col,
      cat_col = cat_col,
      num_col = num_col
    ) %>%
    # dplyr::mutate(dplyr::across(where(is.numeric) &
    #                               !starts_with(group_col), standardize)) %>%
    dplyr::summarize(dplyr::across(where(is.numeric) &
                                  !starts_with(group_col), sd))
  summary

}
