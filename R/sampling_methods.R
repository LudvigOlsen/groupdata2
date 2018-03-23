# Sampling methods

n_ids_ <- function(data, size, cat_col, id_col, mark_new_rows) {
  #
  # Makes sure there is the same number of IDs in each category
  #

  # Count ids per group
  rows_per_id_per_category <- data %>%
    dplyr::count(!!as.name(cat_col), !!as.name(id_col)) %>%
    dplyr::select(-c(n))

  # balance the number of ids per category
  balanced_ids <- balance(rows_per_id_per_category,
                          size,
                          cat_col = cat_col,
                          mark_new_rows = TRUE,
                          new_rows_col_name=".ids_balanced_new_rows")

  # select the chosen ids in data and return
  balanced_data <- dplyr::full_join(data, balanced_ids,
                                    by=c(cat_col, id_col)) %>%
    dplyr::mutate(.TempNewRow = dplyr::if_else(.TempNewRow + .ids_balanced_new_rows > 0, 1, 0)) %>%
    dplyr::select(-c(.ids_balanced_new_rows))

  if (!isTRUE(mark_new_rows)) {
    balanced_data$.TempNewRow <- NULL
  }
  balanced_data

}
#
# n_rows_ <- function(data, size, cat_col, id_col, mark_new_rows){
#
#   #
#   # Tries to make the number of rows in each cat_col as equal as possible
#   # While still respecting the IDs.
#   #
#
#   # Count ids per group
#   rows_per_id_per_category <- data %>%
#     dplyr::count(!!as.name(cat_col), !!as.name(id_col))
#   # rows_per_group <- rows_per_id_per_category %>%
#   #   dplyr::group_by(!!as.name(cat_col)) %>%
#   #   dplyr::summarize(total_rows = sum(n))
#   to_size <- get_target_size(data, size, cat_col)
#   print(rows_per_id_per_category)
#   print(to_size)
#
#   balanced_ids <- plyr::ldply(unique(rows_per_id_per_category[[cat_col]]), function(category) {
#     ids_for_cat <- rows_per_id_per_category %>%
#       filter(!!as.name(cat_col) == category)
#     current_n_rows <- sum(ids_for_cat$n)
#     difference <- current_n_rows-to_size
#
#   stop("NOT IMPLEMENTED YET!")
# #
# #     n_rows <- nrow(data_for_cat)
# #
# #     print(n_rows)
# #     print(data_for_cat)
# #
# #     if (n_rows == to_size) {
# #       return(data_for_cat)
# #     } else if (n_rows < to_size) {
# #
# #       stop()
# #       # return(add_rows_with_sampling(data_for_cat, to_size = to_size))
# #     } else {
# #       print("not implemented")
# #       # data_for_cat %>%
# #       #   sample_n(size = to_size, replace = FALSE)
# #     }
#   })
#
#
# }


