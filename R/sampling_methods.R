# Sampling methods

id_method_n_ids_ <- function(data, size, cat_col, id_col, mark_new_rows) {
  #
  # Makes sure there is the same number of IDs in each category
  #

  # Count ids per group
  rows_per_id_per_category <- data %>%
    dplyr::count(!!as.name(cat_col),!!as.name(id_col)) %>%
    dplyr::select(-c(n))

  # balance the number of ids per category
  balanced_ids <- balance(
    rows_per_id_per_category,
    size,
    cat_col = cat_col,
    mark_new_rows = TRUE,
    new_rows_col_name = ".ids_balanced_new_rows"
  )

  select_rows_from_ids(data, balanced_ids, cat_col, id_col, mark_new_rows)

}

id_method_n_rows_closest <- function(data, size, cat_col, id_col, mark_new_rows) {
  #
  # Tries to make the number of rows in each cat_col as equal as possible
  # While still respecting the IDs.
  #

  # Count ids per group
  rows_per_id_per_category <- data %>%
    dplyr::count(!!as.name(cat_col),!!as.name(id_col))
  # Get target size (number of rows)
  to_size <- get_target_size(data, size, cat_col)

  balanced_ids <-
    plyr::ldply(unique(rows_per_id_per_category[[cat_col]]), function(category) {
      # Subset data with current category
      ids_for_cat <- rows_per_id_per_category %>%
        dplyr::filter(!!as.name(cat_col) == category) %>%
        dplyr::mutate(.ids_balanced_new_rows = 0)

      # Get stats on subset
      current_n_rows <- sum(ids_for_cat$n)
      difference <- current_n_rows - to_size

      if (difference == 0) {
        return(ids_for_cat)

      } else if (difference < 0) {

        #### Repeat dataset ####

        n_to_repeat <- (to_size / current_n_rows) - 1
        if (n_to_repeat >= 1) {
          # TEST THIS
          # repeat dataframe
          ids_for_cat <- ids_for_cat %>%
            dplyr::bind_rows(
              ids_for_cat[rep(seq_len(nrow(ids_for_cat)), floor(n_to_repeat)),] %>%
              dplyr::mutate(.ids_balanced_new_rows = 1))
          current_n_rows <- sum(ids_for_cat$n)
          difference <- current_n_rows - to_size
          if (difference == 0)
            return(ids_for_cat)
        }

        #### Sample closest match from dataset iteratively ####

        # Copy data. For popping elements, to avoid duplicates.
        ids_to_sample_from <- ids_for_cat
        while (TRUE) {
          if (difference == 0) {
            return(ids_for_cat)
          }
          # Get the one where n is closest to difference
          closest_n <-
            ids_to_sample_from[which.min(abs(ids_to_sample_from$n - abs(difference))), ] %>%
            dplyr::sample_n(1)
          # Remove the used row from ids_to_sample_from
          ids_to_sample_from <- ids_to_sample_from %>%
            dplyr::anti_join(closest_n,
                             by = c(cat_col,
                                    id_col,
                                    "n",
                                    ".ids_balanced_new_rows"
                             ))
          # Only add the new row if the difference will be closer to zero afterwards
          if (closest_n[["n"]] > abs(difference)) {
            return(ids_for_cat)
          } else {
            ids_for_cat <- ids_for_cat %>%
              dplyr::bind_rows(closest_n  %>%
                                 dplyr::mutate(.ids_balanced_new_rows = 1))
            # Update stats
            current_n_rows <- sum(ids_for_cat$n)
            difference <- current_n_rows - to_size
          }
        }
      } else if (difference > 0) {

        #### Removing IDs with n closest to difference ####

        while (TRUE) {
          if (difference == 0) {
            return(ids_for_cat)
          }
          # Get the one where n is closest to difference
          closest_n <-
            ids_for_cat[which.min(abs(ids_for_cat$n - difference)), ] %>%
            dplyr::sample_n(1)
          # Only add the new row if the difference will be closer to zero afterwards
          if (closest_n[["n"]] > difference) {
            return(ids_for_cat)
          } else {
            # Remove the row from ids_for_cat
            ids_for_cat <- ids_for_cat %>%
              dplyr::anti_join(closest_n,
                               by = c(cat_col,
                                      id_col,
                                      "n"))
            # Update stats
            current_n_rows <- sum(ids_for_cat$n)
            difference <- current_n_rows - to_size
          }
        }
      }
    }) %>% dplyr::select(-c(n))

  select_rows_from_ids(data, balanced_ids, cat_col, id_col, mark_new_rows)

}
