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

id_method_n_rows_optimal <- function(data, size, cat_col, id_col, mark_new_rows){

  # for each cat
  #   shuffle the rows
  #   use dyn. prog. to get a matrix of best combinations
  #   return first that meets target size
  #   otherwise return best combination (sampling if multiple winners)

}

id_method_nested <- function(data, size, cat_col, id_col, mark_new_rows){

  # nested balancing
  # calls balance on each category with id as cat_col

  plyr::ldply(unique(data[[cat_col]]), function(category) {
      data %>%
        dplyr::filter(!!as.name(cat_col) == category) %>%
        balance(size=size, cat_col=id_col, mark_new_rows=mark_new_rows)
    })

}

id_method_distributed <- function(data, size, cat_col, id_col, mark_new_rows){

  # Count of rows
  rows_per_id_per_category <- data %>%
    dplyr::count(!!as.name(cat_col), !!as.name(id_col))

  # Get target size (number of rows)
  to_size <- get_target_size(data, size, cat_col)

  # Get the number of rows to keep for each ID
  balanced_ids <-
    plyr::ldply(unique(rows_per_id_per_category[[cat_col]]), function(category) {

      # Subset data with current category
      ids_for_cat <- rows_per_id_per_category %>%
        dplyr::filter(!!as.name(cat_col) == category)

      # Get stats on subset
      current_n_rows <- sum(ids_for_cat$n)
      difference <- current_n_rows - to_size


      if (difference == 0) {
        return(ids_for_cat)

      #### Adding rows ####
      } else if (difference < 0){

        # The number of rows to add
        to_add <- abs(difference)

        # Get stats on subset
        current_n_ids <- nrow(ids_for_cat)
        add_to_all <- floor(to_add/current_n_ids)
        to_distribute <- to_add - add_to_all * current_n_ids

        # Find which IDs get an extra (distributed) row
        add_factor <- c(rep(1, to_distribute), rep(0, current_n_ids-to_distribute))
        add_factor <- sample(add_factor)
        ids_for_cat$add_factor <- add_factor

        # Now add number of rows to the n column
        ids_for_cat <- ids_for_cat %>%
          dplyr::mutate(n = n + add_factor + add_to_all) %>%
          dplyr::select(-c(add_factor))

        return(ids_for_cat)

      #### Removing rows ####
      } else if (difference > 0){

        # The number of rows to remove
        to_remove <- abs(difference)

        # Iteratively:
        # .. Update statistics
        # .. Divide the number of rows to remove by the number of IDs.
        # .. Pick the smallest between this and the minimum ID size.
        # .. Subtract this number from n and remove all IDs with n == 0.
        # .. Break
        # .. .. if nothing needs to be removed
        # .. .. if the number of rows to remove is smaller then the number of IDs


        while(TRUE){
          if (to_remove == 0){
            break
          }

          # Update stats
          current_n_ids <- nrow(ids_for_cat)
          min_nrows <- min(ids_for_cat$n)
          remove_from_all <- floor(to_remove/current_n_ids)
          remove_from_all <- dplyr::if_else(remove_from_all>0,
                                            min(c(remove_from_all,min_nrows)),
                                            0)

          if (remove_from_all == 0){
            break
          }

          # Remove from all IDs
          # Filter out empty IDs
          ids_for_cat <- ids_for_cat %>%
            dplyr::mutate(n = n - remove_from_all) %>%
            dplyr::filter(n > 0)

          # Update number of rows to remove
          to_remove <- to_remove - remove_from_all*current_n_ids
        }

        # If there are still rows to remove
        # Make a list of the 1s to remove and 0s for the rest.
        # Shuffle the list and subtract it from n

        if (to_remove > 0) {
          current_n_ids <- nrow(ids_for_cat)
          remove_factor <- c(rep(1,to_remove), rep(0,current_n_ids-to_remove))
          remove_factor <- sample(remove_factor)
          ids_for_cat$remove_factor <- remove_factor
          ids_for_cat <- ids_for_cat %>%
            dplyr::mutate(remove_factor=remove_factor,
                          n = n - remove_factor) %>%
            dplyr::select(-c(remove_factor))
        }
        return(ids_for_cat)
      }
    }) %>%
    dplyr::rename(.to_keep_ = n)

  plyr::ldply(unique(balanced_ids[[id_col]]), function(id) {
    # Subset data with current category
    data_for_id <- data %>%
      dplyr::filter(!!as.name(id_col) == id)

    # Get the number of rows to keep for this ID
    to_keep <- balanced_ids %>%
      dplyr::filter(!!as.name(id_col) == id) %>%
      dplyr::pull(.data$.to_keep_)

    # Call balance on the subset, to get the balanced (up-/downsampled) ID
    data_for_id %>%
      balance(size = to_keep, cat_col = id_col, mark_new_rows = TRUE, new_rows_col_name = ".TempNewRow")

  })
}
