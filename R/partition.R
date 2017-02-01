partition <- function(data, p = list(0.2),cat_col = NULL,
                      id_col = NULL, fixed = FALSE) {

  #
  # Balanced partitioning
  # data: dataframe or vector
  # p: list of partitions given as percentage (0-1) or group sizes (wholenumber)
  # cat_col: Categorical variable to balance by
  # id_col: ID column to keep rows with shared IDs in the same partition
  # fixed: Whether you only want the inputted partitions or the exceeding values gets a partition (logical)
  #        FALSE allows you to pass "p = 0.2" and get 2 partions - 0.2 and 0.8
  #


  # If cat_col is not NULL
  if (!is.null(cat_col)){

    # If id_col is not NULL
    if (!is.null(id_col)){

      # Group by cat_col
      # For each group:
      # .. create groups of the unique IDs (e.g. subjects)
      # .. add grouping factor to data
      # Group by new grouping factor '.partitions'

      data <- data %>%
        group_by_(cat_col) %>%
        do(group_uniques_(., n = p, id_col, method = 'l_sizes',
                          col_name = '.partitions')) %>%
        group_by_('.partitions')


      # If id_col is NULL
    } else {

      # Group by cat_col
      # Create groups from data
      # .. and add grouping factor to data

      data <- data %>%
        group_by_(cat_col) %>%
        do(group(., n = p, method = 'l_sizes',
                 randomize = TRUE,
                 col_name = '.partitions'))


    }


    # If cat_col is NULL
  } else {

    # If id_col is not NULL
    if (!is.null(id_col)){

      # Create groups of unique IDs
      # .. and add grouping factor to data

      data <- data %>%
        group_uniques_(n = p, id_col, method = 'l_sizes',
                       col_name = '.partitions')


      # If id_col is NULL
    } else {

      # Create groups from all the data points
      # .. and add grouping factor to data

      data <- group(data, n = p,
                    method = 'l_sizes',
                    randomize = TRUE,
                    col_name = '.partitions')

    }

  }


  # Return data
  return(data)


}

