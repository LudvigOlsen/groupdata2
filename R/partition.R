partition <- function(data, p, cat_col = NULL,
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


  # Currently not working

  return()



}

