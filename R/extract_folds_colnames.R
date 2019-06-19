

extract_fold_colnames <- function(data, regex="^.folds"){
  colnames(data)[grep(regex, colnames(data))]
}

# Extracts the max number in the end of fold column names
# As ".fold" will return NA, we set the min. output to 0
extract_max_fold_cols_number <- function(fold_colnames){
  max_number <- max(c(0, as.integer(substring(fold_colnames, 8))), na.rm = TRUE)
  max_number
}

name_new_fold_col <- function(num_to_create,
                              num_existing,
                              max_existing_number,
                              current){
  ifelse(num_to_create > 1 || num_existing > 0,
         paste0(".folds_", current + max_existing_number),
         ".folds")
}
