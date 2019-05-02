extract_folds_colnames <- function(data, regex="^.folds"){
  colnames(data)[grep(regex, colnames(data))]
}
