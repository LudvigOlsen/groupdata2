## find_starts
#' @title Find start positions of groups in data.
#' @description Finds values or indices of values that are not the same
#' as the previous value.
#'
#' E.g. to use with the \code{l_starts} method.
#'
#' Wraps \code{\link{differs_from_previous}()}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @param data Dataframe or Vector
#'
#' N.B. If checking a factor, it is converted to a character vector.
#' Conversion will generate a warning, which can be turned off by setting \code{factor_conversion_warning} to \code{FALSE}.
#' @param return_index Return indices of starts. (Logical)
#' @param col Name of column to find starts in. Used when data is
#'  dataframe. (Character)
#' @param factor_conversion_warning Generate warning when converting factor to character. (Logical)
#' @return Vector with either start values or indices of start values.
#' @family l_starts tools
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create a dataframe
#' df <- data.frame('a' = c('a','a','b',
#'                          'b','c','c'))
#'
#' # Get start values for new groups in column 'a'
#' find_starts(df, col = 'a')
#'
#' # Get indices of start values for new groups
#' # in column 'a'
#' find_starts(df, col = 'a',
#'             return_index = TRUE)
#'
#' ## Use found starts with l_starts method
#' # Notice: This is equivalent to n = 'auto'
#' # with l_starts method
#'
#' # Get start values for new groups in column 'a'
#' starts <- find_starts(df, col = 'a')
#'
#' # Use starts in group() with 'l_starts' method
#' group(df, n = starts, method = 'l_starts',
#'       starts_col = 'a')
#'
#' # Similar but with indices instead of values
#'
#' # Get indices of start values for new groups
#' # in column 'a'
#' starts_ind <- find_starts(df, col = 'a',
#'                           return_index = TRUE)
#'
#' # Use starts in group() with 'l_starts' method
#' group(df, n = starts_ind, method = 'l_starts',
#'       starts_col = 'index')
#'
#'
find_starts <- function(data, col = NULL, return_index = FALSE, factor_conversion_warning=TRUE){

  differs_from_previous(data, col=col, threshold=NULL,
                        return_index=return_index,
                        include_first = TRUE,
                        factor_conversion_warning=factor_conversion_warning)

}


