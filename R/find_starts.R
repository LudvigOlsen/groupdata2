## find_starts
#' @title Find start positions of groups in data
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  Finds values or indices of values that are not the same
#'  as the previous value.
#'
#'  E.g. to use with the \code{"l_starts"} method.
#'
#'  Wraps \code{\link{differs_from_previous}()}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param data \code{data.frame} or \code{vector}.
#'
#'  \strong{N.B.} If checking a \code{factor}, it is converted to a \code{character vector}.
#'  Conversion will generate a warning, which can be turned off by
#'  setting \code{`factor_conversion_warning`} to \code{FALSE}.
#'
#'  \strong{N.B.} If \code{`data`} is a \emph{grouped} \code{data.frame},
#'  the function is applied group-wise and the output is a \code{list} of \code{vector}s.
#'  The names are based on the group indices
#'  (see \code{\link[dplyr:group_indices]{dplyr::group_indices()}}).
#' @param return_index Whether to return indices of starts. (Logical)
#' @param col Name of column to find starts in. Used when \code{`data`} is
#'  a \code{data.frame}. (Character)
#' @inheritParams differs_from_previous
#' @param factor_conversion_warning Throw warning when
#'  converting \code{factor} to \code{character}. (Logical)
#' @return \code{vector} with either the start values or the indices of the start values.
#'
#'  \strong{N.B.} If \code{`data`} is a \emph{grouped} \code{data.frame},
#'  the output is a \code{list} of \code{vector}s.
#'  The names are based on the group indices
#'  (see \code{\link[dplyr:group_indices]{dplyr::group_indices()}}).
#' @family l_starts tools
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "a" = c("a", "a", "b", "b", "c", "c"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Get start values for new groups in column 'a'
#' find_starts(df, col = "a")
#'
#' # Get indices of start values for new groups
#' # in column 'a'
#' find_starts(df,
#'   col = "a",
#'   return_index = TRUE
#' )
#'
#' ## Use found starts with l_starts method
#' # Notice: This is equivalent to n = 'auto'
#' # with l_starts method
#'
#' # Get start values for new groups in column 'a'
#' starts <- find_starts(df, col = "a")
#'
#' # Use starts in group() with 'l_starts' method
#' group(df,
#'   n = starts, method = "l_starts",
#'   starts_col = "a"
#' )
#'
#' # Similar but with indices instead of values
#'
#' # Get indices of start values for new groups
#' # in column 'a'
#' starts_ind <- find_starts(df,
#'   col = "a",
#'   return_index = TRUE
#' )
#'
#' # Use starts in group() with 'l_starts' method
#' group(df,
#'   n = starts_ind, method = "l_starts",
#'   starts_col = "index"
#' )
find_starts <- function(data,
                        col = NULL,
                        return_index = FALSE,
                        handle_na = "ignore",
                        factor_conversion_warning = TRUE) {
  differs_from_previous(
    data = data,
    col = col,
    threshold = NULL,
    return_index = return_index,
    include_first = TRUE,
    handle_na = handle_na,
    factor_conversion_warning = factor_conversion_warning
  )
}
