## rearrange
#' @title Rearrange data by a set of methods.
#' @description \strong{Internal}: Creates a rearrange factor and sorts the data by it.
#'  A rearrange factor is simply a vector of integers to sort by.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @keywords internal
#' @param data Dataframe or Vector.
#' @param method Name of method used to create rearrange factor.
#' Currently only \code{pair_extremes}.
#' \subsection{pair_extremes}{
#' The first and last rows are grouped. The second and the second last rows are grouped. Etc.
#'
#' E.g.: 1,2,3,2,1
#' }
#' @param unequal_method Name of method to use for dealing with unequal number of rows/elements in data.
#' \code{first}, \code{middle} or \code{last}
#' \subsection{first}{
#' The first group will have size 1.
#'
#' E.g. \strong{1},2,3,4,4,3,2.
#' }
#' \subsection{middle}{
#' The middle group will have size 1.
#'
#' E.g. 1,2,4,5,\strong{3},5,4,2,1.
#' }
#' \subsection{last}{
#' The last group will have size 1.
#'
#' E.g. 1,2,3,4,4,3,2,1,\strong{5}.
#' }
#' @param drop_rearrange_factor Whether to drop rearrange factor after sorting dataset. \code{Logical}.
#' @param rearrange_factor_name Name of rearrange factor.
#'
#'  N.B. Only used when \code{drop_rearrange_factor} is FALSE.
#'
rearrange <- function(data, method="pair_extremes",
                      unequal_method = "middle",
                      drop_rearrange_factor = TRUE,
                      rearrange_factor_name=".rearrange_factor"){

  # Note, pre-sorting of data must happen outside rearrange.

  local_tmp_rearrange_var <- create_tmp_var(data, ".rearrange_factor_")

  # Check data
  # Potentially convert vector to dataframe
  if (is.vector(data)){
    data <- data %>% tibble::enframe(name = NULL)
  }
  if (method %ni% c("pair_extremes")) {
    stop("'method' must be name of one of the existing methods.")
  }
  if (unequal_method %ni% c("first", "middle", "last")) {
    stop("'unequal_method' must be name of one of the existing methods for dealing with an unequal number of rows/elements.")
  }

  # Get function for creating rearrance factor
  if (method == "pair_extremes"){
    create_rearrange_factor_fn <- create_rearrange_factor_pair_extremes_
  }

  # Arrange by 'by' -> create rearrange factor -> arrange by rearrance factor
  data[[local_tmp_rearrange_var]] <- create_rearrange_factor_fn(
      size = nrow(data), unequal_method = unequal_method)
  data <- data %>%
    dplyr::arrange(!!as.name(local_tmp_rearrange_var))

  # Remove rearrange factor if it shouldn't be returned
  if (isTRUE(drop_rearrange_factor)){
    data <- data %>%
      dplyr::select(-c(!!as.name(local_tmp_rearrange_var)))
  } else {
    data <- replace_col_name(data, local_tmp_rearrange_var, rearrange_factor_name)
  }

  data

}

create_rearrange_factor_pair_extremes_ <- function(size, unequal_method = "middle") {
  #
  # Creates factor for rearranging in 1st, last, 2nd, 2nd last, 3rd, 3rd last, ...
  # When size is unequal, there are two methods for dealing with it:
  # .. "first":
  # .. .. the first row becomes group 1 on its own.
  # .. .. creates rearrange factor on the rest, all gets +1
  # .. .. e.g. 1,2,3,4,4,3,2
  # .. "middle":
  # .. .. adds ceiling(size / 4) in the middle of the factor
  # .. .. every value larger than or equal to the middle value gets +1
  # .. .. e.g. 1,2,4,5,3,5,4,2,1
  # .. "last":
  # .. .. the last row becomes the last group on its own.
  # .. .. creates rearrange factor on the rest
  # .. .. e.g. 1,2,3,4,4,3,2,1,5
  #

  half_size <- floor(size / 2)
  idx <- 1:(half_size)
  if (half_size * 2 == size) {
    return(c(idx, rev(idx)))
  } else {
    if (unequal_method == "middle") {
      middle <- ceiling((half_size / 2)) + 1
      idx <- idx %>%
        tibble::enframe(name = NULL) %>%
        dplyr::mutate(value = ifelse(.data$value >= middle, .data$value + 1, .data$value)) %>%
        dplyr::pull(.data$value)
      return(c(idx, middle, rev(idx)))
    } else if (unequal_method == "first") {
      return(c(1, c(idx, rev(idx)) + 1))
    } else if (unequal_method == "last") {
      return(c(c(idx, rev(idx)), max(idx) + 1))
    }
  }
}
