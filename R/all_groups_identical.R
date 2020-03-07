#' @title Test if two grouping factors contain the same groups.
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  Checks whether two grouping factors contain the same groups,
#'  looking only at the group members, allowing for different group names / identifiers.
#' @param x,y Two grouping factors (vectors/factors with group identifiers) to compare.
#'
#'  \strong{N.B.} Both are converted to character vectors.
#' @details
#'  Both factors are sorted by \code{x}.
#'  A grouping factor is created with new groups starting at the values in
#'  \code{y} which differ from the previous row
#'  (i.e. \code{\link{group}()} with \code{method = "l_starts"} and \code{n = "auto"}).
#'  A similar grouping factor is created for \code{x},
#'  to have group identifiers range from \code{1} to the number of groups.
#'  The two generated grouping factors are tested for equality.
#' @return Whether \strong{all} groups in \code{x} are the same in \code{y}, \emph{memberwise}. (logical)
#' @family grouping functions
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @examples
#' # Attach groupdata2
#' library(groupdata2)
#'
#' # Same groups, different identifiers
#' x1 <- c(1, 1, 2, 2, 3, 3)
#' x2 <- c(2, 2, 1, 1, 4, 4)
#' all_groups_identical(x1, x2) # TRUE
#'
#' # Same groups, different identifier types
#' x1 <- c(1, 1, 2, 2, 3, 3)
#' x2 <- c("a", "a", "b", "b", "c", "c")
#' all_groups_identical(x1, x2) # TRUE
#'
#' # Not same groups
#' # Note that all groups must be the same to return TRUE
#' x1 <- c(1, 1, 2, 2, 3, 3)
#' x2 <- c(1, 2, 2, 3, 3, 3)
#' all_groups_identical(x1, x2) # FALSE
#'
#' # Different number of groups
#' x1 <- c(1, 1, 2, 2, 3, 3)
#' x2 <- c(1, 1, 1, 2, 2, 2)
#' all_groups_identical(x1, x2) # FALSE
all_groups_identical <- function(x, y) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert(checkmate::check_vector(x = x, strict=TRUE),
                    checkmate::check_factor(x = x),
                    .var.name = "x")
  checkmate::assert(checkmate::check_vector(x = y, strict=TRUE),
                    checkmate::check_factor(x = y),
                    .var.name = "y")
  if (length(x) != length(y))
    assert_collection$push("'x' and 'y' must have same length.")
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  d <- tibble::tibble(
    "col_1" = as.character(x),
    "col_2" = as.character(y)
  ) %>%
    dplyr::arrange(.data$col_1) %>%
    group(
      n = "auto", method = "l_starts",
      col_name = ".groups_1",
      starts_col = "col_2"
    ) %>%
    dplyr::ungroup()

  if (nlevels(d[[".groups_1"]]) != length(unique(d[["col_1"]]))) {
    return(FALSE)
  } else {
    d <- d %>%
      group(
        n = "auto",
        method = "l_starts",
        col_name = ".groups_2",
        starts_col = "col_1"
      ) %>%
      dplyr::ungroup()
    all(as.character(d[[".groups_1"]]) == as.character(d[[".groups_2"]]))
  }
}
