

#   __________________ #< 5ec83ba9168e5ec8da216e162db403c9 ># __________________
#   Collapse groups wrappers                                                ####


##  .................. #< 97b9cf970cbaebba7d4719c38ad7e0bf ># ..................
##  Collapse wrappers documentation                                         ####

#' @title Collapse groups balanced by a single attribute
#' @name collapse_groups_by
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Collapses a set of groups into a smaller set of groups.
#'
#'  Balance the new groups by:
#'
#'   * The \strong{number of rows} with \code{collapse_groups_by_size()}
#'
#'   * \strong{Numerical columns} with \code{collapse_groups_by_numeric()}
#'
#'   * One or more levels of \strong{categorical columns} with \code{collapse_groups_by_levels()}
#'
#'   * Level counts in \strong{ID columns} with \code{collapse_groups_by_ids()}
#'
#'   * \strong{Any combination} of these with \code{collapse_groups()}
#'
#'  These functions wrap \code{\link[groupdata2:collapse_groups]{collapse_groups()}}
#'  to provide a simpler interface. To balance more than one of the attributes at a time
#'  and/or create multiple new unique grouping columns at once, use
#'  \code{\link[groupdata2:collapse_groups]{collapse_groups()}} directly.
#'
#'  \strong{Tip}: Check the balances of the new groups with
#'  \code{\link[groupdata2:summarize_balances]{summarize_balances()}}.
#' @inheritParams collapse_groups
#' @param n Number of new groups.
#' @param method \code{"balance"}, \code{"ascending"}, or \code{"descending"}.
#'
#'  * \code{"balance"} balances the attribute between the groups.
#'  * \code{"ascending"} orders the attribute and groups from the lowest to highest value.
#'  * \code{"descending"} orders the attribute and groups from the highest to lowest value.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @return \code{`data`} with a new grouping factor column.
#' @family grouping functions
#' @examples
#' # Attach packages
NULL


##  .................. #< 077f4d486ff8d1e493b6e0d8c6626776 ># ..................
##  Collapse by size                                                        ####


#' @rdname collapse_groups_by
#' @export
collapse_groups_by_size <- function(data,
                                    n,
                                    group_cols,
                                    auto_tune = FALSE,
                                    method = "balance", # ascending/descending
                                    extreme_pairing_levels = 1, # only method==balance
                                    col_name = ".coll_groups") {
  collapse_groups(
    data = data,
    n = n,
    group_cols = group_cols,
    auto_tune = auto_tune,
    method = method,
    extreme_pairing_levels = extreme_pairing_levels,
    num_new_group_cols = 1,
    col_name = col_name,
    balance_size = TRUE
  )
}


##  .................. #< 0bd44ba51e9f181c74193f831f89210d ># ..................
##  Collapse by numeric                                                     ####


#' @rdname collapse_groups_by
#' @export
collapse_groups_by_numeric <- function(data,
                                       n,
                                       group_cols,
                                       num_cols,
                                       balance_size = FALSE,
                                       auto_tune = FALSE,
                                       method = "balance", # ascending/descending
                                       group_aggregation_fn = mean,
                                       extreme_pairing_levels = 1,
                                       col_name = ".coll_groups") {
  collapse_groups(
    data = data,
    n = n,
    group_cols = group_cols,
    num_cols = num_cols,
    balance_size = balance_size,
    auto_tune = auto_tune,
    method = method,
    extreme_pairing_levels = extreme_pairing_levels,
    num_new_group_cols = 1,
    group_aggregation_fn = group_aggregation_fn,
    col_name = col_name
  )
}


##  .................. #< 06fd80b0d0291e8a4a1c2960e9e5c443 ># ..................
##  Collapse by factor                                                      ####


#' @rdname collapse_groups_by
#' @export
collapse_groups_by_levels <- function(data,
                                      n,
                                      group_cols,
                                      cat_cols,
                                      cat_levels = NULL,
                                      balance_size = FALSE,
                                      auto_tune = FALSE,
                                      method = "balance", # ascending/descending
                                      extreme_pairing_levels = 1,
                                      col_name = ".coll_groups") {
  collapse_groups(
    data = data,
    n = n,
    group_cols = group_cols,
    cat_cols = cat_cols,
    cat_levels = cat_levels,
    balance_size = balance_size,
    auto_tune = auto_tune,
    method = method,
    extreme_pairing_levels = extreme_pairing_levels,
    num_new_group_cols = 1,
    col_name = col_name
  )
}


##  .................. #< 99d1063b7b96a07a92a8f94b2ecbf7c1 ># ..................
##  Collapse by ID                                                          ####


#' @rdname collapse_groups_by
#' @export
collapse_groups_by_ids <- function(data,
                                   n,
                                   group_cols,
                                   id_cols,
                                   balance_size = FALSE,
                                   auto_tune = FALSE,
                                   method = "balance", # ascending/descending
                                   extreme_pairing_levels = 1,
                                   col_name = ".coll_groups") {
  collapse_groups(
    data = data,
    n = n,
    group_cols = group_cols,
    id_cols = id_cols,
    balance_size = balance_size,
    auto_tune = auto_tune,
    method = method,
    extreme_pairing_levels = extreme_pairing_levels,
    col_name = col_name
  )
}
