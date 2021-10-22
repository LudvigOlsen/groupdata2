

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
#'  While, \emph{on average}, the balancing work better than without, this is
#'  \strong{not guaranteed on every run}. \code{`auto_tune`} (enabled by default) can yield
#'  a much better overall balance than without in most contexts. This generates a larger set
#'  of group columns using all combinations of the balancing columns and selects the
#'  most balanced group column(s). This is slower and can be speeded up by enabling
#'  parallelization (see \code{`parallel`}).
#'
#'  \strong{Tip}: When speed is more important than balancing, disable \code{`auto_tune`}.
#'
#'  \strong{Tip}: Check the balances of the new groups with
#'  \code{\link[groupdata2:summarize_balances]{summarize_balances()}} and
#'  \code{\link[groupdata2:ranked_balances]{ranked_balances()}}.
#'
#'  \strong{Note}: The categorical and ID balancing algorithms are different to those
#'  in \code{\link[groupdata2:fold]{fold()}} and
#'  \code{\link[groupdata2:partition]{partition()}}.
#' @details See details in \code{\link[groupdata2:collapse_groups]{collapse_groups()}}.
#' @inheritParams collapse_groups
#' @param n Number of new groups.
#' @param method \code{"balance"}, \code{"ascending"}, or \code{"descending"}.
#'
#'  * \code{"balance"} balances the attribute between the groups.
#'  * \code{"ascending"} orders by the attribute and groups from the lowest to highest value.
#'  * \code{"descending"} orders by the attribute and groups from the highest to lowest value.
#' @param parallel Whether to parallelize the group column comparisons
#'  when \code{`auto_tune`} is enabled.
#'
#'  Requires a registered parallel backend.
#'  Like \code{doParallel::registerDoParallel}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @return \code{`data`} with a new grouping factor column.
#' @family grouping functions
#' @examples
#' # Attach packages
#' library(groupdata2)
#' library(dplyr)
#'
#' # Set seed
#' xpectr::set_test_seed(42)
#'
#' # Create data frame
#' df <- data.frame(
#'   "participant" = factor(rep(1:20, 3)),
#'   "age" = rep(sample(c(1:100), 20), 3),
#'   "answer" = factor(sample(c("a", "b", "c", "d"), 60, replace = TRUE)),
#'   "score" = sample(c(1:100), 20 * 3)
#' )
#' df <- df %>% dplyr::arrange(participant)
#' df$session <- rep(c("1", "2", "3"), 20)
#'
#' # Sample rows to get unequal sizes per participant
#' df <- dplyr::sample_n(df, size = 53)
#'
#' # Create the initial groups (to be collapsed)
#' df <- fold(
#'   data = df,
#'   k = 8,
#'   method = "n_dist",
#'   id_col = "participant"
#' )
#'
#' # Ungroup the data frame
#' # Otherwise `collapse_groups*()` would be
#' # applied to each fold separately!
#' df <- dplyr::ungroup(df)
#'
#' # When `auto_tune` is enabled for larger datasets
#' # we recommend enabling parallelization
#' # This can be done with:
#' # library(doParallel)
#' # doParallel::registerDoParallel(7) # use 7 cores
#'
#' \dontrun{
#'
#' # Collapse to 3 groups with size balancing
#' # Creates new `.coll_groups` column
#' df_coll <- collapse_groups_by_size(
#'   data = df,
#'   n = 3,
#'   group_cols = ".folds"
#' )
#'
#' # Check balances
#' (coll_summary <- summarize_balances(
#'   data = df_coll,
#'   group_cols = ".coll_groups"
#' ))
#'
#' # Get ranked balances
#' # This is most useful when having created multiple
#' # new group columns with `collapse_groups()`
#' # The scores are standard deviations across groups
#' ranked_balances(coll_summary)
#'
#' # Collapse to 3 groups with *categorical* balancing
#' df_coll <- collapse_groups_by_levels(
#'   data = df,
#'   n = 3,
#'   group_cols = ".folds",
#'   cat_cols = "answer"
#' )
#'
#' # Check balances
#' (coll_summary <- summarize_balances(
#'   data = df_coll,
#'   group_cols = ".coll_groups",
#'   cat_cols = 'answer'
#' ))
#'
#' # Collapse to 3 groups with *numerical* balancing
#' # Also balance size to get similar sums
#' # as well as means
#' df_coll <- collapse_groups_by_numeric(
#'   data = df,
#'   n = 3,
#'   group_cols = ".folds",
#'   num_cols = "score",
#'   balance_size = TRUE
#' )
#'
#' # Check balances
#' (coll_summary <- summarize_balances(
#'   data = df_coll,
#'   group_cols = ".coll_groups",
#'   num_cols = 'score'
#' ))
#'
#' # Collapse to 3 groups with *ID* balancing
#' # This should give us a similar number of IDs per group
#' df_coll <- collapse_groups_by_ids(
#'   data = df,
#'   n = 3,
#'   group_cols = ".folds",
#'   id_cols = "participant"
#' )
#'
#' # Check balances
#' (coll_summary <- summarize_balances(
#'   data = df_coll,
#'   group_cols = ".coll_groups",
#'   id_cols = 'participant'
#' ))
#'
#' # Collapse to 3 groups with balancing of ALL attributes
#' # We create 5 new grouping factors and compare them
#' # The latter is in-general a good strategy even if you
#' # only need a single collapsed grouping factor
#' # as you can choose your preferred balances
#' # based on the summary
#' # NOTE: This is slow (up to a few minutes)
#' # consider enabling parallelization
#' df_coll <- collapse_groups(
#'   data = df,
#'   n = 3,
#'   num_new_group_cols = 5,
#'   group_cols = ".folds",
#'   cat_cols = "answer",
#'   num_cols = 'score',
#'   id_cols = "participant",
#'   auto_tune = TRUE   # Disabled by default in `collapse_groups()`
#'   # parallel = TRUE  # Add comma above and uncomment
#' )
#'
#' # Check balances
#' (coll_summary <- summarize_balances(
#'   data = df_coll,
#'   group_cols = paste0(".coll_groups_", 1:5),
#'   cat_cols = "answer",
#'   num_cols = 'score',
#'   id_cols = 'participant'
#' ))
#'
#' # Compare the new grouping columns
#' # The lowest across-group standard deviation
#' # is the most balanced
#' ranked_balances(coll_summary)
#'
#' }
#'
NULL


##  .................. #< 077f4d486ff8d1e493b6e0d8c6626776 ># ..................
##  Collapse by size                                                        ####


#' @rdname collapse_groups_by
#' @export
collapse_groups_by_size <- function(data,
                                    n,
                                    group_cols,
                                    auto_tune = TRUE,
                                    method = "balance",
                                    col_name = ".coll_groups",
                                    parallel = FALSE,
                                    verbose = FALSE) {
  collapse_groups(
    data = data,
    n = n,
    group_cols = group_cols,
    auto_tune = auto_tune,
    method = method,
    num_new_group_cols = 1,
    col_name = col_name,
    balance_size = TRUE,
    parallel = parallel,
    verbose = verbose
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
                                       auto_tune = TRUE,
                                       method = "balance",
                                       group_aggregation_fn = mean,
                                       col_name = ".coll_groups",
                                       parallel = FALSE,
                                       verbose = FALSE) {
  collapse_groups(
    data = data,
    n = n,
    group_cols = group_cols,
    num_cols = num_cols,
    balance_size = balance_size,
    auto_tune = auto_tune,
    method = method,
    num_new_group_cols = 1,
    group_aggregation_fn = group_aggregation_fn,
    col_name = col_name,
    parallel = parallel,
    verbose = verbose
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
                                      auto_tune = TRUE,
                                      method = "balance",
                                      col_name = ".coll_groups",
                                      parallel = FALSE,
                                      verbose = FALSE) {
  collapse_groups(
    data = data,
    n = n,
    group_cols = group_cols,
    cat_cols = cat_cols,
    cat_levels = cat_levels,
    balance_size = balance_size,
    auto_tune = auto_tune,
    method = method,
    num_new_group_cols = 1,
    col_name = col_name,
    parallel = parallel,
    verbose = verbose
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
                                   auto_tune = TRUE,
                                   method = "balance",
                                   col_name = ".coll_groups",
                                   parallel = FALSE,
                                   verbose = FALSE) {
  collapse_groups(
    data = data,
    n = n,
    group_cols = group_cols,
    id_cols = id_cols,
    balance_size = balance_size,
    auto_tune = auto_tune,
    method = method,
    col_name = col_name,
    parallel = parallel,
    verbose = verbose
  )
}
