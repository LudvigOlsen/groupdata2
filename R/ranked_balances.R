

#   __________________ #< 7adc6f1503373fe7794e7b31b7dea650 ># __________________
#   Extract ranked balances                                                 ####


#' @title Extract ranked standard deviations from summary
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Extract the standard deviations from the \code{"Summary" data.frame}
#'  from the output of \code{\link[groupdata2:summarize_balances]{summarize_balances()}},
#'  ordered by the \code{`SD_rank`} column.
#'
#'  See examples of usage in
#'  \code{\link[groupdata2:summarize_balances]{summarize_balances()}}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param summary \code{"Summary" data.frame} from output of
#'  \code{\link[groupdata2:summarize_balances]{summarize_balances()}}.
#'
#'  Can also be the direct output list of
#'  \code{\link[groupdata2:summarize_balances]{summarize_balances()}}, in which case
#'  the \code{"Summary"} element is used.
#' @family summarization functions
#' @return The rows in \code{`summary`} where \code{`measure` == "SD"},
#'  ordered by the \code{`SD_rank`} column.
ranked_balances <- function(summary){

  if (is.list(summary) &&
      all(c("Group", "Summary") %in% names(summary))) {
    summary <- summary[["Summary"]]
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = summary, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(
    x = colnames(summary),
    must.include = c("measure", "SD_rank"),
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  arrange_by <- c(dplyr::group_vars(summary), "SD_rank")

  summary %>%
    dplyr::filter(measure == "SD") %>%
    dplyr::arrange(!!!rlang::syms(arrange_by))
}
