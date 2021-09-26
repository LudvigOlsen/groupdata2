

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
      all(c("Groups", "Summary") %in% names(summary))) {
    summary <- summary[["Summary"]]
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = summary, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(
    x = colnames(summary),
    must.include = c("measure"),
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Extract rows with standard deviations
  sd_rows <- summary %>%
    dplyr::filter(.data$measure == "SD")

  if (nrow(sd_rows) > 1){
    if ("SD_rank" %ni% colnames(summary)){
      assert_collection$push(
        paste0(
          "`summary` must include the `SD_rank` column when containing more ",
          "than one group column."
        )
      )
      checkmate::reportAssertions(assert_collection)
    }
    arrange_by <- c(dplyr::group_vars(summary), "SD_rank")
    sd_rows <- dplyr::arrange(sd_rows, !!!rlang::syms(arrange_by))
  }

  sd_rows

}
