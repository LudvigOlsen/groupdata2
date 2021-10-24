

#   __________________ #< 7adc6f1503373fe7794e7b31b7dea650 ># __________________
#   Extract ranked balances                                                 ####


#' @title Extract ranked standard deviations from summary
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Extract the standard deviations (default) from the \code{"Summary" data.frame}
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
#' @param measure The measure to extract rows for. One of:
#'  \code{"mean", "median", "SD", "IQR", "min", "max"}.
#'
#'  The most meaningful measures to consider as metrics of balance are \code{`SD`} and \code{`IQR`},
#'  as a smaller spread of variables across group summaries means they are more similar.
#'
#'  \strong{NOTE}: Ranks are of standard deviations and not affected by this argument.
#' @family summarization functions
#' @return The rows in \code{`summary`} where \code{`measure` == "SD"},
#'  ordered by the \code{`SD_rank`} column.
ranked_balances <- function(summary, measure = "SD") {

  if (is.list(summary) &&
      all(c("Groups", "Summary") %in% names(summary))) {
    summary <- summary[["Summary"]]
  }

  available_measures <- c("mean", "median", "SD", "IQR", "min", "max")

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = summary, add = assert_collection)
  checkmate::assert_string(measure, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(
    x = colnames(summary),
    must.include = c("measure"),
    add = assert_collection
  )
  checkmate::assert_names(
    measure,
    subset.of = available_measures,
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Rename variable to avoid confusion with column name
  measure_ <- measure
  measure <- NULL

  # Extract rows with standard deviations
  measure_rows <- summary %>%
    dplyr::filter(.data$measure == measure_)

  if (nrow(measure_rows) > 1){
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
    measure_rows <- dplyr::arrange(
      measure_rows,
      !!!rlang::syms(arrange_by)
    )
  }

  measure_rows

}
