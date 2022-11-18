

#   __________________ #< 607ec1b0fdc4958652e477eba6d39dfb ># __________________
#   Summarize group columns                                                 ####


#' @title Summarize group columns
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Get the following summary statistics for each group column:
#'   1) Number of groups
#'   2) Mean, median, std., IQR, min, and max number of rows per group.
#'
#'  The output can be given in either \emph{long} (default) or \emph{wide} format.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @family summarization functions
#' @param data \code{data.frame} with one or more group columns (\code{factor}s) to summarize.
#' @param group_cols Names of columns to summarize. These columns must be factors in \code{`data`}.
#' @param long Whether the output should be in \emph{long} or \emph{wide} format.
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create data frame
#' df <- data.frame(
#'   "some_var" = runif(25),
#'   "grp_1" = factor(sample(1:5, size = 25, replace=TRUE)),
#'   "grp_2" = factor(sample(1:8, size = 25, replace=TRUE)),
#'   "grp_3" = factor(sample(LETTERS[1:3], size = 25, replace=TRUE)),
#'   "grp_4" = factor(sample(LETTERS[1:12], size = 25, replace=TRUE))
#' )
#'
#' # Summarize the group columns (long format)
#' summarize_group_cols(
#'   data = df,
#'   group_cols = paste0("grp_", 1:4),
#'   long = TRUE
#'  )
#'
#' # Summarize the group columns (wide format)
#' summarize_group_cols(
#'   data = df,
#'   group_cols = paste0("grp_", 1:4),
#'   long = FALSE
#'  )
#' @export
#' @return
#'  Data frame (\code{tibble}) with summary statistics for each column in \code{`group_cols`}.
#' @importFrom stats IQR median sd quantile
summarize_group_cols <- function(data, group_cols, long = TRUE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, min.cols = 1, add = assert_collection)
  checkmate::assert_character(group_cols, min.chars = 1, any.missing = FALSE,
                              min.len = 1, add = assert_collection)
  checkmate::assert_flag(long, na.ok = FALSE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(colnames(data), must.include = group_cols)
  checkmate::reportAssertions(assert_collection)

  # Select the group columns only
  data <- data[, group_cols, drop = FALSE]

  # Check the group columns only
  checkmate::assert_data_frame(data, types = "factor", any.missing = FALSE,
                               min.cols = 1, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # The summarization functions to apply
  # This is where the output is named as well
  fns <- list(
    "Num Groups" = function(x)
      length(unique(x)),
    "Mean Rows" = function(x)
      mean(table(x)),
    "Median Rows" = function(x)
      median(table(x)),
    "Std Rows" = function(x)
      sd(table(x)),
    "IQR Rows" = function(x)
      IQR(table(x)),
    "Min Rows" = function(x)
      min(table(x)),
    "Max Rows" = function(x)
      max(table(x))
  )

  # Apply summary functions to each group column
  summaries <- plyr::ldply(fns, function(fn) {
    data %>%
      dplyr::summarise_all(.funs = fn)
  }) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(Measure = ".id")

  if (isTRUE(long)) {
    # Convert to long format
    summaries <- summaries %>%
      tidyr::gather(key = "Group Column", value = "Value", -"Measure") %>%
      tidyr::spread(.data$Measure, .data$Value) %>%
      dplyr::select(dplyr::one_of("Group Column", names(fns)))

    # Order by the original `group_cols` order
    order_df <- tibble::tibble("Group Column" = group_cols,
                               idx = seq_along(group_cols))
    summaries <- summaries %>%
      dplyr::left_join(order_df, by = "Group Column") %>%
      dplyr::arrange(.data$idx) %>%
      dplyr::select(-"idx")
  }

  summaries

}
