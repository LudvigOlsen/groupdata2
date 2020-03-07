
## partition
#' @title Create balanced partitions.
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("stable")}
#'
#'  Splits data into partitions.
#'  Balances a given categorical variable and/or numerical variable between partitions and keeps (if possible)
#'  all data points with a shared ID (e.g. participant_id) in the same partition.
#' @details
#'  \subsection{cat_col}{
#'    \enumerate{
#'      \item Data is subset by \code{cat_col}.
#'      \item Subsets are partitioned and merged.
#'    }
#'  }
#'
#'  \subsection{id_col}{
#'    \enumerate{
#'      \item Partitions are created from unique IDs.
#'    }
#'  }
#'
#'  \subsection{num_col}{
#'    \enumerate{
#'      \item Rows are shuffled.
#'
#'      \strong{Note} that this will only affect rows with the same value in \code{num_col}.
#'      \item Extreme pairing 1: Rows are ordered as smallest, largest, second smallest, second largest, etc.
#'      Each pair get a group identifier.
#'      \item If \code{extreme_pairing_levels > 1}: The group identifiers are reordered as smallest,
#'      largest, second smallest, second largest, etc., by the sum of \code{num_col} in the represented rows.
#'      These pairs (of pairs) get a new set of group identifiers, and the process is repeated
#'       \code{extreme_pairing_levels-2} times. Note that the group identifiers at the last level will represent
#'       \code{2^extreme_pairing_levels} rows, why you should be careful when choosing that setting.
#'      \item The final group identifiers are shuffled, and their order is applied to the full dataset.
#'      \item The ordered dataset is split by the sizes in \code{p}.
#'    }
#'
#'  N.B. When doing extreme pairing of an unequal number of rows,
#'  the row with the largest value is placed in a group by itself, and the order is instead:
#'  smallest, second largest, second smallest, third largest, ... , largest.
#'  }
#'
#'  \subsection{cat_col AND id_col}{
#'    \enumerate{
#'      \item Data is subset by \code{cat_col}.
#'      \item Partitions are created from unique IDs in each subset.
#'      \item Subsets are merged.
#'    }
#'  }
#'
#'  \subsection{cat_col AND num_col}{
#'    \enumerate{
#'      \item Data is subset by \code{cat_col}.
#'      \item Subsets are partitioned by \code{num_col}.
#'      \item Subsets are merged.
#'    }
#'  }
#'
#'  \subsection{num_col AND id_col}{
#'    \enumerate{
#'      \item Values in \code{num_col} are aggregated for each ID, using \code{id_aggregation_fn}.
#'      \item The IDs are partitioned, using the aggregated values as "\code{num_col}".
#'      \item The partition identifiers are transferred to the rows of the IDs.
#'    }
#'  }
#'
#'  \subsection{cat_col AND num_col AND id_col}{
#'    \enumerate{
#'      \item Values in \code{num_col} are aggregated for each ID, using \code{id_aggregation_fn}.
#'      \item IDs are subset by \code{cat_col}.
#'      \item The IDs for each subset are partitioned,
#'      by using the aggregated values as "\code{num_col}".
#'      \item The partition identifiers are transferred to the rows of the IDs.
#'    }
#'  }
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family grouping functions
#' @param data Data frame.
#' @param p List or vector of partition sizes.
#'  Given as whole number(s) and/or percentage(s) (\code{0} < \code{n} < \code{1}).
#'  E.g. \eqn{c(0.2, 3, 0.1)}.
#' @param cat_col Name of categorical variable to balance between partitions.
#'
#'  E.g. when training and testing a model for predicting a binary variable (a or b),
#'  we usually want both classes represented in both the training set and the test set.
#'
#'  N.B. If also passing an \code{id_col}, \code{cat_col} should be constant within each ID.
#' @param num_col Name of numerical variable to balance between partitions.
#'
#'  N.B. When used with \code{id_col}, values in \code{num_col} for each ID are
#'  aggregated using \code{id_aggregation_fn} before being balanced.
#' @param id_col Name of factor with IDs. Used to keep all rows that share an ID in
#'  the same partition (if possible).
#'
#'  E.g. If we have measured a participant multiple times and want to see the
#'  effect of time, we want to have all observations of this participant in
#'  the same partition.
#' @param id_aggregation_fn Function for aggregating values in \code{num_col} for each ID,
#'  before balancing \code{num_col}.
#'
#'  N.B. Only used when \code{num_col} and \code{id_col} are both specified.
#' @param extreme_pairing_levels How many levels of extreme pairing to do
#'  when balancing partitions by a numerical column (i.e. \code{num_col} is specified).
#'
#'  \strong{Extreme pairing}: Rows/pairs are ordered as smallest, largest,
#'  second smallest, second largest, etc. If \code{extreme_pairing_levels > 1},
#'  this is done "recursively" on the extreme pairs. See \code{"Details/num_col"} for more.
#'
#'  N.B. Larger values work best with large datasets. If set too high,
#'  the result might not be stochastic. Always check if an increase
#'  actually makes the partitions more balanced. See example.
#' @param list_out Return partitions in a list. (Logical)
#' @param force_equal Discard excess data. (Logical)
#' @return If \code{list_out is TRUE}:
#'
#' A list of partitions where partitions are data frames.
#'
#' If \code{list_out is FALSE}:
#'
#' A data frame with grouping factor for subsetting.
#' @examples
#' # Attach packages
#' library(groupdata2)
#' library(dplyr)
#'
#' # Create data frame
#' df <- data.frame(
#'   "participant" = factor(rep(c("1", "2", "3", "4", "5", "6"), 3)),
#'   "age" = rep(sample(c(1:100), 6), 3),
#'   "diagnosis" = factor(rep(c("a", "b", "a", "a", "b", "b"), 3)),
#'   "score" = sample(c(1:100), 3 * 6)
#' )
#' df <- df %>% arrange(participant)
#' df$session <- rep(c("1", "2", "3"), 6)
#'
#' # Using partition()
#'
#' # Without balancing
#' partitions <- partition(data = df, p = c(0.2, 0.3))
#'
#' # With cat_col
#' partitions <- partition(data = df, p = 0.5, cat_col = "diagnosis")
#'
#' # With id_col
#' partitions <- partition(data = df, p = 0.5, id_col = "participant")
#'
#' # With num_col
#' partitions <- partition(data = df, p = 0.5, num_col = "score")
#'
#' # With cat_col and id_col
#' partitions <- partition(
#'   data = df,
#'   p = 0.5,
#'   cat_col = "diagnosis",
#'   id_col = "participant"
#' )
#'
#' # With cat_col, num_col and id_col
#' partitions <- partition(
#'   data = df,
#'   p = 0.5,
#'   cat_col = "diagnosis",
#'   num_col = "score",
#'   id_col = "participant"
#' )
#'
#' # Return data frame with grouping factor
#' # with list_out = FALSE
#' partitions <- partition(df, c(0.5), list_out = FALSE)
#'
#' # Check if additional extreme_pairing_levels
#' # improve the numerical balance
#' set.seed(2) # try with seed 1 as well
#' partitions_1 <- partition(
#'   data = df,
#'   p = 0.5,
#'   num_col = "score",
#'   extreme_pairing_levels = 1,
#'   list_out = FALSE
#' )
#' partitions_1 %>%
#'   dplyr::group_by(.partitions) %>%
#'   dplyr::summarise(
#'     sum_score = sum(score),
#'     mean_score = mean(score)
#'   )
#' set.seed(2) # try with seed 1 as well
#' partitions_2 <- partition(
#'   data = df,
#'   p = 0.5,
#'   num_col = "score",
#'   extreme_pairing_levels = 2,
#'   list_out = FALSE
#' )
#' partitions_2 %>%
#'   dplyr::group_by(.partitions) %>%
#'   dplyr::summarise(
#'     sum_score = sum(score),
#'     mean_score = mean(score)
#'   )
#' @importFrom dplyr group_by do %>%
partition <- function(data,
                      p = 0.2,
                      cat_col = NULL,
                      num_col = NULL,
                      id_col = NULL,
                      id_aggregation_fn = sum,
                      extreme_pairing_levels = 1,
                      force_equal = FALSE,
                      list_out = TRUE) {

  #
  # Balanced partitioning
  # data: data frame or vector
  # p: list of partitions given as percentage (0-1) or group sizes (wholenumber)
  # cat_col: Categorical variable to balance by
  # num_col: Numerical variable to balance by
  # id_col: ID column to keep rows with shared IDs in the same partition
  # force_equal: Whether you only want the inputted partitions or the exceeding values gets a partition (logical)
  #        FALSE allows you to pass "p = 0.2" and get 2 partitions - 0.2 and 0.8
  #

  check_partition(
    data = data,
    p = p,
    cat_col = cat_col,
    num_col = num_col,
    id_col = id_col,
    id_aggregation_fn = id_aggregation_fn,
    extreme_pairing_levels = extreme_pairing_levels,
    force_equal = force_equal,
    list_out = list_out
  )

  # If num_col is not NULL
  if (!is.null(num_col)) {
    data <- create_num_col_groups(
      data = data,
      n = p,
      num_col = num_col,
      cat_col = cat_col,
      id_col = id_col,
      col_name = ".partitions",
      id_aggregation_fn = id_aggregation_fn,
      extreme_pairing_levels = extreme_pairing_levels,
      method = "l_sizes",
      unequal_method = "last",
      force_equal = force_equal,
      pre_randomize = TRUE
    )
  } else {

    # If cat_col is not NULL
    if (!is.null(cat_col)) {

      # If id_col is not NULL
      if (!is.null(id_col)) {

        # Group by cat_col
        # For each group:
        # .. create groups of the unique IDs (e.g. subjects)
        # .. add grouping factor to data
        # Group by new grouping factor '.partitions'

        data <- data %>%
          group_by(!!as.name(cat_col)) %>%
          do(group_uniques_(
            data = .,
            n = p,
            id_col,
            method = "l_sizes",
            col_name = ".partitions",
            force_equal = force_equal
          )) %>%
          group_by(!!as.name(".partitions"))

        # If id_col is NULL
      } else {

        # Group by cat_col
        # Create groups from data
        # .. and add grouping factor to data

        data <- data %>%
          group_by(!!as.name(cat_col)) %>%
          do(group(
            data = .,
            n = p,
            method = "l_sizes",
            randomize = TRUE,
            col_name = ".partitions",
            force_equal = force_equal
          ))
      }

      # If cat_col is NULL
    } else {

      # If id_col is not NULL
      if (!is.null(id_col)) {

        # Create groups of unique IDs
        # .. and add grouping factor to data

        data <- data %>%
          group_uniques_(
            n = p,
            id_col = id_col,
            method = "l_sizes",
            col_name = ".partitions",
            force_equal = force_equal
          )

        # If id_col is NULL
      } else {


        # Create groups from all the data points
        # .. and add grouping factor to data

        data <- group(
          data = data,
          n = p,
          method = "l_sizes",
          randomize = TRUE,
          col_name = ".partitions",
          force_equal = force_equal
        )
      }
    }
  }

  if (isTRUE(list_out)) {

    # If we have any NAs in .partitions
    if (anyNA(data$.partitions)) {
      stop("NA in .partitions column.")
    } else if (is.null(data[[".partitions"]])) {
      stop("Column .partitions does not exist")
    } else {
      return(
        plyr::llply(c(1:max(as.integer(data[[".partitions"]]))), function(part) {
          temp_data <- data[data$.partitions == part,]

          temp_data$.partitions <- NULL

          temp_data %>%
            dplyr::ungroup()
      })
      )
    }
  }

  data
}


check_partition <- function(data,
                            p,
                            cat_col,
                            num_col,
                            id_col,
                            id_aggregation_fn,
                            extreme_pairing_levels,
                            force_equal,
                            list_out){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, min.rows = 1, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_numeric(x = p, lower = 1/nrow(data), upper = nrow(data), any.missing = FALSE,
                            finite = TRUE, add = assert_collection)
  checkmate::assert_count(x = extreme_pairing_levels, positive = TRUE, add = assert_collection)
  checkmate::assert_character(x = cat_col, min.len = 1, any.missing = FALSE,
                              null.ok = TRUE, unique = TRUE,
                              names = "unnamed", add = assert_collection)
  checkmate::assert_string(x = num_col, na.ok = FALSE, min.chars = 1,
                           null.ok = TRUE,  add = assert_collection)
  checkmate::assert_string(x = id_col, na.ok = FALSE, min.chars = 1,
                           null.ok = TRUE,  add = assert_collection)
  checkmate::assert_function(x = id_aggregation_fn, add = assert_collection)
  checkmate::assert_flag(x = force_equal, add = assert_collection)
  checkmate::assert_flag(x = list_out, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # TODO Could have a helper for adding this kind of message:
  if (!is.null(cat_col) && length(setdiff(cat_col, colnames(data))) != 0){
    assert_collection$push(paste0("'cat_col' column(s), '",
                                  paste0(setdiff(cat_col, colnames(data)), collapse = ", "),
                                  "', not found in 'data'."))
  }
  if (!is.null(num_col)){
    if (num_col %ni% colnames(data)){
      assert_collection$push(paste0("'num_col' column, '", num_col, "', not found in 'data'."))
    }
    checkmate::reportAssertions(assert_collection)
    if (!checkmate::test_numeric(data[[num_col]])){
      assert_collection$push(paste0("'num_col' column must be numeric."))
    }
  }
  if (!is.null(id_col)) {
    if (id_col %ni% colnames(data)){
      assert_collection$push(paste0("'id_col' column, '", id_col, "', not found in 'data'."))
    }
    checkmate::reportAssertions(assert_collection)
    checkmate::assert_factor(x = data[[id_col]], add = assert_collection)
    if (!is.null(cat_col)) {
      if (id_col %in% cat_col) {
        assert_collection$push("'id_col' and 'cat_col' cannot contain the same column name.")
      }
      # Check that cat_col is constant within each ID
      # Note: I tested and count() is faster than group_keys()
      counts <- dplyr::count(data, !!as.name(id_col), !!as.name(cat_col))
      if (nrow(counts) != length(unique(counts[[id_col]]))) {
        assert_collection$push("The value in 'data[[cat_col]]' must be constant within each ID.")
      }
    }
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

}

