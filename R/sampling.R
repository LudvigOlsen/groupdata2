



## downsample
#' @title Downsampling of rows in a dataframe.
#' @description Uses random downsampling to fix the group sizes to the
#'  smallest group in the data frame.
#'
#'  Wraps \code{\link{balance}()}.
#' @details
#' \subsection{Without id_col}{
#' Downsampling is done without replacement, meaning that rows are not duplicated but only removed.}
#' \subsection{With id_col}{See id_method description.}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @inheritParams balance
#' @family sampling functions
#' @return Dataframe with some rows removed. Ordered by cat_col and (potentially) id_col.
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create dataframe
#' df <- data.frame(
#'   "participant" = factor(c(1, 1, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5)),
#'   "diagnosis" = factor(c(0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0)),
#'   "trial" = c(1, 2, 1, 1, 2, 3, 4, 1, 2, 1, 2, 3, 4),
#'   "score" = sample(c(1:100), 13)
#' )
#'
#' # Using downsample()
#' downsample(df, cat_col="diagnosis")
#'
#' # Using downsample() with id_method "n_ids"
#' # With column specifying added rows
#' downsample(df, cat_col="diagnosis",
#'         id_col="participant", id_method="n_ids",
#'         mark_new_rows = TRUE)
#'
#' # Using downsample() with id_method "n_rows_c"
#' # With column specifying added rows
#' downsample(df, cat_col="diagnosis",
#'         id_col="participant", id_method="n_rows_c",
#'         mark_new_rows = TRUE)
#'
#' # Using downsample() with id_method "distributed"
#' downsample(df, cat_col="diagnosis",
#'         id_col="participant",
#'         id_method="distributed")
#'
#' # Using downsample() with id_method "nested"
#' downsample(df, cat_col="diagnosis",
#'         id_col="participant",
#'         id_method="nested")
#'
downsample <- function(data,
                       cat_col,
                       id_col = NULL,
                       id_method = "n_ids") {
  balance(data,
          size="min",
          cat_col=cat_col,
          id_col=id_col,
          id_method=id_method)

}

## upsample
#' @title Upsampling of rows in a dataframe.
#' @description Uses random upsampling to fix the group sizes to the
#'  largest group in the data frame.
#'
#'  Wraps \code{\link{balance}()}.
#' @details
#' \subsection{Without id_col}{
#' Upsampling is done with replacement for added rows, while the original data remains intact.}
#' \subsection{With id_col}{See id_method description.}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @inheritParams balance
#' @family sampling functions
#' @return Dataframe with added rows. Ordered by cat_col and (potentially) id_col.
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create dataframe
#' df <- data.frame(
#'   "participant" = factor(c(1, 1, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5)),
#'   "diagnosis" = factor(c(0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0)),
#'   "trial" = c(1, 2, 1, 1, 2, 3, 4, 1, 2, 1, 2, 3, 4),
#'   "score" = sample(c(1:100), 13)
#' )
#'
#' # Using upsample()
#' upsample(df, cat_col="diagnosis")
#'
#' # Using upsample() with id_method "n_ids"
#' # With column specifying added rows
#' upsample(df, cat_col="diagnosis",
#'         id_col="participant", id_method="n_ids",
#'         mark_new_rows = TRUE)
#'
#' # Using upsample() with id_method "n_rows_c"
#' # With column specifying added rows
#' upsample(df, cat_col="diagnosis",
#'         id_col="participant", id_method="n_rows_c",
#'         mark_new_rows = TRUE)
#'
#' # Using upsample() with id_method "distributed"
#' # With column specifying added rows
#' upsample(df, cat_col="diagnosis",
#'         id_col="participant", id_method="distributed",
#'         mark_new_rows = TRUE)
#'
#' # Using upsample() with id_method "nested"
#' # With column specifying added rows
#' upsample(df, cat_col="diagnosis",
#'         id_col="participant", id_method="nested",
#'         mark_new_rows = TRUE)
upsample <- function(data,
                     cat_col,
                     id_col = NULL,
                     id_method = "n_ids",
                     mark_new_rows = FALSE,
                     new_rows_col_name = ".new_row") {
  balance(data,
          size="max",
          cat_col=cat_col,
          id_col=id_col,
          id_method=id_method,
          mark_new_rows=mark_new_rows,
          new_rows_col_name=)
}

## balance
#' @title Balance groups by up- or downsampling.
#' @description Uses up- or downsampling to fix the group size to the
#'  min, max, mean, or median group size or to a specific number of rows. Allows to balance on ID level.
#' @details
#' \subsection{Without id_col}{Upsampling is done with replacement for added rows, while the original data remains intact.
#' Downsampling is done without replacement, meaning that rows are not duplicated but only removed.}
#' \subsection{With id_col}{See id_method description.}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @param data Dataframe.
#' @param size Size to fix group sizes to.
#'  Can be a specific number, given as a whole number, or one of the following strings:
#'  \code{"min"}, \code{"max"}, \code{"mean"}, \code{"median"}.
#'
#'  \subsection{number}{
#'  Fix each group to have the size of the specified number of row.
#'  Uses downsampling for groups with too many rows and upsampling for groups with too few rows.
#'  }
#'  \subsection{min}{
#'  Fix each group to have the size of smallest group in the dataset.
#'  Uses downsampling on all groups that have too many rows.
#'  }
#'  \subsection{max}{
#'  Fix each group to have the size of largest group in the dataset.
#'  Uses upsampling on all groups that have too few rows.
#'  }
#'  \subsection{mean}{
#'  Fix each group to have the mean group size in the dataset. The mean is rounded.
#'  Uses downsampling for groups with too many rows and upsampling for groups with too few rows.
#'  }
#'  \subsection{median}{
#'  Fix each group to have the median group size in the dataset. The median is rounded.
#'  Uses downsampling for groups with too many rows and upsampling for groups with too few rows.
#'  }
#' @param cat_col Name of categorical variable to balance by. (Character)
#' @param id_col Name of factor with IDs. (Character)
#'
#'  IDs are considered entities, e.g. allowing us to add or remove all rows for an ID.
#'  How this is used is up to the \code{id_method}.
#'
#'  E.g. If we have measured a participant multiple times and
#'  want make sure that we keep all these measurements. Then we would either
#'  remove/add all measurements for the participant or leave in
#'  all measurements for the participant.
#' @param id_method Method for balancing the IDs. (Character)
#'
#'  \code{n_ids}, \code{n_rows_c}, \code{distributed}, or \code{nested}.
#'  \subsection{n_ids}{
#'  Balances on ID level only. It makes sure there are the same number of IDs for each category.
#'  This might lead to a different number of rows between categories.
#'  }
#'  \subsection{n_rows_c}{
#'  Attempts to level the number of rows per category, while only removing/adding entire IDs.
#'  This is done in 2 steps:
#'  \enumerate{
#'  \item If a category needs to add all its rows one or more times, the data is repeated.
#'  \item Iteratively, the ID with the number of rows closest to the
#'     lacking/excessive number of rows is added/removed.
#'     This happens until adding/removing the closest ID would lead to a size further from
#'     the target size than the current size.
#'     If multiple IDs are closest, one is randomly sampled.
#'     }
#'  }
#'  \subsection{distributed}{
#'  Distributes the lacking/excess rows equally between the IDs.
#'  If the number to distribute can not be equally divided, some IDs will have 1 row more/less than the others.
#'  }
#'  \subsection{nested}{
#'  Calls balance() on each category with IDs as cat_col.
#'
#'  I.e. if size is "min", IDs will have the size of the smallest ID in their category.
#'  }
#' @param mark_new_rows Add column with 1s for added rows, and 0s for original rows. (Logical)
#' @param new_rows_col_name Name of column marking new rows. Defaults to ".new_row".
#' @family sampling functions
#' @return Dataframe with added and/or deleted rows. Ordered by cat_col and (potentially) id_col.
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create dataframe
#' df <- data.frame(
#'   "participant" = factor(c(1, 1, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5)),
#'   "diagnosis" = factor(c(0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0)),
#'   "trial" = c(1, 2, 1, 1, 2, 3, 4, 1, 2, 1, 2, 3, 4),
#'   "score" = sample(c(1:100), 13)
#' )
#'
#' # Using balance() with specific number of rows
#' balance(df, 3, cat_col="diagnosis")
#'
#' # Using balance() with min
#' balance(df, "min", cat_col="diagnosis")
#'
#' # Using balance() with max
#' balance(df, "max", cat_col="diagnosis")
#'
#' # Using balance() with id_method "n_ids"
#' # With column specifying added rows
#' balance(df, "max", cat_col="diagnosis",
#'         id_col="participant", id_method="n_ids",
#'         mark_new_rows = TRUE)
#'
#' # Using balance() with id_method "n_rows_c"
#' # With column specifying added rows
#' balance(df, "max", cat_col="diagnosis",
#'         id_col="participant", id_method="n_rows_c",
#'         mark_new_rows = TRUE)
#'
#' # Using balance() with id_method "distributed"
#' # With column specifying added rows
#' balance(df, "max", cat_col="diagnosis",
#'         id_col="participant", id_method="distributed",
#'         mark_new_rows = TRUE)
#'
#' # Using balance() with id_method "nested"
#' # With column specifying added rows
#' balance(df, "max", cat_col="diagnosis",
#'         id_col="participant", id_method="nested",
#'         mark_new_rows = TRUE)
#'
#' @importFrom dplyr filter sample_n %>%
balance <- function(data,
                    size,
                    cat_col,
                    id_col = NULL,
                    id_method = "n_ids",
                    # replace = TRUE, # TODO Some times we want to choose between replacement or repetition
                    mark_new_rows = FALSE,
                    new_rows_col_name = ".new_row") {
  if (is.character(size)) {
    if (size %ni% c("min", "max", "mean", "median")) {
      stop("'size' must be one of 'min','max','mean','median' or a whole number.")
    }
  } else {
    if (!arg_is_wholenumber_(size)) {
      stop("'size' must be one of 'min','max','mean','median' or a whole number.")
    }
  }
  if (!is.character(cat_col)) {
    stop("'cat_col' must be the name of a column in data.")
  }
  if (!is.null(id_col)){
    if (! is.factor(data[[id_col]])){
      stop("'id_col' must be a factor.")
    }
  }

  stopifnot(id_method %in% c("n_ids",
                             "n_rows_c",
                             # TODO "n_rows_o"
                             # Should find the optimal combinations of IDs.
                             # E.g. using dynamic programming.
                             # "n_rows_o",
                             "distributed",
                             "nested")) # find more

  # mark_new_rows : should add a binary column with 1 for the additions,
  # so people can manipulate them separately

  # Add .new_row column
  data$.TempNewRow <- 0

  if (!is.null(id_col)) {

    if (id_method == "n_ids") {
      balanced <- id_method_n_ids_(
        data = data,
        size = size,
        cat_col = cat_col,
        id_col = id_col,
        mark_new_rows = mark_new_rows
      )}
    else if (id_method == "n_rows_c") {
        balanced <- id_method_n_rows_closest(
          data = data,
          size = size,
          cat_col = cat_col,
          id_col = id_col,
          mark_new_rows = mark_new_rows
        )}
    else if (id_method == "distributed") {
      balanced <- id_method_distributed(
        data = data,
        size = size,
        cat_col = cat_col,
        id_col = id_col,
        mark_new_rows = mark_new_rows
      )}
    else if (id_method == "nested") {
      balanced <- id_method_nested(
        data = data,
        size = size,
        cat_col = cat_col,
        id_col = id_col,
        mark_new_rows = mark_new_rows
      )}
  } else {

    to_size <- get_target_size(data, size, cat_col)
    balanced <- plyr::ldply(unique(data[[cat_col]]), function(category) {
        data_for_cat <- data %>%
          filter(!!as.name(cat_col) == category)

        n_rows <- nrow(data_for_cat)

        if (n_rows == to_size) {
          return(data_for_cat)
        } else if (n_rows < to_size) {
          return(add_rows_with_sampling(data_for_cat, to_size = to_size))
        } else {
          data_for_cat %>%
            sample_n(size = to_size, replace = FALSE)
        }
      })

  }

  if (!isTRUE(mark_new_rows)) {
    balanced$.TempNewRow <- NULL
  } else {
    # Replace temporary column name with passed column name
    # e.g. '.new_row'
    balanced <- replace_col_name(balanced, '.TempNewRow', new_rows_col_name)
  }

  if (!is.null(id_col)) {
  balanced %>%
    dplyr::arrange(!! as.name(cat_col), !! as.name(id_col))
  } else {
    balanced %>%
      dplyr::arrange(!! as.name(cat_col))
  }

}
