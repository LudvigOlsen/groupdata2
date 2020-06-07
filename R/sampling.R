
## downsample
#' @title Downsampling of rows in a data frame
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  Uses random downsampling to fix the group sizes to the
#'  smallest group in the \code{data.frame}.
#'
#'  Wraps \code{\link{balance}()}.
#' @details
#'  \subsection{Without \code{`id_col`}}{
#'  Downsampling is done without replacement, meaning that rows are not duplicated but only removed.}
#'  \subsection{With \code{`id_col`}}{See \code{`id_method`} description.}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @inheritParams balance
#' @family sampling functions
#' @return \code{data.frame} with some rows removed.
#'  Ordered by \code{`cat_col`} and (potentially) \code{`id_col`}.
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create data frame
#' df <- data.frame(
#'   "participant" = factor(c(1, 1, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5)),
#'   "diagnosis" = factor(c(0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0)),
#'   "trial" = c(1, 2, 1, 1, 2, 3, 4, 1, 2, 1, 2, 3, 4),
#'   "score" = sample(c(1:100), 13)
#' )
#'
#' # Using downsample()
#' downsample(df, cat_col = "diagnosis")
#'
#' # Using downsample() with id_method "n_ids"
#' # With column specifying added rows
#' downsample(df,
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   id_method = "n_ids"
#' )
#'
#' # Using downsample() with id_method "n_rows_c"
#' # With column specifying added rows
#' downsample(df,
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   id_method = "n_rows_c"
#' )
#'
#' # Using downsample() with id_method "distributed"
#' downsample(df,
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   id_method = "distributed"
#' )
#'
#' # Using downsample() with id_method "nested"
#' downsample(df,
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   id_method = "nested"
#' )
downsample <- function(data,
                       cat_col,
                       id_col = NULL,
                       id_method = "n_ids") {
  balance(
    data = data,
    size = "min",
    cat_col = cat_col,
    id_col = id_col,
    id_method = id_method
  )
}

## upsample
#' @title Upsampling of rows in a data frame
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  Uses random upsampling to fix the group sizes to the
#'  largest group in the data frame.
#'
#'  Wraps \code{\link{balance}()}.
#' @details
#'  \subsection{Without \code{`id_col`}}{
#'  Upsampling is done with replacement for added rows, while the original data remains intact.}
#'  \subsection{With \code{`id_col`}}{See \code{`id_method`} description.}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @inheritParams balance
#' @family sampling functions
#' @return \code{data.frame} with added rows. Ordered by \code{`cat_col`} and (potentially) \code{`id_col`}.
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create data frame
#' df <- data.frame(
#'   "participant" = factor(c(1, 1, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5)),
#'   "diagnosis" = factor(c(0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0)),
#'   "trial" = c(1, 2, 1, 1, 2, 3, 4, 1, 2, 1, 2, 3, 4),
#'   "score" = sample(c(1:100), 13)
#' )
#'
#' # Using upsample()
#' upsample(df, cat_col = "diagnosis")
#'
#' # Using upsample() with id_method "n_ids"
#' # With column specifying added rows
#' upsample(df,
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   id_method = "n_ids",
#'   mark_new_rows = TRUE
#' )
#'
#' # Using upsample() with id_method "n_rows_c"
#' # With column specifying added rows
#' upsample(df,
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   id_method = "n_rows_c",
#'   mark_new_rows = TRUE
#' )
#'
#' # Using upsample() with id_method "distributed"
#' # With column specifying added rows
#' upsample(df,
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   id_method = "distributed",
#'   mark_new_rows = TRUE
#' )
#'
#' # Using upsample() with id_method "nested"
#' # With column specifying added rows
#' upsample(df,
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   id_method = "nested",
#'   mark_new_rows = TRUE
#' )
upsample <- function(data,
                     cat_col,
                     id_col = NULL,
                     id_method = "n_ids",
                     mark_new_rows = FALSE,
                     new_rows_col_name = ".new_row") {
  balance(
    data = data,
    size = "max",
    cat_col = cat_col,
    id_col = id_col,
    id_method = id_method,
    mark_new_rows = mark_new_rows,
    new_rows_col_name = new_rows_col_name
  )
}

## balance
#' @title Balance groups by up- and downsampling
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  Uses up- and/or downsampling to fix the group sizes to the
#'  \code{min}, \code{max}, \code{mean}, or \code{median} group size or
#'  to a specific number of rows. Has a range of methods for balancing on
#'  ID level.
#' @details
#' \subsection{Without \code{`id_col`}}{Upsampling is done with replacement for added rows,
#'  while the original data remains intact.
#'  Downsampling is done without replacement, meaning that rows are not duplicated but only removed.}
#' \subsection{With \code{`id_col`}}{See \code{`id_method`} description.}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param data \code{data.frame}.
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
#'  How this is used is up to the \code{`id_method`}.
#'
#'  E.g. If we have measured a participant multiple times and
#'  want make sure that we keep all these measurements. Then we would either
#'  remove/add all measurements for the participant or leave in
#'  all measurements for the participant.
#' @param id_method Method for balancing the IDs. (Character)
#'
#'  \code{"n_ids"}, \code{"n_rows_c"}, \code{"distributed"}, or \code{"nested"}.
#'  \subsection{n_ids (default)}{
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
#'  Calls \code{balance()} on each category with IDs as cat_col.
#'
#'  I.e. if size is \code{"min"}, IDs will have the size of the smallest ID in their category.
#'  }
#' @param mark_new_rows Add column with \code{1}s for added rows, and \code{0}s for original rows. (Logical)
#' @param new_rows_col_name Name of column marking new rows. Defaults to \code{".new_row"}.
#' @family sampling functions
#' @return \code{data.frame} with added and/or deleted rows.
#'  Ordered by \code{`cat_col`} and (potentially) \code{`id_col`}.
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create data frame
#' df <- data.frame(
#'   "participant" = factor(c(1, 1, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5)),
#'   "diagnosis" = factor(c(0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0)),
#'   "trial" = c(1, 2, 1, 1, 2, 3, 4, 1, 2, 1, 2, 3, 4),
#'   "score" = sample(c(1:100), 13)
#' )
#'
#' # Using balance() with specific number of rows
#' balance(df, 3, cat_col = "diagnosis")
#'
#' # Using balance() with min
#' balance(df, "min", cat_col = "diagnosis")
#'
#' # Using balance() with max
#' balance(df, "max", cat_col = "diagnosis")
#'
#' # Using balance() with id_method "n_ids"
#' # With column specifying added rows
#' balance(df, "max",
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   id_method = "n_ids",
#'   mark_new_rows = TRUE
#' )
#'
#' # Using balance() with id_method "n_rows_c"
#' # With column specifying added rows
#' balance(df, "max",
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   id_method = "n_rows_c",
#'   mark_new_rows = TRUE
#' )
#'
#' # Using balance() with id_method "distributed"
#' # With column specifying added rows
#' balance(df, "max",
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   id_method = "distributed",
#'   mark_new_rows = TRUE
#' )
#'
#' # Using balance() with id_method "nested"
#' # With column specifying added rows
#' balance(df, "max",
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   id_method = "nested",
#'   mark_new_rows = TRUE
#' )
#' @importFrom dplyr filter sample_n %>%
#' @importFrom rlang :=
balance <- function(data,
                    size,
                    cat_col,
                    id_col = NULL,
                    id_method = "n_ids",
                    # replace = TRUE, # TODO Some times we want to choose between replacement or repetition
                    mark_new_rows = FALSE,
                    new_rows_col_name = ".new_row") {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data,min.rows = 1, add = assert_collection)
  if (!(
    (checkmate::test_string(size) && size %in% c("min", "max", "mean", "median")) ||
    checkmate::test_count(x = size, positive = TRUE))){
    assert_collection$push("'size' must be one of 'min','max','mean','median' or a positive whole number.")
  }
  checkmate::assert_character(x = cat_col, min.len = 1, any.missing = FALSE, unique = TRUE,
                              names = "unnamed", add = assert_collection)
  checkmate::assert_string(x = id_col, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(x = id_method, add = assert_collection)
  checkmate::assert_string(x = new_rows_col_name, add = assert_collection)
  checkmate::assert_flag(x = mark_new_rows, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (!is.null(id_col) && id_col %ni% colnames(data)) {
    assert_collection$push(paste0("'id_col' column, '", id_col, "', not found in 'data'."))
  }
  if (length(setdiff(cat_col, colnames(data))) != 0){
    assert_collection$push(paste0("'cat_col' column(s), '",
                                  paste0(setdiff(cat_col, colnames(data)), collapse = ", "),
                                  "', not found in 'data'."))
  }
  # checkmate::assert_names( # More informative to say the *_col args are wrong!
  #   x = colnames(data),
  #   must.include = unique(c(cat_col, id_col)),
  #   add = assert_collection, what = "colnames"
  # )
  checkmate::assert_names(
    x = id_method,
    subset.of = c("n_ids", "n_rows_c", "distributed", "nested"),
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  if (!is.null(id_col)) {
    checkmate::assert_factor(x = data[[id_col]], add = assert_collection)
    if (id_col %in% cat_col) {
      assert_collection$push("'id_col' and 'cat_col' cannot contain the same column name.")
    }
    # Check that cat_col is constant within each ID
    # Note: I tested and count() is faster than group_keys()
    counts <- dplyr::count(data, !!as.name(id_col), !!as.name(cat_col))
    if (nrow(counts) != length(unique(counts[[id_col]]))){
      assert_collection$push("The value in 'data[[cat_col]]' must be constant within each ID.")
    }
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Add new row flag column
  local_tmp_new_row_var <- create_tmp_var(data, ".TmpNewRow")
  data[[local_tmp_new_row_var]] <- 0

  if (!is.null(id_col)) {
    if (id_method == "n_ids") {
      balanced <- id_method_n_ids_(
        data = data,
        size = size,
        cat_col = cat_col,
        id_col = id_col,
        mark_new_rows = mark_new_rows,
        new_rows_col_name = local_tmp_new_row_var
      )
    }
    else if (id_method == "n_rows_c") {
      balanced <- id_method_n_rows_closest(
        data = data,
        size = size,
        cat_col = cat_col,
        id_col = id_col,
        mark_new_rows = mark_new_rows,
        new_rows_col_name = local_tmp_new_row_var
      )
    }
    else if (id_method == "distributed") {
      balanced <- id_method_distributed(
        data = data,
        size = size,
        cat_col = cat_col,
        id_col = id_col,
        mark_new_rows = mark_new_rows,
        new_rows_col_name = local_tmp_new_row_var
      )
    }
    else if (id_method == "nested") {
      balanced <- id_method_nested(
        data = data,
        size = size,
        cat_col = cat_col,
        id_col = id_col,
        mark_new_rows = mark_new_rows,
        new_rows_col_name = local_tmp_new_row_var
      )
    }
  } else {
    to_size <- get_target_size(data, size, cat_col)
    balanced <- plyr::ldply(unique(data[[cat_col]]), function(category) {
      data_for_cat <- data %>%
        filter(!!as.name(cat_col) == category)

      n_rows <- nrow(data_for_cat)

      if (n_rows == to_size) {
        return(data_for_cat)
      } else if (n_rows < to_size) {
        return(
          add_rows_with_sampling(
            data = data_for_cat,
            to_size = to_size,
            new_rows_col_name = local_tmp_new_row_var
          )
        )
      } else {
        return(
          data_for_cat %>%
            sample_n(size = to_size, replace = FALSE)
        )
      }
    })
  }

  if (!isTRUE(mark_new_rows)) {
    balanced <- base_deselect(data = balanced,
                              cols = local_tmp_new_row_var)
  } else {
    # Replace temporary column name with passed column name
    # e.g. '.new_row'
    balanced <- base_rename(
      data = balanced,
      before = local_tmp_new_row_var,
      after = new_rows_col_name
    )
  }

  if (!is.null(id_col)) {
    balanced %>%
      dplyr::arrange(!!as.name(cat_col), !!as.name(id_col))
  } else {
    balanced %>%
      dplyr::arrange(!!as.name(cat_col))
  }
}
