

## downsample
#' @title Downsampling of rows in a dataframe.
#' @description Uses random downsampling to fix the group sizes to the
#'  smallest group in the data frame.
#' @details Downsampling is done without replacement, meaning that rows are not duplicated but only removed.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @inheritParams balance
#' @family sampling functions
#' @return Dataframe with some rows removed. Ordered by cat_col.
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create dataframe
#' df <- data.frame(
#'  "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
#'  "trial" = c(1,2,1,1,2,3,4),
#'  "score" = sample(c(1:100), 7))
#'
#' # Using upsample() with number
#' downsample(df,"participant")
#'
downsample <- function(data, cat_col) {
  balance(data, "min", cat_col)

}

## upsample
#' @title Upsampling of rows in a dataframe.
#' @description Uses random upsampling to fix the group sizes to the
#'  largest group in the data frame.
#' @details Upsampling is done with replacement for added rows, while the original data remains intact.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @inheritParams balance
#' @family sampling functions
#' @return Dataframe with added rows. Ordered by cat_col.
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create dataframe
#' df <- data.frame(
#'  "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
#'  "trial" = c(1,2,1,1,2,3,4),
#'  "score" = sample(c(1:100), 7))
#'
#' # Using upsample() with number
#' upsample(df,"participant")
#'
upsample <- function(data, cat_col) {
  balance(data, "max", cat_col)

}

## balance
#' @title Balance groups by up- or downsampling.
#' @description Uses up- or downsampling to fix the group size to the
#'  min, max, mean, or median group size or to a specific number of rows.
#' @details Upsampling is done with replacement for added rows, while the original data remains intact.
#' Downsampling is done without replacement, meaning that rows are not duplicated but only removed.
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
#' @family sampling functions
#' @return Dataframe with added and/or deleted rows. Ordered by cat_col.
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create dataframe
#' df <- data.frame(
#'  "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
#'  "trial" = c(1,2,1,1,2,3,4),
#'  "score" = sample(c(1:100), 7))
#'
#' # Using balance() with number
#' balance(df, 3, "participant")
#'
#' # Using balance() with min
#' balance(df, "min", "participant")
#'
#' # Using balance() with max
#' balance(df, "max", "participant")
#'
#' @importFrom dplyr filter sample_n %>%
balance <- function(data, size, cat_col) {
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

  group_sizes_summary <- find_group_sizes_summary(data, cat_col)

  if (is.character(size)) {
    to_size <- group_sizes_summary[size]
  } else {
    to_size <- size
  }

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

  balanced

}
