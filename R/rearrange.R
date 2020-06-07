

#   __________________ #< c9bf40be0f67f749426e4b447b274c13 ># __________________
#   Rearrange data frame                                                    ####


#' @title Arrange a data frame by a set of methods
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a special sorting factor and sorts by it. Current methods are
#'  \code{"pair_extremes"}, \code{"center_max"}, and \code{"center_min"}.
#'
#'  For an easier experience and usage examples, see the relevant wrapper functions: ...
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @keywords internal
#' @family rearrange functions
#' @param data \code{data.frame} or \code{vector}. When a \code{vector}, it is converted to a \code{data.frame}.
#' @param col Column to create sorting factor by. When \code{NULL} and \code{data} is a data frame,
#'  it uses the row numbers.
#' @param method Sorting method.
#'
#'  One of: \code{"pair_extremes"}, \code{"center_max"}, or \code{"center_min"}.
#'
#'  \subsection{pair_extremes}{
#'  The values are paired/grouped such that the highest and lowest values form the first group,
#'  the second highest and the second lowest values form the second group, and so on.
#'  The values are then sorted by these groups/pairs.
#'
#'  When \code{data} has an uneven number of rows,
#'  the \code{unequal_method} argument determines which group should have only 1 element.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, 5, 6)}
#'
#'  Creates the \strong{sorting factor}:
#'
#'  \code{c(1, 2, 3, 3, 2, 1)}
#'
#'  And are \strong{ordered as}:
#'
#'  \code{c(1, 6, 2, 5, 3, 4)}
#'  }
#'
#'  \subsection{center_max}{
#'  The highest value is positioned in the middle with
#'  values decreasing around it.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4,} \strong{\code{5}}\code{)}
#'
#'  are \strong{ordered as}:
#'
#'  \code{c(1, 3,} \strong{\code{5}}\code{, 4, 2)}
#'
#'  }
#'
#'  \subsection{center_min}{
#'  The lowest value is positioned in the middle with
#'  values increasing around it.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(}\strong{\code{1}}\code{, 2, 3, 4, 5)}
#'
#'  are \strong{ordered as}:
#'
#'  \code{c(5, 3,} \strong{\code{1}}\code{, 2, 4)}
#'
#'  }
#' @param unequal_method Method for dealing with an unequal number of rows
#'  in \code{data} when \code{method} is \code{"pair_extremes"}.
#'
#'  One of: \code{first}, \code{middle} or \code{last}
#'
#'  \subsection{first}{
#'  The first group will have size \code{1}.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, 5)}
#'
#'  Creates the \strong{sorting factor}:
#'
#'  \code{c(}\strong{\code{1}}\code{, 2, 3, 3, 2)}
#'
#'  And are \strong{ordered as}:
#'
#'  \code{c(}\strong{\code{1}}\code{, 2, 5, 3, 4)}
#'
#'  }
#'
#' \subsection{middle}{
#'  The middle group will have size \code{1}.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, 5)}
#'
#'  Creates the \strong{sorting factor}:
#'
#'  \code{c(1, 3, }\strong{\code{2}}\code{, 3, 1)}
#'
#'  And are \strong{ordered as}:
#'
#'  \code{c(1, 5, } \strong{\code{3}}\code{, 2, 4)}
#'
#'  }
#' \subsection{last}{
#'  The last group will have size \code{1}.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, 5)}
#'
#'  Creates the \strong{sorting factor}:
#'
#'  \code{c(1, 2, 2, 1, }\strong{\code{3}}\code{)}
#'
#'  And are \strong{ordered as}:
#'
#'  \code{c(1, 4, 2, 3,} \strong{\code{5}}\code{)}
#'
#'  }
#' @param shuffle_members Whether to shuffle the pair members. (Logical)
#'
#'  For the \code{"center_*"} methods, this randomizes which values are
#'  to the right and left of the center.
#' @param shuffle_pairs Whether to shuffle the pairs when
#'  \code{method} is \code{"pair_extremes"}. (Logical)
#' @param keep_factor Whether to keep the sorting factor in the data frame. \code{Logical}.
#'
#'  This is mostly useful with the \code{"pair_extremes"} method.
#' @param factor_name Name of sorting factor.
#'
#'  N.B. Only used when \code{keep_factor} is \code{TRUE}.
#' @return
#'  The sorted data frame. Optionally with the sorting factor added.
rearrange <- function(data,
                      col = NULL,
                      method = "pair_extremes",
                      unequal_method = "middle",
                      shuffle_members = FALSE,
                      shuffle_pairs = FALSE,
                      keep_factor = FALSE,
                      factor_name = ".rearrange_factor") {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()

  # Initial check of 'data'
  checkmate::assert(
    checkmate::check_data_frame(
      data),
    checkmate::check_vector(
      data, strict = TRUE, any.missing = FALSE),
    checkmate::check_factor(
      data, any.missing = FALSE)
  )

  if (!is.data.frame(data) && is.list(data)){
    assert_collection$push(
      "when 'data' is not a data frame, it cannot be a list."
    )
    checkmate::reportAssertions(assert_collection)
  }

  # Convert to data frame
  if (!is.list(data)){
    if (!is.null(col)){
      assert_collection$push(
        "when 'data' is not a data frame, 'col' must be 'NULL'."
      )
      checkmate::reportAssertions(assert_collection)
    }
    data <- data.frame("Value" = data,
                       stringsAsFactors = FALSE)
    col = "Value"
  }

  # Second check of 'data'
  checkmate::assert_data_frame(data, min.rows = 1, add = assert_collection)
  checkmate::assert_string(col, min.chars = 1,null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(method, min.chars = 1, add = assert_collection)
  checkmate::assert_string(unequal_method, min.chars = 1, add = assert_collection)
  checkmate::assert_string(factor_name, min.chars = 1, add = assert_collection)
  checkmate::assert_flag(keep_factor, add = assert_collection)
  checkmate::assert_flag(shuffle_members, add = assert_collection)
  checkmate::assert_flag(shuffle_pairs, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(method,
                          subset.of = c("pair_extremes", "center_max", "center_min"),
                          add = assert_collection)
  checkmate::assert_names(unequal_method,
                          subset.of = c("first", "middle", "last"),
                          add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  local_tmp_rearrange_var <- create_tmp_var(data, ".rearrange_factor_")
  rm_col <- is.null(col)
  if (is.null(col)){
    col <- create_tmp_var(data, ".tmp_col_")
  }

  # Extract and add group indices
  tmp_grp_indices <- create_tmp_var(data, tmp_var = "grp_indices")
  data[[tmp_grp_indices]] <- dplyr::group_indices(data)
  data <- dplyr::ungroup(data)

  # Rearrange per group
  data <- plyr::ldply(unique(data[[tmp_grp_indices]]), function(grp_ind){

    # Subset current group
    current_data <- data[data[[tmp_grp_indices]] == grp_ind, ]

    if (isTRUE(rm_col)){
      current_data[[col]] <- seq_len(nrow(current_data))
    } else {
      # , drop is required for working with single-column data frames
      current_data <- current_data[order(current_data[[col]]),
                                   , drop = FALSE]
    }

    # We might get future methods that don't require extreme pairing
    if (method == "pair_extremes"){
      current_data[[local_tmp_rearrange_var]] <- create_rearrange_factor_pair_extremes_(
        size = nrow(current_data), unequal_method = unequal_method
      )

      # Order current_data by the pairs
      current_data <- order_by_group(
        data = current_data,
        group_col = local_tmp_rearrange_var,
        shuffle_members = shuffle_members,
        shuffle_pairs = shuffle_pairs)

    } else if (method %in% c("center_max", "center_min")){
      current_data <- rearrange_center_by(
        data = current_data,
        col = col,
        shuffle_members = shuffle_members,
        what = ifelse(method == "center_max", "max", "min")
      )
      current_data[[local_tmp_rearrange_var]] <- seq_len(nrow(current_data))
    }

    current_data

  }) %>%
    base_deselect(cols = tmp_grp_indices)


  # Remove rearrange factor if it shouldn't be returned
  if (!isTRUE(keep_factor)) {
    data <- data %>%
      base_deselect(cols = local_tmp_rearrange_var)
  } else if (local_tmp_rearrange_var != factor_name) {
    data <- base_rename(data,
      before = local_tmp_rearrange_var,
      after = factor_name
    )
    data[[factor_name]] <- as.factor(data[[factor_name]])
  }

  # Remove tmp column if 'col' was 'NULL'
  if (isTRUE(rm_col)){
    data[[col]] <- NULL
  }

  row.names(data) <- NULL

  data
}

##  .................. #< d7de8cc19d87063af61c9c5164b8e37c ># ..................
##  Rearrange helpers                                                       ####


# TODO rename to pair_extreme_rows
# Then we can also have pair_extreme_vals
create_rearrange_factor_pair_extremes_ <- function(size, unequal_method = "middle") {
  #
  # Creates factor for rearranging in 1st, last, 2nd, 2nd last, 3rd, 3rd last, ...
  # When size is unequal, there are two methods for dealing with it:
  # .. "first":
  # .. .. the first row becomes group 1 on its own.
  # .. .. creates rearrange factor on the rest, all gets +1
  # .. .. e.g. 1,2,3,4,4,3,2
  # .. "middle":
  # .. .. adds ceiling(size / 4) in the middle of the factor
  # .. .. every value larger than or equal to the middle value gets +1
  # .. .. e.g. 1,2,4,5,3,5,4,2,1
  # .. "last":
  # .. .. the last row becomes the last group on its own.
  # .. .. creates rearrange factor on the rest
  # .. .. e.g. 1,2,3,4,4,3,2,1,5
  #

  if (size == 1)
    return(1)
  half_size <- floor(size / 2)
  idx <- 1:(half_size)
  if (half_size * 2 == size) {
    return(c(idx, rev(idx)))
  } else {
    if (unequal_method == "middle") {
      middle <- ceiling((half_size / 2)) + 1
      idx <- idx %>%
        tibble::enframe(name = NULL) %>%
        dplyr::mutate(value = ifelse(
          .data$value >= middle, .data$value + 1, .data$value)) %>%
        dplyr::pull(.data$value)
      return(c(idx, middle, rev(idx)))
    } else if (unequal_method == "first") {
      return(c(1, c(idx, rev(idx)) + 1))
    } else if (unequal_method == "last") {
      return(c(c(idx, rev(idx)), max(idx) + 1))
    }
  }
}

rearrange_center_by <- function(data, col, shuffle_members, what = "max") {

  size <- nrow(data)

  if (size < 2){
    return(data)
  }

  # NOTE: The extra comma is on purpose!
  # 'drop' is required for single-column data frames
  data <- data[order(data[[col]], decreasing = what == "min"),
               , drop = FALSE]

  # If uneven length
  # extract max and remove from vec
  middle_row <- NULL
  if (size %% 2 == 1){
    if (what == "max"){
      middle_val <- max(data[[col]])
    } else if (what == "min"){
      middle_val <- min(data[[col]])
    }
    # Extract and remove middle row
    middle_row <- data[min(which(data[[col]] == middle_val)), , drop = FALSE]
    data <- data[-min(which(data[[col]] == middle_val)), , drop = FALSE]
    size <- size - 1
  }

  # Create extreme pairs factor
  extreme_pairs <- create_rearrange_factor_pair_extremes_(
    size = size)

  if (isTRUE(shuffle_members)){
    # Add noise to the pair indices to randomize the order
    # of the members
    extreme_pairs <- as.numeric(extreme_pairs) +
      runif(length(extreme_pairs), -0.1, 0.1)
  }

  # Convert to ordering index
  new_order <- seq_len(size)[order(extreme_pairs)]
  # Add ordering index to data
  tmp_order_var <- create_tmp_var(data, tmp_var = ".tmp_ordering_factor")
  data[[tmp_order_var]] <- new_order
  # Order 'data'
  data <- data[order(data[[tmp_order_var]]), ]
  data[[tmp_order_var]] <- NULL

  # Insert middle if it was removed
  if (!is.null(middle_row)){
    data <- insert_row(
      data = data,
      new_row = middle_row,
      after = size %/% 2)
  }

  data
}

order_by_group <- function(data, group_col, shuffle_members, shuffle_pairs){

  backup_group_col <- isTRUE(shuffle_pairs) || isTRUE(shuffle_members)
  if (isTRUE(backup_group_col)){
    tmp_backup_group_col <- create_tmp_var(data, tmp_var = "group_col_backup_")
    data[[tmp_backup_group_col]] <- data[[group_col]]
  }

  if (isTRUE(shuffle_pairs)){
    # Randomize levels
    data[[group_col]] <- as.numeric(
      factor(data[[group_col]], levels = sample(unique(data[[group_col]])))
    )
  }
  if (isTRUE(shuffle_members)){
    # Add random noise to the group indices
    data[[group_col]] <-
      as.numeric(data[[group_col]]) +
      runif(nrow(data), -0.1, 0.1)
  }

  # Order data frame by the groups
  data <- data[order(data[[group_col]]), , drop = FALSE]

  if (isTRUE(backup_group_col)){
    # Revert to original group col
    data[[group_col]] <- data[[tmp_backup_group_col]]
    data[[tmp_backup_group_col]] <- NULL
  }

  data
}
