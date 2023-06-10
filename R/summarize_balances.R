

#   __________________ #< fcc7722513f44550a0f7a3a862ba5820 ># __________________
#   Summarize group balances                                                ####


# TODO The names of the output are perhaps a bit confusing?
# This requires really well-explained docs! And perhaps find better names?

# TODO Add more comments to code

# TODO Add group_aggregation_fn = list("mean"=mean, "sum"=sum) so people can select
# what to rank the numeric columns by.

#' @title Summarize group balances
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Summarize the balances of numeric, categorical, and ID columns
#'  in and between groups in one or more group columns.
#'
#'  This tool allows you to quickly and thoroughly assess the balance
#'  of different columns between groups. This is for instance useful
#'  after creating groups with \code{\link[groupdata2:fold]{fold()}},
#'  \code{\link[groupdata2:partition]{partition()}}, or
#'  \code{\link[groupdata2:collapse_groups]{collapse_groups()}} to
#'  check how well they did and to compare multiple
#'  groupings.
#'
#'  The output contains:
#'  \enumerate{
#'    \item \code{`Groups`}: a summary per group (per grouping column).
#'    \item \code{`Summary`}: statistical descriptors of the group summaries.
#'    \item \code{`Normalized Summary`}: statistical descriptors of a set of
#'    "normalized" group summaries. (Disabled by default)
#'  }
#'
#'  When comparing how balanced the grouping columns are, we can use
#'  the standard deviations of the group summary columns. The lower a standard
#'  deviation is, the more similar the groups are in that column. To quickly
#'  extract these standard deviations, ordered by an aggregated rank,
#'  use \code{\link[groupdata2:ranked_balances]{ranked_balances()}} on the
#'   \code{"Summary" data.frame} in the output.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param data \code{data.frame} with group columns to summarize
#'  by.
#'
#'  Can be \emph{grouped} (see \code{\link[dplyr:group_by]{dplyr::group_by()}}),
#'  in which case the function is applied group-wise. This is not to
#'  be confused with \code{`group_cols`}.
#' @param group_cols Names of columns with group identifiers to summarize columns
#'  in \code{`data`} by.
#' @param cat_cols Names of categorical columns to summarize.
#'
#'  Each categorical level is counted per group.
#'
#'  To distinguish between levels with the same name from different
#'  \code{`cat_col`} columns, we prefix the count column name for each
#'  categorical level with parts of the name of the categorical column.
#'  This amount can be controlled with \code{`max_cat_prefix_chars`}.
#'
#'  Normalization when \code{`include_normalized`} is enabled:
#'  The counts of each categorical level is normalized with \code{log(1 + count)}.
#' @param num_cols Names of numerical columns to summarize.
#'
#'  For each column, the \code{mean} and \code{sum} is calculated per group.
#'
#'  Normalization when \code{`include_normalized`} is enabled:
#'  Each column is normalized with \code{`num_normalize_fn`} before
#'  calculating the \code{mean} and \code{sum} per group.
#' @param id_cols Names of \code{factor} columns with IDs to summarize.
#'
#'  The number of unique IDs are counted per group.
#'
#'  Normalization when \code{`include_normalized`} is enabled:
#'  The count of unique IDs is normalized with \code{log(1 + count)}.
#' @param summarize_size Whether to summarize the number of rows per group.
#' @param include_normalized Whether to calculate and include the
#'  normalized summary in the output.
#' @param num_normalize_fn Function for normalizing the \code{`num_cols`} columns before
#'  calculating normalized group summaries.
#'
#'  Only used when \code{`include_normalized`} is enabled.
#'
#' @param rank_weights A named \code{vector} with weights for averaging the rank columns when calculating the \code{`SD_rank`} column.
#'  The name is one of the balancing columns and the number is its weight. Non-specified columns are given the weight \code{1}.
#'  The weights are automatically scaled to sum to 1.
#'
#'  When summarizing size (see \code{`summarize_size`}), name its weight \code{"size"}.
#'
#'  E.g. \code{c("size" = 1, "a_cat_col" = 2, "a_num_col" = 4, "an_id_col" = 2)}.
#' @param cat_levels_rank_weights Weights for averaging ranks of the categorical levels in \code{`cat_cols`}.
#'  Given as a named \code{list} with a named \code{vector} for each column in \code{`cat_cols`}.
#'  Non-specified levels are given the weight \code{1}.
#'  The weights are automatically scaled to sum to 1.
#'
#'  E.g. \code{list("a_cat_col" = c("a" = 3, "b" = 5), "b_cat_col" = c("1" = 3, "2" = 9))}
#' @family summarization functions
#' @return \code{list} with two/three \code{data.frames}:
#'
#'  \subsection{Groups}{
#'   A summary per group.
#'
#'   \code{`cat_cols`}: Each level has its own column with the count
#'   of the level per group.
#'
#'   \code{`num_cols`}: The \code{mean} and \code{sum} per group.
#'
#'   \code{`id_cols`}: The count of unique IDs per group.
#'  }
#'
#'  \subsection{Summary}{
#'   Statistical descriptors of the columns in \code{`Groups`}.
#'
#'   Contains the \code{mean}, \code{median}, standard deviation (\code{SD}),
#'   interquartile range (\code{IQR}), \code{min}, and \code{max} measures.
#'
#'   Especially the standard deviations and IQR measures can tell us about how
#'   balanced the groups are. When comparing multiple \code{`group_cols`},
#'   the group column with the lowest \code{SD} and \code{IQR}
#'   can be considered the most balanced.
#'  }
#'
#'  \subsection{Normalized Summary}{
#'   (Disabled by default)
#'
#'   Same statistical descriptors as in \code{`Summary`} but for a
#'   "normalized" version of the group summaries. The motivation
#'   is that these normalized measures can more easily be compared
#'   or combined to a single "balance score".
#'
#'   First, we normalize each balance column:
#'
#'   \code{`cat_cols`}: The level counts in the original group summaries are
#'   normalized with with \code{log(1 + count)}. This eases comparison
#'   of the statistical descriptors (especially standard deviations)
#'   of levels with very different count scales.
#'
#'   \code{`num_cols`}: The numeric columns are normalized prior to
#'   summarization by group, using the \code{`num_normalize_fn`} function.
#'   By default this applies MinMax scaling to columns such that ~95% of the values
#'   are expected to be in the \code{[0, 1]} range.
#'
#'   \code{`id_cols`}: The counts of unique IDs in the original group summaries are
#'   normalized with \code{log(1 + count)}.
#'
#'   Contains the \code{mean}, \code{median}, standard deviation (\code{SD}),
#'   interquartile range (\code{IQR}), \code{min}, and \code{max} measures.
#'  }
#'
#' @examples
#' # Attach packages
#' library(groupdata2)
#' library(dplyr)
#'
#' set.seed(1)
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
#' # Using fold()
#'
#' ## Without balancing
#' set.seed(1)
#' df_folded <- fold(data = df, k = 3)
#'
#' # Check the balances of the various columns
#' # As we have not used balancing in `fold()`
#' # we should not expect it to be amazingly balanced
#' df_folded %>%
#'   dplyr::ungroup() %>%
#'   summarize_balances(
#'     group_cols = ".folds",
#'     num_cols = c("score", "age"),
#'     cat_cols = "diagnosis",
#'     id_cols = "participant"
#'   )
#'
#' ## With balancing
#' set.seed(1)
#' df_folded <- fold(
#'   data = df,
#'   k = 3,
#'   cat_col = "diagnosis",
#'   num_col = 'score',
#'   id_col = 'participant'
#' )
#'
#' # Now the balance should be better
#' # although it may be difficult to get a good balance
#' # the 'score' column when also balancing on 'diagnosis'
#' # and keeping all rows per participant in the same fold
#' df_folded %>%
#'   dplyr::ungroup() %>%
#'   summarize_balances(
#'     group_cols = ".folds",
#'     num_cols = c("score", "age"),
#'     cat_cols = "diagnosis",
#'     id_cols = "participant"
#'   )
#'
#' # Comparing multiple grouping columns
#' # Create 3 fold column that only balance "score"
#' set.seed(1)
#' df_folded <- fold(
#'   data = df,
#'   k = 3,
#'   num_fold_cols = 3,
#'   num_col = 'score'
#' )
#'
#' # Summarize all three grouping cols at once
#' (summ <- df_folded %>%
#'   dplyr::ungroup() %>%
#'   summarize_balances(
#'     group_cols = paste0(".folds_", 1:3),
#'     num_cols = c("score")
#'   )
#' )
#'
#' # Extract the across-group standard deviations
#' # The group column with the lowest standard deviation(s)
#' # is the most balanced group column
#' summ %>% ranked_balances()
#'
summarize_balances <- function(
  data,
  group_cols,
  cat_cols = NULL,
  num_cols = NULL,
  id_cols = NULL,
  summarize_size = TRUE,
  include_normalized = FALSE,
  rank_weights = NULL,
  cat_levels_rank_weights = NULL,
  num_normalize_fn = function(x) {
    rearrr::min_max_scale(
      x,
      old_min = quantile(x, .025),
      old_max = quantile(x, .975),
      new_min = 0,
      new_max = 1)
  }) {

  #### Check arguments ####
  check_summarize_balances_(
    data = data,
    group_cols = group_cols,
    cat_cols = cat_cols,
    num_cols = num_cols,
    id_cols = id_cols,
    summarize_size = summarize_size,
    include_normalized = include_normalized,
    rank_weights = rank_weights,
    cat_levels_rank_weights = cat_levels_rank_weights,
    num_normalize_fn = num_normalize_fn
  )
  # End of argument checks ####

  # Calculate needed number of characters in col name prefixes
  # that allows distinguishing between column names
  max_prefix_chars <- calculate_max_prefix_nchars_(cat_cols = cat_cols, num_cols = num_cols)
  max_cat_prefix_chars <- max_prefix_chars[["max_cat_prefix_chars"]]
  max_num_prefix_chars <- max_prefix_chars[["max_num_prefix_chars"]]

  # Run per grouping in `data`
  result <- run_by_group_list(
    data = data,
    .fn = run_summarize_balances_,
    group_cols = group_cols,
    cat_cols = cat_cols,
    num_cols = num_cols,
    id_cols = id_cols,
    summarize_size = summarize_size,
    rank_weights = rank_weights,
    cat_levels_rank_weights = cat_levels_rank_weights,
    include_normalized = include_normalized,
    num_normalize_fn = num_normalize_fn,
    max_cat_prefix_chars = max_cat_prefix_chars,
    max_num_prefix_chars = max_num_prefix_chars
  )

  # If `data` was not grouped, this is our output
  if (!dplyr::is_grouped_df(data)){
    return(result)
  }

  # Get the grouping keys to add to the outputs
  group_keys <- data %>%
    dplyr::group_keys() %>%
    dplyr::mutate(._group_index = as.character(dplyr::row_number()))
  # Get names of `data`'s grouping columns
  group_col_names <- dplyr::group_vars(data)

  # Extracts all elements from list `l` with a given name
  # Binds them to a single data.frame
  # Adds group keys
  # Reorders columns and groups by the added group keys
  extract_and_add_groups_ <- function(l, name){
    l %c% name %>%
      dplyr::bind_rows(.id = "._group_index") %>%
      dplyr::left_join(group_keys, by = "._group_index") %>%
      dplyr::select(dplyr::one_of(group_col_names), dplyr::everything(), -"._group_index") %>%
      dplyr::group_by(!!!rlang::syms(group_col_names))
  }

  # Extract all the Groups data.frames
  # bind them, add group keys and formatting
  group_summaries <- extract_and_add_groups_(result, "Groups")
  summaries <- extract_and_add_groups_(result, "Summary")

  if (isTRUE(include_normalized)){
    normalized_summaries <- extract_and_add_groups_(result, "Normalized Summary")
  }

  #### Preparing output ####

  # Prepare output list
  out <- list(
    "Groups" = group_summaries,
    "Summary" = summaries
  )

  # Add normalized standard deviations
  if (isTRUE(include_normalized)){
    out[["Normalized Summary"]] <- normalized_summaries
  }

  out

}

run_summarize_balances_ <- function(
  data,
  group_cols,
  cat_cols,
  num_cols,
  id_cols,
  summarize_size,
  rank_weights,
  cat_levels_rank_weights,
  include_normalized,
  num_normalize_fn,
  max_cat_prefix_chars,
  max_num_prefix_chars) {

  # Make sure `group_cols` columns are factors
  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(dplyr::one_of(group_cols), factor))

  # Replace weight names with the names
  # of the summarized columns for the summarized
  # cat levels
  # Somewhat in the form: '# <col>_<level>'
  cat_levels_rank_weights <- prepare_cat_levels_rank_weights_(
    data = data,
    cat_cols = cat_cols,
    max_cat_prefix_chars = max_cat_prefix_chars,
    name_prefix = "# ",
    cat_levels_rank_weights = cat_levels_rank_weights
  )

  #### Create summaries ####

  # Create summaries
  group_summaries <-
    create_group_balance_summaries_(
      data = data,
      group_cols = group_cols,
      cat_cols = cat_cols,
      num_cols = num_cols,
      id_cols = id_cols,
      summarize_size = summarize_size,
      max_cat_prefix_chars = max_cat_prefix_chars
    )

  #### Combining summaries ####

  # Find expected column names from cat_cols summarization
  cat_summary_columns_list <- list()
  if (!is.null(cat_cols)){
    cat_summary_columns <- group_summaries[["cat"]] %>%
      dplyr::select(where(is.numeric)) %>%
      colnames()

    if (length(cat_cols) > 1 && max_cat_prefix_chars > 0){
      cat_summary_columns_list <- purrr::map(.x = cat_cols, .f = ~{
        base_name <- paste0("# ",
                            substr(.x, start = 1, stop = max_cat_prefix_chars))
        cat_summ_substring <- substr(cat_summary_columns,
                     start = 1,
                     stop = max_cat_prefix_chars + 2)
        # Remove potential trailing underscore
        cat_summ_substring <- gsub("_$", "", cat_summ_substring)
        # Get names that starts with this prefix
        cat_summary_columns[cat_summ_substring == base_name]
      }) %>% setNames(nm = cat_cols)
    } else {
      cat_summary_columns_list <- list(cat_summary_columns) %>%
        setNames(nm = cat_cols)
    }
  }

  # Join summaries
  group_summary <- join_group_summaries_(
    group_summaries = group_summaries,
    cat_cols = cat_cols,
    num_cols = num_cols,
    id_cols = id_cols,
    summarize_size = summarize_size
  )

  # Calculate measures for all numeric columns
  descriptors <- measure_summary_numerics_(
    group_summary,
    num_cols = num_cols,
    cat_level_cols = cat_summary_columns_list,
    id_cols = id_cols,
    summarize_size = summarize_size,
    rank_weights = rank_weights,
    cat_levels_rank_weights = cat_levels_rank_weights,
    logged_counts = FALSE,
    max_cat_prefix_chars = max_cat_prefix_chars,
    max_num_prefix_chars = max_num_prefix_chars
  )

  #### Normalized summaries ####

  if (isTRUE(include_normalized)){

    # Start with empty summary

    normalized_group_summary <- group_summaries[["empty"]]

    if (isTRUE(summarize_size)) {
      normalized_group_summary <- group_summaries[["size"]] %>%
        # Apply log10 to counts
        dplyr::mutate(dplyr::across(where(is.numeric), function(x) {
          # In case of zero-frequencies
          log10(1 + x)
        })) %>%
        dplyr::rename_with( ~ paste0("log(", ., ")"), where(is.numeric))
    }

    # Add summary of ID columns
    if (!is.null(id_cols)) {
      normalized_id_summary <- group_summaries[["id"]] %>%
        # Apply log10 to counts
        dplyr::mutate(dplyr::across(where(is.numeric), function(x) {
          # In case of zero-frequencies
          log10(1 + x)
        })) %>%
        dplyr::rename_with( ~ paste0("log(", ., ")"), where(is.numeric))

      normalized_group_summary <- normalized_group_summary %>%
        dplyr::left_join(normalized_id_summary, by = c(".group_col", ".group"))
    }

    # Add summary of normalized (MinMax scaled) numeric columns
    if (!is.null(num_cols)) {
      normalized_num_summary <- data %>%
        dplyr::mutate(dplyr::across(dplyr::one_of(num_cols), num_normalize_fn)) %>%
        create_group_balance_summaries_(group_cols = group_cols,
                                        num_cols = num_cols) %>%
        .[["num"]] %>%
        dplyr::rename_with( ~ gsub(pattern = "(^.*\\()([[:alnum:]]*)\\)$" ,
                                   replacement = "\\1norm(\\2))", x = .), where(is.numeric))

      normalized_group_summary <- normalized_group_summary %>%
        dplyr::left_join(normalized_num_summary, by = c(".group_col", ".group"))
    }

    # Add summary of categorical columns
    if (!is.null(cat_cols)) {
      normalized_cat_summary <- group_summaries[["cat"]] %>%
        # Apply log10 to counts
        dplyr::mutate(dplyr::across(where(is.numeric), function(x) {
          # In case of zero-frequencies
          log10(1 + x)
        })) %>%
        dplyr::rename_with( ~ paste0("log(", ., ")"), where(is.numeric))

      normalized_group_summary <- normalized_group_summary %>%
        dplyr::left_join(normalized_cat_summary, by = c(".group_col", ".group"))
    }

    # Calculate measures for all numeric columns
    normalized_descriptors <- measure_summary_numerics_(
      normalized_group_summary,
      num_cols = num_cols,
      cat_level_cols = purrr::map(
        .x = cat_summary_columns_list,
        .f = ~ {
          paste0("log(", .x, ")")
        }),
      id_cols = id_cols,
      summarize_size = summarize_size,
      rank_weights = rank_weights,
      cat_levels_rank_weights = purrr::map(
        .x = cat_levels_rank_weights,
        .f = ~ {
          names(.x) <- paste0("log(", names(.x), ")")
          .x
        }),
      logged_counts = TRUE,
      max_cat_prefix_chars = max_cat_prefix_chars,
      max_num_prefix_chars = max_num_prefix_chars
    )
  }

  #### Preparing output ####

  # Prepare output list
  out <- list(
    "Groups" = group_summary,
    "Summary" = descriptors
  )

  # Add normalized standard deviations
  if (isTRUE(include_normalized)){
    out[["Normalized Summary"]] <- normalized_descriptors
  }

  out
}


##  .................. #< 0fe21d3103c070a95e80dda9f1a89dcd ># ..................
##  Utils                                                                   ####


# Find the number of characters necessary to
# distinguish between column names in prefixes
calculate_max_prefix_nchars_ <- function(cat_cols, num_cols) {

  out <- list()

  # Find the number of characters necessary to
  # distinguish between names in `cat_cols`
  if (!is.null(cat_cols)) {
    for (i in 4:max(nchar(cat_cols))) {
      shorts <- substr(cat_cols, 1, i)
      if (length(unique(shorts)) == length(cat_cols)) {
        max_cat_prefix_chars <- i
        break
      }
    }
    out[["max_cat_prefix_chars"]] <- max_cat_prefix_chars
  }

  # Do the same for the num cols
  # We shorten the num+cat cols strings in SD ranking columns
  # We might have similarly named cat and num cols, so we
  # include cat_cols in the shortening
  if (!is.null(num_cols)) {
    for (i in 4:max(nchar(c(num_cols, cat_cols)))) {
      shorts <- substr(c(num_cols, cat_cols), 1, i)
      if (length(unique(shorts)) == length(num_cols) + length(cat_cols)) {
        max_num_prefix_chars <- i
        break
      }
    }
    out[["max_num_prefix_chars"]] <- max_num_prefix_chars
  }

  out

}


### . . . . . . . . .. #< 118181bc1a8cfda41b36a5b40cc32174 ># . . . . . . . . ..
### Summary creators                                                        ####


# Calculate each of the balance summaries
# for each group in each group column
create_group_balance_summaries_ <- function(
  data,
  group_cols,
  cat_cols = NULL,
  num_cols = NULL,
  id_cols = NULL,
  summarize_size = TRUE,
  max_cat_prefix_chars = 5) {

  format_summary_ <- function(data) {
    data %>%
      # Ensure right types
      dplyr::mutate(
        .group = factor(.data$.group),
        .group_col = factor(.data$.group_col)
      ) %>%
      position_first(col = ".group_col") %>%
      dplyr::arrange(.data$.group_col, .data$.group)
  }

  set_group_col_ <- function(data, name){
    data %>%
      dplyr::rename(.group = !!as.name(name)) %>%
      dplyr::mutate(.group_col = name)
  }

  out <- list("empty" = NULL, "size" = NULL, "id" = NULL, "num" = NULL, "cat" = NULL)

  out[["empty"]] <- purrr::map_df(.x = group_cols, .f = ~ {
    create_empty_summary_(data = data, group_col = .x) %>%
      set_group_col_(.x)
  }) %>% format_summary_()

  if (isTRUE(summarize_size)) {
    out[["size"]] <- purrr::map_df(.x = group_cols, .f = ~ {
      create_size_summary_(data = data, group_col = .x, name ="# rows") %>%
        set_group_col_(.x)
    }) %>% format_summary_()
  }

  if (!is.null(id_cols)){
    out[["id"]] <- purrr::map_df(.x = group_cols, .f = ~ {
      create_id_summaries_(data = data,
                           group_col = .x,
                           id_cols = id_cols) %>%
        set_group_col_(.x)
    }) %>% format_summary_()
  }

  if (!is.null(num_cols)){
    out[["num"]] <- purrr::map_df(.x = group_cols, .f = ~ {
      create_num_summaries_(data = data,
                            group_col = .x,
                            num_cols = num_cols,
                            rename = TRUE) %>%
        set_group_col_(.x)
    }) %>% format_summary_()
  }

  if (!is.null(cat_cols)){
    out[["cat"]] <- purrr::map_df(.x = group_cols, .f = ~ {
      create_cat_summaries_(
        data = data,
        group_col = .x,
        cat_cols = cat_cols,
        max_cat_prefix_chars = max_cat_prefix_chars
      ) %>%
        set_group_col_(.x)
    }) %>% format_summary_()
  }

  out
}

join_group_summaries_ <- function(
  group_summaries,
  cat_cols,
  num_cols,
  id_cols,
  summarize_size
) {

  # Join summaries
  group_summary <- group_summaries[["empty"]]

  if (isTRUE(summarize_size)) {
    group_summary <-
      dplyr::left_join(group_summary,
                       group_summaries[["size"]],
                       by = c(".group_col", ".group"))
  }
  if (!is.null(id_cols)) {
    group_summary <-
      dplyr::left_join(group_summary,
                       group_summaries[["id"]],
                       by = c(".group_col", ".group"))
  }
  if (!is.null(num_cols)) {
    group_summary <-
      dplyr::left_join(group_summary,
                       group_summaries[["num"]],
                       by = c(".group_col", ".group"))
  }
  if (!is.null(cat_cols)) {
    group_summary <-
      dplyr::left_join(group_summary,
                       group_summaries[["cat"]],
                       by = c(".group_col", ".group"))
  }

  group_summary
}


measure_summary_numerics_ <- function(
  data,
  num_cols,
  cat_level_cols,
  id_cols,
  summarize_size,
  rank_weights,
  cat_levels_rank_weights,
  logged_counts,
  max_cat_prefix_chars,
  max_num_prefix_chars) {

  # Calculate measures for all numeric columns
  descriptors <- data %>%
    dplyr::group_by(.data$.group_col)

  desc_fns <-
    list(
      "mean" = mean,
      "median" = median,
      "SD" = sd,
      "IQR" = IQR,
      "min" = min,
      "max" = max
    )

  # Calculate each measure
  measures <- plyr::llply(names(desc_fns), function(fn_name){
    fn <- desc_fns[[fn_name]]
    dplyr::summarize(descriptors, dplyr::across(where(is.numeric), fn)) %>%
      dplyr::mutate(measure = fn_name)
  }) %>% dplyr::bind_rows() %>%
    position_first(col = "measure") %>%
    position_first(col = ".group_col")  %>%
    dplyr::arrange(.data$.group_col)

  # Add SD rank columns
  measures <- add_sd_ranks_(
    measures = measures,
    cat_level_cols = cat_level_cols,
    num_cols = num_cols,
    id_cols = id_cols,
    summarize_size = summarize_size,
    rank_weights = rank_weights,
    cat_levels_rank_weights = cat_levels_rank_weights,
    logged_counts = logged_counts,
    max_cat_prefix_chars = max_cat_prefix_chars,
    max_num_prefix_chars = max_num_prefix_chars
  )

  measures

}


### . . . . . . . . .. #< 0bf5c2e81b2f1c78a4811574a98f12aa ># . . . . . . . . ..
### Ranking utils                                                           ####


# Calculate rank columns for the standard deviations
# And add to `measures`
add_sd_ranks_ <- function(
  measures,
  cat_level_cols,
  num_cols,
  id_cols,
  summarize_size,
  rank_weights,
  cat_levels_rank_weights,
  logged_counts,
  max_cat_prefix_chars,
  max_num_prefix_chars) {

  # Calculate average SD rank
  sd_rows <- measures %>%
    dplyr::filter(.data$measure == "SD")

  # Ranking only makes sense with multiple group cols
  if (nrow(sd_rows) == 1){
    return(measures)
  }

  # We map the user specified names to the output names
  # so we can use the ranking weights
  rank_names <- list()

  if (isTRUE(summarize_size)) {
    if (isTRUE(logged_counts)) {
      rank_names[["size"]] <- "log(# rows)"
    } else {
      rank_names[["size"]] <- "# rows"
    }
  }

  if (!is.null(id_cols)) {
    for (id_col in id_cols) {
      if (isTRUE(logged_counts)) {
        rank_names[[id_col]] <- paste0("log(# ", id_col, ")")
      } else {
        rank_names[[id_col]] <- paste0("# ", id_col)
      }
    }
  }

  cat_sd_rank_cols = character(0)
  for (cat_col in names(cat_level_cols)){
    cat_col_prefix <- substr(cat_col, start = 1, stop = max_cat_prefix_chars)
    cat_sd_rank_col <- paste0(cat_col_prefix, "_SD_rank")
    rank_names[[cat_col]] <- cat_sd_rank_col
    cat_sd_rank_cols <- c(
      cat_sd_rank_cols,
      cat_sd_rank_col
    )

    current_cat_level_weights <- NULL
    if (!is.null(cat_levels_rank_weights) &&
        cat_col %in% names(cat_levels_rank_weights)){
      current_cat_level_weights <- cat_levels_rank_weights[[cat_col]]
      missing_cat_level_weights <- setdiff(
        cat_level_cols[[cat_col]],
        names(current_cat_level_weights)
      )
      unknown_cat_level_weights <- setdiff(
        names(current_cat_level_weights),
        cat_level_cols[[cat_col]]
      )
      if (length(unknown_cat_level_weights) > 0){
        stop(paste0("Found ranking weights for unknown categorical levels: ",
                    paste0(unknown_cat_level_weights, collapse = ", ")))
      }
      if (length(missing_cat_level_weights) > 0){
        current_cat_level_weights <- c(
          current_cat_level_weights,
          setNames(
            rep(1, length(missing_cat_level_weights)),
            missing_cat_level_weights
          )
        )
      }
    }

    sd_rows <- sd_rows %>%
      mean_rank_numeric_cols_(
        cols = cat_level_cols[[cat_col]],
        col_name = rank_names[[cat_col]],
        rank_weights = current_cat_level_weights
      )
  }

  num_sd_rank_cols = character(0)
  if (!is.null(num_cols)){
    for (num_col in num_cols){
      # Names of num_cols columns (e.g. mean(score) and sum(score))
      num_col_cols <- colnames(sd_rows)[
        grepl(paste0("\\(", num_col, "\\)"), colnames(sd_rows))]
      num_col_prefix <- substr(num_col, start = 1, stop = max_num_prefix_chars)
      num_sd_rank_col <- paste0(num_col_prefix, "_SD_rank")
      rank_names[[num_col]] <- num_sd_rank_col
      num_sd_rank_cols <- c(
        num_sd_rank_cols,
        num_sd_rank_col
      )

      # Combined rank for this num_col
      sd_rows <- sd_rows %>%
        mean_rank_numeric_cols_(
          cols = num_col_cols,
          col_name = rank_names[[num_col]]
        )
    }
  }

  if (!is.null(rank_weights)) {
    # Convert names in ranking weights to their names in the output
    names(rank_weights) <-
      purrr::map(.x = names(rank_weights), .f = ~ {
        rank_names[[.x]]
      })

    # If any of the elements are not in the weights
    # We add them with a value of 1
    names_to_add <- setdiff(rank_names, names(rank_weights))
    for (name in names_to_add) {
      rank_weights[[name]] <- 1
    }
  } else {
    rank_weights <- rep(1, times = length(rank_names)) %>%
      setNames(rank_names)
  }

  if (is.list(rank_weights)){
    # Convert to numeric
    rank_weights <- unlist(rank_weights,
                           recursive = FALSE,
                           use.names = TRUE)
  }

  cols_to_rank <- sd_rows %>%
    dplyr::select(where(is.numeric)) %>%
    dplyr::select(-dplyr::any_of(unlist(cat_level_cols)),
                  # We combined ranks for the num cols already
                  -dplyr::contains("mean("),
                  -dplyr::contains("sum(")) %>%
    colnames()

  sd_ranks <- sd_rows %>%
    mean_rank_numeric_cols_(
      cols = cols_to_rank,
      col_name = "SD_rank",
      rank_weights = rank_weights,
      already_rank_cols = c(cat_sd_rank_cols, num_sd_rank_cols)) %>%
    dplyr::select(".group_col",
                  dplyr::ends_with("SD_rank"))

  measures <- measures %>%
    dplyr::left_join(sd_ranks, by = ".group_col")

  measures
}


prepare_cat_levels_rank_weights_ <- function(
  data,
  cat_cols,
  max_cat_prefix_chars,
  name_prefix = "# ",
  cat_levels_rank_weights
){
  if (is.null(cat_cols) || is.null(cat_levels_rank_weights)){
    return(NULL)
  }

  # Create mappings of cat level to name
  # of summarized cat level column
  cat_levels_name_maps <- create_cat_name_map_(
    data = data,
    cat_cols = cat_cols,
    max_cat_prefix_chars = max_cat_prefix_chars,
    name_prefix = name_prefix
  )

  # Replace weight names with the summarized
  # cat level column names
  for (cat_col in names(cat_levels_rank_weights)){
    names(cat_levels_rank_weights[[cat_col]]) <- purrr::map(
      names(cat_levels_rank_weights[[cat_col]]),
      .f = ~{
        cat_levels_name_maps[[cat_col]][[.x]]
      }
    )
  }

  cat_levels_rank_weights

}


### . . . . . . . . .. #< 2f811300a39e4a220023e6117136c4aa ># . . . . . . . . ..
### Argument checks                                                         ####


# Check arguments for `summarize_balances()`
check_summarize_balances_ <- function(
  data,
  group_cols,
  cat_cols,
  num_cols,
  id_cols,
  summarize_size,
  include_normalized,
  rank_weights,
  cat_levels_rank_weights,
  num_normalize_fn
){
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, min.rows = 1, add = assert_collection)
  checkmate::assert_character(
    x = group_cols,
    min.len = 1,
    min.chars = 1,
    any.missing = FALSE,
    unique = TRUE,
    names = "unnamed",
    add = assert_collection
  )
  checkmate::assert_character(
    x = cat_cols,
    min.len = 1,
    min.chars = 1,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE,
    names = "unnamed",
    add = assert_collection
  )
  checkmate::assert_character(
    x = num_cols,
    min.len = 1,
    min.chars = 1,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE,
    names = "unnamed",
    add = assert_collection
  )
  checkmate::assert_character(
    x = id_cols,
    min.len = 1,
    min.chars = 1,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE,
    names = "unnamed",
    add = assert_collection
  )
  checkmate::assert_numeric(
    x = rank_weights,
    lower = 0,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 1,
    names = "unique",
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_list(
    x = cat_levels_rank_weights,
    types = "numeric",
    any.missing = FALSE,
    names = "unique",
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_flag(x = summarize_size, add = assert_collection)
  checkmate::assert_flag(x = include_normalized, add = assert_collection)
  checkmate::assert_function(x = num_normalize_fn, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(
    x = colnames(data),
    must.include = c(group_cols, cat_cols, num_cols, id_cols),
    add = assert_collection
  )
  if (!is.null(rank_weights)){
    checkmate::assert_names(
      x = names(rank_weights),
      subset.of = c(cat_cols, num_cols, id_cols, "size"),
      add = assert_collection
    )
  }
  if (!is.null(cat_levels_rank_weights)){
    checkmate::assert_names(
      x = names(cat_levels_rank_weights),
      subset.of = cat_cols,
      add = assert_collection
    )
    checkmate::reportAssertions(assert_collection)
    for (cat_col in names(cat_levels_rank_weights)){
      checkmate::assert_numeric(
        cat_levels_rank_weights[[cat_col]],
        lower = 0,
        finite = TRUE,
        any.missing = FALSE,
        names = "unique",
        .var.name = paste0("cat_levels_rank_weights[['", cat_col, "']]")
      )
      checkmate::assert_names(
        x = names(cat_levels_rank_weights[[cat_col]]),
        subset.of = levels(data[[cat_col]]),
        .var.name = paste0("names(cat_levels_rank_weights[['", cat_col, "']])")
      )
    }
  }
  if (length(intersect(cat_cols, id_cols)) > 0) {
    assert_collection$push("found identical names in `cat_cols` and `id_cols`.")
  }
  # Get names of `data`'s grouping columns
  group_col_names <- dplyr::group_vars(data)
  if (length(intersect(group_col_names, c(
    c(cat_cols, num_cols, id_cols, group_cols)
  ))) > 0) {
    assert_collection$push("columns that `data` is grouped by can not be used in the arguments.")
  }
  checkmate::reportAssertions(assert_collection)
  if (!is.null(num_cols)){
    for (num_col in num_cols){
      checkmate::assert_numeric(
        data[[num_col]],
        finite = TRUE,
        any.missing = FALSE,
        add = assert_collection,
        .var.name = paste0("data[['", num_col, "']]")
      )
    }
  }
  checkmate::reportAssertions(assert_collection)

  invisible(NULL)
}
