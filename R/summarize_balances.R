

#   __________________ #< fcc7722513f44550a0f7a3a862ba5820 ># __________________
#   Summarize group balances                                                ####


# TODO The names of the output are perhaps a bit confusing?
# This requires really well-explained docs! And perhaps find better names?

# TODO Add more comments to code

#' @title Summarize group balances
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Summarize the balances of numeric, categorical, and ID columns
#'  in and between groups in one or more group columns.
#'
#'  This tools allows you to quickly and thorughly assess the balance
#'  of different columns between groups. This is for instance useful
#'  after creating groups with \code{\link[groupdata2:fold]{fold()}},
#'  \code{\link[groupdata2:partition]{partition()}}, or
#'  \code{\link[groupdata2:collapse_groups]{collapse_groups()}} to
#'  check their performance on your data.
#'
#'  The output contains:
#'  \enumerate{
#'    \item \code{`Groups`}: a summary per group.
#'    \item \code{`Summary`}: statistical descriptors of the group summaries.
#'    \item \code{`Normalized Summary`}: statistical descriptors of a set of
#'    "normalized" group summaries. (Disabled by default)
#'  }
#'
#'  When evaluating how balanced the grouping columns are, we use
#'  the standard deviations of the group summary columns. The lower a standard
#'  deviation is, the more similar the groups are on that column. To quickly
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
#'  Normalization: The counts of each categorical level is normalized with \code{log(1 + count)}.
#' @param num_cols Names of numerical columns to summarize.
#'
#'  For each column, the \code{mean} and \code{sum} is calculated per group.
#'
#'  Normalization: Each column is normalized with \code{`num_normalize_fn`} before
#'  calculating the \code{mean} and \code{sum} per group.
#' @param id_cols Names of factor columns with IDs to summarize.
#'
#'  The number of unique IDs are counted per group.
#'
#'  Normalization: The count of unique IDs is normalized with \code{log(1 + count)}.
#' @param summarize_size Whether to summarize size (number of rows per group).
#' @param include_normalized Whether to calculate and include the
#'  normalized summary in the output. (logical)
#' @param num_normalize_fn Function for normalizing the \code{`num_cols`} columns before
#'  calculating normalized group summaries.
#'
#'  Only used when \code{`include_normalized`} is enabled.
#'
#' @param ranking_weights A named vector with weights for averaging the rank columns when calculating the `SD_rank` column.
#'  The name is one of the balancing columns and the number is its weight. Non-specified columns are given the weight \code{1}.
#'  The weights are automatically scaled to sum to 1.
#'
#'  When summarizing size (see \code{`summarize_size`}), name its weight \code{"size"}.
#'
#'  E.g. \code{c("size" = 1, "a_cat_col" = 2, "a_num_col" = 4, "an_id_col" = 2)}.
#' @family summarization functions
#' @return \code{list} with three \code{data.frames}:
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
#'   Statistical descriptors of the columns in \code{`Group`}.
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
#'   \code{`num_cols`}: The numerical columns are normalized prior to
#'   summarization by group, using the \code{`num_normalize_fn`} function.
#'   By default this applies MinMax scaling to columns so they are in the
#'   range \code{[0, 1]}.
#'   !!!TODO outliers can make them difficult to compare?!!!
#'
#'   \code{`id_cols`}: The counts of unique IDs in the original group summaries are
#'   normalized with with \code{log(1 + count)}.
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
#' df_folded <- fold(data = df, k = 3, method = "n_dist")
#'
#' ## With cat_col
#' df_folded <- fold(
#'   data = df,
#'   k = 3,
#'   cat_col = "diagnosis",
#'   method = "n_dist"
#' )
#'
#' # summ$Summary %>% ranked_balances()
summarize_balances <- function(
  data,
  group_cols,
  cat_cols = NULL,
  num_cols = NULL,
  id_cols = NULL,
  summarize_size = TRUE,
  include_normalized = FALSE,
  ranking_weights = NULL,
  num_normalize_fn = function(x) {
    rearrr::min_max_scale(x, new_min = 0, new_max = 1)
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
    ranking_weights = ranking_weights,
    num_normalize_fn = num_normalize_fn
  )
  # End of argument checks ####

  # Calculate needed number of characters in colname prefixes
  # that allows distinguishing between column names
  max_prefix_chars <- calculate_max_prefix_nchars_(cat_cols = cat_cols, num_cols = num_cols)
  max_cat_prefix_chars <- max_prefix_chars[["max_cat_prefix_chars"]]
  max_num_prefix_chars <- max_prefix_chars[["max_num_prefix_chars"]]

  # Run per grouping in `data`
  result <- run_by_group_list(
    data = data,
    .fn = run_summarize_balances,
    group_cols = group_cols,
    cat_cols = cat_cols,
    num_cols = num_cols,
    id_cols = id_cols,
    summarize_size = summarize_size,
    ranking_weights = ranking_weights,
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
      dplyr::select(dplyr::one_of(group_col_names), dplyr::everything(), -.data$._group_index) %>%
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

run_summarize_balances <- function(
  data,
  group_cols,
  cat_cols,
  num_cols,
  id_cols,
  summarize_size,
  ranking_weights,
  include_normalized,
  num_normalize_fn,
  max_cat_prefix_chars,
  max_num_prefix_chars) {

  # Make sure `group_cols` columns are factors
  data <- data %>%
    dplyr::ungroup() %>% # TODO should be handled before this part!
    dplyr::mutate(dplyr::across(dplyr::one_of(group_cols), factor))

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
        base_name <- paste0(
          "# ",
          substr(.x, start = 1, stop = max_cat_prefix_chars)
        )
        # Get names that starts with this prefix
        cat_summary_columns[
          substr(cat_summary_columns,
                 start = 1,
                 stop = max_cat_prefix_chars + 2) == base_name
        ]
      }) %>% setNames(nm = cat_cols)
    } else {
      cat_summary_columns_list <- list(cat_summary_columns) %>%
        setNames(nm = cat_cols)
    }
  }

  # Join summaries
  group_summary <- group_summaries[["empty"]] %>%
    # When the arg is not NULL, add it's summary with a join
    purrr::when(isTRUE(summarize_size) ~
                  dplyr::left_join(., group_summaries[["size"]],
                                   by = c(".group_col", ".group")),
                ~ .) %>% # Else return the input
    purrr::when(!is.null(id_cols) ~
                  dplyr::left_join(., group_summaries[["id"]],
                                   by = c(".group_col", ".group")),
                ~ .) %>%
    purrr::when(!is.null(num_cols) ~
                  dplyr::left_join(., group_summaries[["num"]],
                                   by = c(".group_col", ".group")),
                ~ .) %>%
    purrr::when(!is.null(cat_cols) ~
                  dplyr::left_join(., group_summaries[["cat"]],
                                   by = c(".group_col", ".group")),
                ~ .)

  # Calculate measures for all numeric columns
  descriptors <- measure_summary_numerics_(
    group_summary,
    num_cols = num_cols,
    cat_level_cols = cat_summary_columns_list,
    id_cols = id_cols,
    summarize_size = summarize_size,
    ranking_weights = ranking_weights,
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
      # TODO perhaps add a row mean per cat_col after log?

      normalized_group_summary <- normalized_group_summary %>%
        dplyr::left_join(normalized_cat_summary, by = c(".group_col", ".group"))
    }

    # Calculate measures for all numeric columns
    normalized_descriptors <- measure_summary_numerics_(
      normalized_group_summary,
      num_cols = num_cols,
      cat_level_cols = purrr::map(.x = cat_summary_columns_list,
                                  .f = ~ {
                                    paste0("log(", .x, ")")
                                  }),
      id_cols = id_cols,
      summarize_size = summarize_size,
      ranking_weights = ranking_weights,
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
    for (i in 4:max(nchar(num_cols))) {
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

  out <- list("empty" = NULL, "size" = NULL, "id" = NULL, "num" = NULL, "cat" = NULL)

  out[["empty"]] <- purrr::map_df(.x = group_cols, .f = ~ {
    create_empty_summary_(data = data, group_col = .x) %>%
      dplyr::rename(.group = !!as.name(.x)) %>%
      dplyr::mutate(.group_col = .x)
  }) %>% format_summary_()

  if (isTRUE(summarize_size)) {
    out[["size"]] <- purrr::map_df(.x = group_cols, .f = ~ {
      create_size_summary_(data = data, group_col = .x) %>%
        dplyr::rename(.group = !!as.name(.x)) %>%
        dplyr::mutate(.group_col = .x)
    }) %>% format_summary_()
  }

  if (!is.null(id_cols)){
    out[["id"]] <- purrr::map_df(.x = group_cols, .f = ~ {
      create_id_summaries_(data = data,
                           group_col = .x,
                           id_cols = id_cols) %>%
        dplyr::rename(.group = !!as.name(.x)) %>%
        dplyr::mutate(.group_col = .x)
    }) %>% format_summary_()
  }

  if (!is.null(num_cols)){
    out[["num"]] <- purrr::map_df(.x = group_cols, .f = ~ {
      create_num_summaries_(data = data,
                            group_col = .x,
                            num_cols = num_cols) %>%
        dplyr::rename(.group = !!as.name(.x)) %>%
        dplyr::mutate(.group_col = .x)
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
        dplyr::rename(.group = !!as.name(.x)) %>%
        dplyr::mutate(.group_col = .x)
    }) %>% format_summary_()
  }

  out
}

create_empty_summary_ <- function(data, group_col){
  data %>%
    dplyr::ungroup() %>%
    dplyr::select(!!as.name(group_col)) %>%
    dplyr::group_by(!!as.name(group_col)) %>%
    dplyr::group_keys()
}

create_size_summary_ <- function(data, group_col){
  data %>%
    dplyr::ungroup() %>%
    dplyr::select(!!as.name(group_col)) %>%
    dplyr::group_by(!!as.name(group_col)) %>%
    dplyr::summarise(`# rows` = dplyr::n())
}

create_id_summaries_ <- function(data, group_col, id_cols){
  summary <- data %>%
    dplyr::ungroup() %>%
    dplyr::select(!!!rlang::syms(c(group_col, id_cols))) %>%
    dplyr::group_by(!!as.name(group_col)) %>%
    dplyr::summarise(dplyr::across(dplyr::one_of(id_cols), function(x){length(unique(x))})) %>%
    dplyr::rename_with(
      ~ paste0("# ", .),
      -dplyr::one_of(group_col)
    )
  summary
}

create_num_summaries_ <- function(data, group_col, num_cols){
  data %>%
    dplyr::ungroup() %>%
    dplyr::select(!!!rlang::syms(c(group_col, num_cols))) %>%
    dplyr::group_by(!!as.name(group_col)) %>%
    dplyr::summarise(dplyr::across(dplyr::one_of(num_cols),
                                   list("mean" = mean, "sum" = sum)),
                     .groups = "drop") %>%
    # Rename so `xxx_mean <- mean(xxx)` and `xxx_sum <- sum(xxx)`
    dplyr::rename_with(
      ~ gsub(
        pattern = "(^.*)_([[:alnum:]]*)$",
        replacement = "\\2(\\1)",
        x = .
      ),
      where(is.numeric)
    )
}

create_cat_summaries_ <- function(data, group_col, cat_cols, max_cat_prefix_chars = 5){

  tmp_n_var <- create_tmp_var(data, tmp_var = ".n")

  data %>%
    dplyr::ungroup() %>%
    dplyr::select(!!!rlang::syms(c(group_col, cat_cols))) %>%
    dplyr::mutate(dplyr::across(dplyr::one_of(cat_cols), as.character)) %>%
    tidyr::gather(key = "cat_col", value = "cat_val", cat_cols) %>%
    dplyr::count(!!as.name(group_col), .data$cat_col, .data$cat_val,
                 name = tmp_n_var) %>%
    dplyr::mutate(
      cat_col = tolower(.data$cat_col),
      cat_val = tolower(.data$cat_val),
      cat_col = substr(.data$cat_col, start = 1, stop = max_cat_prefix_chars),
      cat_name = paste0(.data$cat_col, "_", .data$cat_val),
      cat_name = gsub("^_", "", .data$cat_name)
    ) %>%
    dplyr::select(dplyr::one_of(group_col, "cat_name", tmp_n_var)) %>%
    tidyr::spread(key = .data$cat_name,
                  value = !!as.name(tmp_n_var),
                  fill = 0) %>%
    dplyr::rename_with(
      ~ paste0("# ", .),
      -dplyr::one_of(group_col)
    )
}


measure_summary_numerics_ <- function(
  data,
  num_cols,
  cat_level_cols,
  id_cols,
  summarize_size,
  ranking_weights,
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
  measures <- add_sd_ranks(
    measures = measures,
    cat_level_cols = cat_level_cols,
    num_cols = num_cols,
    id_cols = id_cols,
    summarize_size = summarize_size,
    ranking_weights = ranking_weights,
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
add_sd_ranks <- function(
  measures,
  cat_level_cols,
  num_cols,
  id_cols,
  summarize_size,
  ranking_weights,
  logged_counts,
  max_cat_prefix_chars,
  max_num_prefix_chars) {

  # Calculate average SD rank
  sd_rows <- measures %>%
    dplyr::filter(measure == "SD")

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

  for (cat_col in names(cat_level_cols)){
    rank_names[[cat_col]] <- paste0(
      substr(cat_col, start = 1, stop = max_cat_prefix_chars),
      "_SD_rank"
    )

    sd_rows <- sd_rows %>%
      mean_rank_numeric_cols(
        cols = cat_level_cols[[cat_col]],
        col_name = rank_names[[cat_col]]
      )
  }

  if (!is.null(num_cols)){
    for (num_col in num_cols){
      # Names of num_cols columns (e.g. mean(score) and sum(score))
      num_col_cols <- colnames(sd_rows)[
        grepl(paste0("\\(", num_col, "\\)"), colnames(sd_rows))]

      rank_names[[num_col]] <- paste0(
        substr(num_col, start = 1, stop = max_num_prefix_chars),
        "_SD_rank"
      )

      # Combined rank for this num_col
      sd_rows <- sd_rows %>%
        mean_rank_numeric_cols(
          cols = num_col_cols,
          col_name = rank_names[[num_col]]
        )
    }
  }

  if (!is.null(ranking_weights)) {
    # Convert names in ranking weights to their names in the output
    names(ranking_weights) <-
      purrr::map(.x = names(ranking_weights), .f = ~ {
        rank_names[[.x]]
      })

    # If any of the elements are not in the weights
    # We add them with a value of 1
    names_to_add <- setdiff(rank_names, names(ranking_weights))
    for (name in names_to_add) {
      ranking_weights[[name]] <- 1
    }
  } else {
    ranking_weights <- rep(1, times = length(rank_names)) %>%
      setNames(rank_names)
  }

  if (is.list(ranking_weights)){
    # Convert to numeric
    ranking_weights <- unlist(ranking_weights,
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
    mean_rank_numeric_cols(
      cols = cols_to_rank,
      col_name = "SD_rank",
      ranking_weights = ranking_weights) %>%
    dplyr::select(.data$.group_col,
                  dplyr::ends_with("SD_rank"))

  measures <- measures %>%
    dplyr::left_join(sd_ranks, by = ".group_col")

  measures
}

# Apply rank() to columns
rank_numeric_cols <- function(data, cols = NULL){
  if (is.null(cols)){
    cols <- data %>%
      dplyr::select(where(is.numeric)) %>%
      colnames()
  }

  data %>%
    dplyr::mutate(dplyr::across(dplyr::one_of(cols), rank))
}

# Create column with weighted-average rank of numeric columns
mean_rank_numeric_cols <- function(data, cols = NULL, col_name = "mean_rank", ranking_weights = NULL){

  if (is.null(cols)){
    cols <- data %>%
      dplyr::select(where(is.numeric)) %>%
      colnames()
  }

  if (is.null(ranking_weights)){
    ranking_weights <- rep(1, times = length(cols)) %>%
      setNames(nm = cols)
  }

  # Calculate average SD rank
  sd_ranks <- data %>%
    rank_numeric_cols(cols = cols)
  sd_ranks[[col_name]] <- sd_ranks %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(cols)) %>%
    purrr::pmap_dbl(.f = weighted_mean_, weights = ranking_weights)

  sd_ranks
}

# Calculates a weighted mean of named numbers
# ... and weights must have same names and length
weighted_mean_ <- function(..., weights){
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(x = weights, lower = 0, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Order the row values and the weights
  r <- c(...)
  r <- r[order(names(r))]
  weights <- weights[order(names(weights))]
  if (!all(names(r) == names(weights))){
    stop("`...` and `weights` must have the exact same names.")
  }

  # Sum to one
  weights <- weights / sum(weights)

  sum(r * weights)
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
  ranking_weights,
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
    x = ranking_weights,
    lower = 0,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 1,
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
  if (!is.null(ranking_weights)){
    checkmate::assert_names(
      x = names(ranking_weights),
      subset.of = c(cat_cols, num_cols, id_cols, "size"),
      add = assert_collection
    )
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
}
