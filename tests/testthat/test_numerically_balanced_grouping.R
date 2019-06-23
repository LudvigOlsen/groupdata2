library(groupdata2)
context("numerically_balanced_group_factor_()")

test_that("numerically_balanced_group_factor_() work with n=2", {

  # Create data frame
  set.seed(1)
  df <- data.frame(
    "participant" = factor(c(1, 3, 5, 6, 7, 8)),
    "score" = c(79,85,140,69,87,92))
  vbf <- numerically_balanced_group_factor_(df, 2, num_col="score")
  df_vbf <- df %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf, factor(c(2,1,1,2,1,2)))
  expect_equal(group_sums$group_sum, c(312,240))

})

test_that("numerically_balanced_group_factor_() works with n=3", {

  # Create data frame
  set.seed(1)
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1,2,1,1,2,3,4),
    "score" = sample(c(1:100), 7)) %>%
    dplyr::mutate(neg_score = score-200,
                  neg_pos_score = score-50)

  # numerically_balanced_group_factor_ on unequal number of data frame rows
  set.seed(1)
  expect_equal(numerically_balanced_group_factor_(df, 3, num_col="score"),
               factor(c(2,3,3,2,1,3,1)))


  set.seed(1)
  nbf1 <- numerically_balanced_group_factor_(df, 3, num_col="neg_score")
  set.seed(1)
  nbf2 <- numerically_balanced_group_factor_(df, 3, num_col="score")
  expect_equal(nbf1,nbf2)

  # add grouping factor to df and get sums of value col
  set.seed(1)
  df_grouped <- df %>%
    dplyr::mutate(.groups = numerically_balanced_group_factor_(df, 3, num_col="score"))

  group_sums <- df_grouped %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))
  # group_sums

  expect_equal(group_sums$group_sum, c(101,102,83))

  # numerically_balanced_group_factor_ on equal number of data frame rows

  df <- df %>% dplyr::filter(dplyr::row_number() != 7)
  set.seed(1)
  expect_equal(numerically_balanced_group_factor_(df, 3, num_col="score"),
               factor(c(2,3,1,2,1,3)))

  set.seed(1)
  nbf1 <- numerically_balanced_group_factor_(df, 3, num_col="neg_score")
  set.seed(1)
  nbf2 <- numerically_balanced_group_factor_(df, 3, num_col="score")
  set.seed(1)
  nbf3 <- numerically_balanced_group_factor_(df, 3, num_col="neg_pos_score")
  expect_equal(nbf1,nbf2)
  expect_equal(nbf1,nbf3)

  # add grouping factor to df and get sums of value col
  set.seed(1)
  df_grouped <- df %>%
    dplyr::mutate(.groups = numerically_balanced_group_factor_(
      df, 3, num_col="score"))

  group_sums <- df_grouped %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(group_sums$group_sum, c(88,102,82))

})


test_that("numerically_balanced_group_factor_() unequal method on small datasets (nrow < n*2)",{

  testthat::skip(message = "Skipping bootstrapped numerical balancing test")

  # In this section we check the unequal_method options on the
  # possible combinations of n_rows and n_folds
  # We find that "first" is best in almost all cases.
  # Running this many combinations also checks for runtime errors


  check_groups <- function(n_rows=5, iter=100, n_folds=3, unequal_method="first"){

    df <- data.frame(
      "x" = factor(1:n_rows))

    group_summaries <- plyr::ldply(1:iter, function(i){

      set.seed(i)
      if (i %% 2 == 0){
        cond <- "uniform"
        df$score <- sample(1:100, n_rows)
      } else {
        cond <- "norm"
        df$score <- rnorm(n_rows)
      }



      df_grouped <- df %>%
        dplyr::mutate(.groups = numerically_balanced_group_factor_(., n_folds,
                                                                   num_col="score",
                                                                   randomize_pairs = T,
                                                                   unequal_method=unequal_method),
                      condition = cond)

      group_summary <- df_grouped %>%
        dplyr::group_by(condition, .groups) %>%
        dplyr::summarize(group_sum = sum(score),
                         group_count = dplyr::n()) %>%
        dplyr::arrange(group_sum) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(.group_rank = 1:dplyr::n(),
                      iter = i)

      group_summary


    })

    group_summaries %>% dplyr::group_by(condition, .group_rank) %>%
      dplyr::summarise(mean_sum = mean(group_sum),
                       mean_count = mean(group_count),
                       sd_sum = sd(group_sum)) %>% dplyr::ungroup()
  }

  compare_hparams <- function(iter = 100,
                              range_n_rows=c(3,20),
                              range_n_folds=c(2,19),
                              unequal_methods=c("first","middle","last"),
                              parallel = FALSE){

    options_n_rows <- range_n_rows[1]:range_n_rows[2]
    options_n_folds <- range_n_folds[1]:range_n_folds[2]

    hparams_grid <- expand.grid(unequal_methods, options_n_rows, options_n_folds,
                                stringsAsFactors = FALSE) %>%
      dplyr::rename(unequal_method = Var1,
                    n_rows = Var2,
                    n_folds = Var3) %>%
      dplyr::filter(n_folds < n_rows) %>%
      dplyr::mutate(combination = 1:dplyr::n()) # %>% head(5)

    # We rank models by size and summarize by rank
    size_ranked_summaries <- plyr::ldply(1:nrow(hparams_grid), .parallel=parallel, function(comb){
      current_hparams <- hparams_grid %>% dplyr::filter(dplyr::row_number() == comb)
      check_groups(n_rows=current_hparams[["n_rows"]],
                   n_folds=current_hparams[["n_folds"]],
                   unequal_method=current_hparams[["unequal_method"]]) %>%
        dplyr::mutate(combination = comb)
    }) %>% dplyr::ungroup()

    aggregated_scores <- size_ranked_summaries %>%
      dplyr::group_by(combination, condition) %>%
      dplyr::summarize(mean_sum_ = mean(mean_sum),
                       sd_sum_ = sd(mean_sum, na.rm = TRUE),
                       iqr_sum_ = IQR(mean_sum, na.rm = TRUE),
                       mean_sd_sum_ = mean(sd_sum),
                       sd_sd_sum_ = sd(sd_sum, na.rm = TRUE),
                       mean_count_ = mean(mean_count),
                       sd_count_ = sd(mean_count, na.rm = TRUE)) %>%
      dplyr::full_join(hparams_grid, by="combination")

    all_runs <- size_ranked_summaries %>%
      dplyr::full_join(hparams_grid, by="combination")

    list("aggregated_scores" = aggregated_scores,
         "all_runs" = all_runs)

  }

  hparams_comparisons <- compare_hparams(iter = 100, parallel = T)
  aggregated_scores <- hparams_comparisons[["aggregated_scores"]] %>%
    dplyr::mutate(case = paste0(n_rows," & ",n_folds))

  table_of_heuristics <- aggregated_scores %>%
    dplyr::group_by(case, condition) %>%
    dplyr::summarise(min_sd = min(sd_sum_)) %>%
    dplyr::left_join(aggregated_scores , by=c("case", "condition", "min_sd"="sd_sum_")) %>%
    dplyr::select(c(case, condition, min_sd, unequal_method, n_rows, n_folds)) %>%
    dplyr::arrange(n_rows, case, condition, min_sd, unequal_method) %>%
    dplyr::group_by(case, condition) %>%
    dplyr::filter(dplyr::row_number() == 1)

  heuristics_map <- plyr::ldply(unique(table_of_heuristics$case), function(cas){
    row_ <- dplyr::filter(table_of_heuristics, case==cas)
    unequal_method_ <- row_$unequal_method[1]

    if (length(unique(row_$unequal_method)) == 1 && unequal_method_ != "first"){
      return(data.frame("case"=cas, "unequal_method"=unequal_method_, stringsAsFactors = F))
    }
  }) %>% dplyr::left_join(table_of_heuristics, by=c("case","unequal_method")) %>%
    dplyr::select(case,unequal_method, n_rows, n_folds) %>%
    dplyr::distinct()

  #heuristics_map
  #table(table_of_heuristics$unequal_method)
  #table(heuristics_map$unequal_method)

  # Basically, the conclusion is that the "first" method is best
  # in all but 14 of the 171 possible combinations of n_rows and n_folds
  #

  # num_combinations <- dplyr::filter(expand.grid(3:20, 2:19), Var2<Var1)


})


test_that("numerically_balanced_group_factor_() work method='l_sizes'", {

  # Create data frame
  set.seed(1)
  df <- data.frame(
    "participant" = factor(c(1, 3, 5, 6, 7, 8)),
    "score" = c(79,85,140,69,87,92))

  vbf <- numerically_balanced_group_factor_(df, 0.5, num_col="score", method = 'l_sizes')
  df_vbf <- df %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf, factor(c(1,2,1,1,2,2)))
  expect_equal(group_sums$group_sum, c(288,264))

  vbf <- numerically_balanced_group_factor_(df, 0.2, num_col="score", method = 'l_sizes')
  df_vbf <- df %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf, factor(c(2,2,2,1,2,2)))
  expect_equal(group_sums$group_sum, c(69,483))

  # With 3 partitions
  vbf <- numerically_balanced_group_factor_(df, c(0.2,0.2), num_col="score",
                                            method = 'l_sizes')
  df_vbf <- df %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf, factor(c(3,1,3,3,2,3)))
  expect_equal(group_sums$group_sum, c(85,87,380))

})


test_that("numerically_balanced_group_factor_() on large datasets", {

  # testthat::skip(message = "Skipping numerical balancing of a large dataset")

  # Create data frame
  set.seed(1)
  df <- data.frame(
    "participant" = factor(rep(1:10, 100)),
    "diagnosis" = factor(rep(rep(c("a","b"), each=5), 100)),
    "score" = runif(1000))

  df$num_balanced_factor_1 <- numerically_balanced_group_factor_(df, 5,
                                                                 num_col="score")

  group_sums <- df %>%
    dplyr::group_by(num_balanced_factor_1) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(group_sums$group_sum, c(100,100,99.8,99.9,99.9), tolerance=1e-3)

  ####

  set.seed(1)
  df_999 <- head(df, 999)

  df_999$num_balanced_factor_2 <- numerically_balanced_group_factor_(df_999,
                                                              n=5,
                                                              num_col="score")


  group_summaries <- df_999 %>%
    dplyr::group_by(num_balanced_factor_2) %>%
    dplyr::summarize(group_sum = sum(score),
                     group_count = dplyr::n())

  expect_equal(group_summaries$group_sum, c(99.9,100,100,98.9,100), tolerance=1e-2)
  expect_equal(group_summaries$group_count, c(200,200,200,199,200), tolerance=1e-2)


})


