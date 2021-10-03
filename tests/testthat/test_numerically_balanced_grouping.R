library(groupdata2)
context("numerically_balanced_group_factor_pairs_()")

#### Using extreme pairing ####

test_that("numerically_balanced_group_factor_pairs_() work with n=2", {

  # Create data frame
  xpectr::set_test_seed(1)
  df <- data.frame(
    "participant" = factor(c(1, 3, 5, 6, 7, 8)),
    "score" = c(79, 85, 140, 69, 87, 92)
  )
  vbf <- numerically_balanced_group_factor_pairs_(df, 2, num_col = "score")
  df_vbf <- df %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf, factor(c(1, 2, 2, 1, 2, 1)))
  expect_equal(group_sums$group_sum, c(240, 312))
})

test_that("numerically_balanced_group_factor_pairs_() works with n=3", {

  # Create data frame
  xpectr::set_test_seed(1)
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4),
    "score" = sample(c(1:100), 7)
  ) %>%
    dplyr::mutate(
      neg_score = score - 200,
      neg_pos_score = score - 50
    )

  # numerically_balanced_group_factor_ on unequal number of data frame rows
  xpectr::set_test_seed(1)
  expect_equal(
    numerically_balanced_group_factor_pairs_(df, 3, num_col = "score"),
    factor(c(1, 3, 2, 3, 1, 2, 1))
  )


  xpectr::set_test_seed(1)
  nbf1 <- numerically_balanced_group_factor_pairs_(df, 3, num_col = "neg_score")
  xpectr::set_test_seed(1)
  nbf2 <- numerically_balanced_group_factor_pairs_(df, 3, num_col = "score")
  expect_equal(nbf1, nbf2)

  # add grouping factor to df and get sums of value col
  xpectr::set_test_seed(1)
  df_grouped <- df %>%
    dplyr::mutate(.groups = numerically_balanced_group_factor_pairs_(df, 3, num_col = "score"))

  group_sums <- df_grouped %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))
  # group_sums

  expect_equal(group_sums$group_sum, c(144, 143, 126))

  # numerically_balanced_group_factor_ on equal number of data frame rows

  df <- df %>% dplyr::filter(dplyr::row_number() != 7)
  xpectr::set_test_seed(1)
  expect_equal(
    numerically_balanced_group_factor_pairs_(df, 3, num_col = "score"),
    factor(c(3, 2, 2, 1, 1, 3))
  )

  xpectr::set_test_seed(1)
  nbf1 <- numerically_balanced_group_factor_pairs_(df, 3, num_col = "neg_score")
  xpectr::set_test_seed(1)
  nbf2 <- numerically_balanced_group_factor_pairs_(df, 3, num_col = "score")
  xpectr::set_test_seed(1)
  nbf3 <- numerically_balanced_group_factor_pairs_(df, 3, num_col = "neg_pos_score")
  expect_equal(nbf1, nbf2)
  expect_equal(nbf1, nbf3)

  # add grouping factor to df and get sums of value col
  xpectr::set_test_seed(1)
  df_grouped <- df %>%
    dplyr::mutate(.groups = numerically_balanced_group_factor_pairs_(
      df, 3,
      num_col = "score"
    ))

  group_sums <- df_grouped %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(group_sums$group_sum, c(109, 94, 113))

})

test_that("numerically_balanced_group_factor_pairs_() unequal method on small datasets (nrow < n*2)", {
  testthat::skip(message = "Skipping bootstrapped numerical balancing test")

  xpectr::set_test_seed(1)

  # In this section we check the unequal_method options on the
  # possible combinations of n_rows and n_folds
  # We find that "first" is best in almost all cases.
  # Running this many combinations also checks for runtime errors


  check_groups <- function(n_rows = 5, iter = 100, n_folds = 3, unequal_method = "first") {
    df <- data.frame("x" = factor(1:n_rows), stringsAsFactors = FALSE)

    group_summaries <- plyr::ldply(1:iter, function(i) {
      xpectr::set_test_seed(i)
      if (i %% 2 == 0) {
        cond <- "uniform"
        df$score <- sample(1:100, n_rows)
      } else {
        cond <- "norm"
        df$score <- rnorm(n_rows)
      }

      df_grouped <- df %>%
        dplyr::mutate(
          .groups = numerically_balanced_group_factor_pairs_(., n_folds,
            num_col = "score",
            unequal_method = unequal_method
          ),
          condition = cond
        )

      group_summary <- df_grouped %>%
        dplyr::group_by(condition, .groups) %>%
        dplyr::summarize(
          group_sum = sum(score),
          group_count = dplyr::n()
        ) %>%
        dplyr::arrange(group_sum) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          .group_rank = 1:dplyr::n(),
          iter = i
        )

      group_summary
    })

    group_summaries %>%
      dplyr::group_by(condition, .group_rank) %>%
      dplyr::summarise(
        mean_sum = mean(group_sum),
        mean_count = mean(group_count),
        sd_sum = sd(group_sum)
      ) %>%
      dplyr::ungroup()
  }

  compare_hparams <- function(iter = 100,
                              range_n_rows = c(3, 20),
                              range_n_folds = c(2, 19),
                              unequal_methods = c("first", "last"),
                              parallel = FALSE) {
    options_n_rows <- range_n_rows[1]:range_n_rows[2]
    options_n_folds <- range_n_folds[1]:range_n_folds[2]

    hparams_grid <- expand.grid(unequal_methods, options_n_rows, options_n_folds,
      stringsAsFactors = FALSE
    ) %>%
      dplyr::rename(
        unequal_method = Var1,
        n_rows = Var2,
        n_folds = Var3
      ) %>%
      dplyr::filter(n_folds < n_rows) %>%
      dplyr::mutate(combination = 1:dplyr::n()) # %>% head(5)

    # We rank models by size and summarize by rank
    size_ranked_summaries <- plyr::ldply(seq_len(nrow(hparams_grid)),
      .parallel = parallel, function(comb) {
        current_hparams <- hparams_grid %>% dplyr::filter(dplyr::row_number() == comb)
        check_groups(
          n_rows = current_hparams[["n_rows"]],
          n_folds = current_hparams[["n_folds"]],
          unequal_method = current_hparams[["unequal_method"]]
        ) %>%
          dplyr::mutate(combination = comb)
      }
    ) %>% dplyr::ungroup()

    aggregated_scores <- size_ranked_summaries %>%
      dplyr::group_by(combination, condition) %>%
      dplyr::summarize(
        mean_sum_ = mean(mean_sum),
        sd_sum_ = sd(mean_sum, na.rm = TRUE),
        iqr_sum_ = IQR(mean_sum, na.rm = TRUE),
        mean_sd_sum_ = mean(sd_sum),
        sd_sd_sum_ = sd(sd_sum, na.rm = TRUE),
        mean_count_ = mean(mean_count),
        sd_count_ = sd(mean_count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::full_join(hparams_grid, by = "combination")

    all_runs <- size_ranked_summaries %>%
      dplyr::full_join(hparams_grid, by = "combination")

    list(
      "aggregated_scores" = aggregated_scores,
      "all_runs" = all_runs
    )
  }

  hparams_comparisons <- compare_hparams(iter = 100, parallel = T)
  aggregated_scores <- hparams_comparisons[["aggregated_scores"]] %>%
    dplyr::mutate(case = paste0(n_rows, " & ", n_folds))

  table_of_heuristics <- aggregated_scores %>%
    dplyr::group_by(case, condition) %>%
    dplyr::summarise(min_sd = min(sd_sum_)) %>%
    dplyr::left_join(aggregated_scores, by = c("case", "condition", "min_sd" = "sd_sum_")) %>%
    dplyr::select(c(case, condition, min_sd, unequal_method, n_rows, n_folds)) %>%
    dplyr::arrange(n_rows, case, condition, min_sd, unequal_method) %>%
    dplyr::group_by(case, condition) %>%
    dplyr::filter(dplyr::row_number() == 1)

  heuristics_map <- plyr::ldply(unique(table_of_heuristics$case), function(cas) {
    row_ <- dplyr::filter(table_of_heuristics, case == cas)
    unequal_method_ <- row_$unequal_method[1]

    if (length(unique(row_$unequal_method)) == 1 && unequal_method_ != "first") {
      return(data.frame("case" = cas, "unequal_method" = unequal_method_, stringsAsFactors = F))
    }
  }) %>%
    dplyr::left_join(table_of_heuristics, by = c("case", "unequal_method")) %>%
    dplyr::select(case, unequal_method, n_rows, n_folds) %>%
    dplyr::distinct()

  # heuristics_map
  # table(table_of_heuristics$unequal_method)
  # table(heuristics_map$unequal_method)

  # Basically, the conclusion is that the "first" method is best
  # in all but 14 of the 171 possible combinations of n_rows and n_folds
  #

  # num_combinations <- dplyr::filter(expand.grid(3:20, 2:19), Var2<Var1)
})

test_that("numerically_balanced_group_factor_pairs_() work method='l_sizes'", {

  # Create data frame
  xpectr::set_test_seed(2)
  df <- data.frame(
    "participant" = factor(c(1, 3, 5, 6, 7, 8)),
    "score" = c(79, 85, 140, 69, 87, 92)
  )

  vbf <- numerically_balanced_group_factor_pairs_(df, 0.5, num_col = "score", method = "l_sizes")
  df_vbf <- df %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf, factor(c(2, 2, 1, 1, 2, 1)))
  expect_equal(group_sums$group_sum, c(301, 251))

  xpectr::set_test_seed(4)
  vbf <- numerically_balanced_group_factor_pairs_(df, 0.2, num_col = "score", method = "l_sizes")
  df_vbf <- df %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf, factor(c(2L, 2L, 2L, 2L, 1L, 2L)))
  expect_equal(group_sums$group_sum, c(87, 465))

  xpectr::set_test_seed(19)
  # With 3 partitions
  vbf <- numerically_balanced_group_factor_pairs_(df, c(0.2, 0.2),
    num_col = "score",
    method = "l_sizes"
  )
  df_vbf <- df %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf, factor(c(3, 3, 2, 1, 3, 3)))
  expect_equal(group_sums$group_sum, c(69, 140, 343))
})

test_that("numerically_balanced_group_factor_pairs_() on large datasets", {

  # testthat::skip(message = "Skipping numerical balancing of a large dataset")

  # Create data frame
  xpectr::set_test_seed(1)
  df <- data.frame(
    "participant" = factor(rep(1:10, 100)),
    "diagnosis" = factor(rep(rep(c("a", "b"), each = 5), 100)),
    "score" = runif(1000)
  )

  df$num_balanced_factor_1 <- numerically_balanced_group_factor_pairs_(df, 5,
    num_col = "score"
  )

  group_sums <- df %>%
    dplyr::group_by(num_balanced_factor_1) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(group_sums$group_sum, c(100, 100, 99.9, 99.7, 100), tolerance = 1e-3)

  ####

  xpectr::set_test_seed(6)
  df_999 <- head(df, 999)

  df_999$num_balanced_factor_2 <- numerically_balanced_group_factor_pairs_(df_999,
    n = 5,
    num_col = "score"
  )

  group_summaries <- df_999 %>%
    dplyr::group_by(num_balanced_factor_2) %>%
    dplyr::summarize(
      group_sum = sum(score),
      group_count = dplyr::n()
    )

  expect_equal(
    group_summaries$group_sum,
    c(99.91175, 99.84989, 99.87336, 99.82958, 99.96152),
    tolerance = 1e-4
  )
  expect_equal(group_summaries$group_count,
               c(200, 199, 200, 200, 200),
               tolerance = 1e-3)
})

test_that("experiment: numerically_balanced_group_factor_pairs_() optimizes for sd", {
  testthat::skip("Simulation that runs for a long time")
  # Create data frame
  xpectr::set_test_seed(5)

  # NOTE:
  # It shouldn't actually work to combine those with little and lots of variation
  # as their means will matter a lot more in the variation of the combinations?


  # library(ggplot2)
  # library(dplyr)
  # library(doParallel)
  # registerDoParallel(7)

  # TODO Probably run this in parallel ;)
  group_sums <- plyr::ldply(seq_len(1000), .parallel = TRUE, function(i) {
    df <- data.frame(
      "participant" = factor(1:600),
      "score" = c(runif(200, 0, 30), rnorm(200, 70, 110), runif(200, -70, -30))
    )

    df_with_grouping_factors <- plyr::ldply(1:10, function(g) {
      dplyr::bind_cols(list(
        df,
        numerically_balanced_group_factor_pairs_(df, 2,
          num_col = "score",
          optimize_for = c("mean", "mean", "mean"),
          extreme_pairing_levels = 3
        ) %>%
          tibble::enframe(name = NULL, value = "mmm"),
        numerically_balanced_group_factor_pairs_(df, 2,
          num_col = "score",
          optimize_for = c("mean", "mean", "sd"),
          extreme_pairing_levels = 3
        ) %>%
          tibble::enframe(name = NULL, value = "mms"),
        numerically_balanced_group_factor_pairs_(df, 2,
          num_col = "score",
          optimize_for = c("mean", "sd", "sd"),
          extreme_pairing_levels = 3
        ) %>%
          tibble::enframe(name = NULL, value = "mss"),
        numerically_balanced_group_factor_pairs_(df, 2,
          num_col = "score",
          optimize_for = c("mean", "sd", "mean"),
          extreme_pairing_levels = 3
        ) %>%
          tibble::enframe(name = NULL, value = "msm")
      )) %>%
        dplyr::mutate(sim = g)
    })

    group_sums <- df_with_grouping_factors %>%
      tidyr::gather(key = "opt_for", value = ".groups", 3:6) %>%
      dplyr::group_by(sim, opt_for, .groups) %>%
      dplyr::summarize(
        group_sum = sum(score),
        group_sd = sd(score)
      )

    group_sums
  })

  group_sums <- group_sums %>%
    dplyr::mutate(sampling = rep(1:1000, each = 80))
  # write.csv(group_sums, "numerical_balancing_simulation_scores.csv")

  sim_wise_summaries <- group_sums %>%
    dplyr::group_by(sampling, sim, opt_for) %>%
    dplyr::summarise(
      diff_sum = abs(diff(group_sum)),
      diff_sd = abs(diff(group_sd))
    )

  # tiff("DiffGroupSums.tiff", units="in", width=5, height=6, res=300)
  sim_wise_summaries %>%
    ggplot(aes(x = opt_for, y = diff_sum)) +
    geom_boxplot() +
    labs(
      x = "Optimize for (m = mean, s = sd)",
      y = "Absolute Difference between Group Sums"
    ) +
    theme_light()
  # dev.off()

  # tiff("DiffGroupSDs.tiff", units="in", width=5, height=6, res=300)
  sim_wise_summaries %>%
    ggplot(aes(x = opt_for, y = diff_sd)) +
    geom_boxplot() +
    labs(
      x = "Optimize for (m = mean, s = sd)",
      y = "Absolute Difference between Group SDs"
    ) +
    theme_light()
  # dev.off()

  # 'sampling' is the random dataset
  summary(lmerTest::lmer(diff_sum ~ opt_for + (1 | sampling), data = sim_wise_summaries))
  summary(lmerTest::lmer(diff_sd ~ opt_for + (1 | sampling), data = sim_wise_summaries))
})


#### Using extreme triplet grouping ####


test_that("numerically_balanced_group_factor_triplets_() work with n=2", {

  # Create data frame
  xpectr::set_test_seed(1)
  df <- data.frame(
    "participant" = factor(c(1, 3, 5, 6, 7, 8)),
    "score" = c(79, 85, 140, 69, 87, 92)
  )

  # Vanilla settings
  xpectr::set_test_seed(1)
  vbf <- numerically_balanced_group_factor_triplets_(df, 2, num_col = "score")
  df_vbf <- df %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf, factor(c(2, 1, 1, 1, 2, 2)))
  expect_equal(group_sums$group_sum, c(294, 258))

  ## df is not divisible by three

  xpectr::set_test_seed(1)
  df_5 <- head(df, 5)
  vbf <- numerically_balanced_group_factor_triplets_(df_5, 2, num_col = "score")
  df_vbf <- df_5 %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf, factor(c(1, 2, 2, 1, 1)))
  expect_equal(group_sums$group_sum, c(235, 225))


  ## With extreme pairing (which df is too small for)

  ## Testing 'numerically_balanced_group_factor_triplets_(...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_15728 <- xpectr::capture_side_effects(numerically_balanced_group_factor_triplets_(df, 2, num_col = "score", extreme_grouping_levels = 2), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_15728[['error']]),
    xpectr::strip("`num_col`: The (subset of) data is too small to perform 2 levels of extreme triplet groupings. Decrease `extreme_grouping_levels`."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_15728[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'numerically_balanced_group_factor_triplets_(...'     ####


  # Create data frame
  xpectr::set_test_seed(1)
  df_medium <- data.frame(
    "participant" = factor(1:29),
    "score" = c(79, 85, 140, 69, 87, 92, 32, 56, 76, 23, 59, 18, 49, 22, 28, 23, 98, 101, 57, 30,
                71, 82, 167, 201, 8, 91, 48, 100, 81)
  )

  xpectr::set_test_seed(1)
  vbf <- numerically_balanced_group_factor_triplets_(df_medium, 2, num_col = "score", extreme_grouping_levels = 2)
  df_vbf <- df_medium %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf,
               factor(
                 c(2, 2, 2, 2, 2, 1, 1, 2, 2, 1, 1, 2, 1,
                   2, 2, 2, 1, 2, 1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 1)
               ))
  expect_equal(group_sums$group_sum, c(998, 1075))

  # With 27 rows instead

  xpectr::set_test_seed(1)
  df_medium_27 <- head(df_medium, 27)
  vbf <- numerically_balanced_group_factor_triplets_(df_medium_27, 2, num_col = "score", extreme_grouping_levels = 2)
  df_vbf <- df_medium_27 %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf,
               factor(c(
                 1, 2, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1,
                 2, 1, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2
               )))
  expect_equal(group_sums$group_sum, c(953, 939))

})

test_that("numerically_balanced_group_factor_triplets_() work with n=3", {

  # Create data frame
  xpectr::set_test_seed(1)
  df <- data.frame(
    "participant" = factor(c(1, 3, 5, 6, 7, 8)),
    "score" = c(79, 85, 140, 69, 87, 92)
  )

  # Vanilla settings
  xpectr::set_test_seed(1)
  vbf <- numerically_balanced_group_factor_triplets_(df, 3, num_col = "score")
  df_vbf <- df %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf, factor(c(1, 3, 3, 2, 2, 1)))
  expect_equal(group_sums$group_sum, c(171, 156, 225))

  ## df is not divisible by three

  xpectr::set_test_seed(2)
  df_5 <- head(df, 5)
  vbf <- numerically_balanced_group_factor_triplets_(df_5, 3, num_col = "score")
  df_vbf <- df_5 %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf, factor(c(1, 2, 3, 1, 2)))
  expect_equal(group_sums$group_sum, c(148, 172, 140))


  ## With extreme pairing (which df is too small for)


  ## Testing 'numerically_balanced_group_factor_triplets_(...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19782 <- xpectr::capture_side_effects(numerically_balanced_group_factor_triplets_(df, 3, num_col = "score", extreme_grouping_levels = 2), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19782[['warnings']]),
    xpectr::strip(character(0)),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19782[['messages']]),
    xpectr::strip("`extreme_grouping_levels` was reduced to 1 during extreme triplets numerical balancing.\n"),
    fixed = TRUE)
  # Assigning output
  output_19782 <- xpectr::suppress_mw(numerically_balanced_group_factor_triplets_(df, 3, num_col = "score", extreme_grouping_levels = 2))
  # Testing is factor
  expect_true(
    is.factor(output_19782))
  # Testing values
  expect_equal(
    as.character(output_19782),
    c("2", "3", "2", "3", "1", "1"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_19782),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19782),
    6L)
  # Testing number of levels
  expect_equal(
    nlevels(output_19782),
    3L)
  # Testing levels
  expect_equal(
    levels(output_19782),
    c("1", "2", "3"),
    fixed = TRUE)
  ## Finished testing 'numerically_balanced_group_factor_triplets_(...'     ####



  # Create data frame
  xpectr::set_test_seed(1)
  df_medium <- data.frame(
    "participant" = factor(1:29),
    "score" = c(79, 85, 140, 69, 87, 92, 32, 56, 76, 23, 59, 18, 49, 22, 28, 23, 98, 101, 57, 30,
                71, 82, 167, 201, 8, 91, 48, 100, 81)
  )

  xpectr::set_test_seed(1)
  vbf <- numerically_balanced_group_factor_triplets_(df_medium, 3, num_col = "score", extreme_grouping_levels = 2)
  df_vbf <- df_medium %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf,
               factor(c(2, 2, 2, 2, 2, 1, 1, 2, 1, 3, 1, 3, 3,
               1, 2, 2, 1, 2, 3, 3, 3, 1, 3, 1, 2, 3, 1, 3, 3
               )))
  expect_equal(group_sums$group_sum, c(710, 676, 687))

  # With 27 rows instead

  xpectr::set_test_seed(1)
  df_medium_27 <- head(df_medium, 27)
  vbf <- numerically_balanced_group_factor_triplets_(df_medium_27, 3, num_col = "score", extreme_grouping_levels = 2)
  df_vbf <- df_medium_27 %>%
    dplyr::mutate(.groups = vbf)

  group_sums <- df_vbf %>%
    dplyr::group_by(.groups) %>%
    dplyr::summarize(group_sum = sum(score))

  expect_equal(vbf,
               factor(c(1, 2, 2, 1, 1, 3, 1, 3, 3, 2, 3, 3, 1,
               2, 3, 3, 3, 2, 2, 1, 2, 2, 3, 1, 1, 1, 2)))
  expect_equal(group_sums$group_sum, c(646, 629, 617))

})

test_that("testing calculate_excessive_ids_()", {
  xpectr::set_test_seed(42)

  df <- data.frame(
    "id" = factor(rep(1:7, 3))
  )

  # We have 7 IDs, so we can make 3 groups of 2
  # and have 1 excess ID
  expect_equal(
    calculate_excessive_ids_(
      data = df,
      id_col = 'id',
      num_groups = 2,
      num_known_excessive = 0
    ),
    1
  )

  # We have 7 IDs, so we can make 3 groups of 2
  # If we have 1 known excess element it does not break one of the final groups
  # and so we still just have 1 excess ID
  expect_equal(
    calculate_excessive_ids_(
      data = df,
      id_col = 'id',
      num_groups = 2,
      num_known_excessive = 1
    ),
    1
  )

  # We have 7 IDs but 2 known/predetermined excess elements
  # so we can make 2 groups of 2 and have 3 elements to redistribute
  expect_equal(
    calculate_excessive_ids_(
      data = df,
      id_col = 'id',
      num_groups = 2,
      num_known_excessive = 2
    ),
    3
  )

  # We have 7 IDs but 3 known/predetermined excess elements
  # so we can make 2 groups of 2 and have 3 elements to redistribute
  expect_equal(
    calculate_excessive_ids_(
      data = df,
      id_col = 'id',
      num_groups = 2,
      num_known_excessive = 3
    ),
    3
  )

})

test_that("testing aggregate_groups_()", {
  xpectr::set_test_seed(42)

  df_medium <- data.frame(
    "participant" = factor(1:29),
    "score" = c(79, 85, 140, 69, 87, 92, 32, 56, 76, 23, 59, 18, 49, 22, 28, 23, 98, 101, 57, 30,
                71, 82, 167, 201, 8, 91, 48, 100, 81),
    "rearrange_var" = factor(c(rep(1:9, each=3), c(10,10)))
  )

  # Check manually

  expected_aggregates <- df_medium %>%
    dplyr::group_by(.data$rearrange_var) %>%
    dplyr::summarise(score = sum(.data$score)) %>%
    dplyr::arrange(.data$score)

  output_11213 <- aggregate_groups_(data=df_medium, final_rearrange_var="rearrange_var", num_col="score", optimize_for="mean")
  expect_equal(output_11213$two_smallest, expected_aggregates[1:2,]$score)
  expect_equal(output_11213$two_largest, expected_aggregates[9:10,]$score)
  expect_equal(output_11213$smallest_group_id, as.numeric(expected_aggregates$rearrange_var[[1]]))
  expect_equal(output_11213$largest_group_id, as.numeric(expected_aggregates$rearrange_var[[10]]))
  expect_equal(output_11213$sd_aggregate, sd(expected_aggregates$score))

  # Check entire list

  ## Testing 'aggregate_groups_(data=df_medium, final_rear...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_11346 <- aggregate_groups_(data=df_medium, final_rearrange_var="rearrange_var", num_col="score", optimize_for="mean")
  # Testing class
  expect_equal(
    class(output_11346),
    "list",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_11346,
    type = "list")
  # Testing values
  expect_equal(
    output_11346[["sd_aggregate"]],
    106.45923,
    tolerance = 1e-4)
  expect_equal(
    output_11346[["two_smallest"]],
    c(99, 100),
    tolerance = 1e-4)
  expect_equal(
    output_11346[["two_largest"]],
    c(304, 450),
    tolerance = 1e-4)
  expect_equal(
    output_11346[["smallest_group_id"]],
    5,
    tolerance = 1e-4)
  expect_equal(
    output_11346[["largest_group_id"]],
    8,
    tolerance = 1e-4)
  # Testing names
  expect_equal(
    names(output_11346),
    c("sd_aggregate", "two_smallest", "two_largest", "smallest_group_id",
      "largest_group_id"),
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_11346),
    5L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_11346)),
    7L)
  # Testing element classes
  expect_equal(
    xpectr::element_classes(output_11346),
    c("numeric", "numeric", "numeric", "numeric", "numeric"),
    fixed = TRUE)
  # Testing element types
  expect_equal(
    xpectr::element_types(output_11346),
    c("double", "double", "double", "double", "double"),
    fixed = TRUE)
  ## Finished testing 'aggregate_groups_(data=df_medium, final_rear...'     ####


  # Check single group data frame

  ## Testing 'aggregate_groups_(data=head(df_medium, 3), f...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_14622 <- xpectr::capture_side_effects(aggregate_groups_(data=head(df_medium, 3), final_rearrange_var="rearrange_var", num_col="score", optimize_for="mean"), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14622[['error']]),
    xpectr::strip("`aggregate_groups_` found only a single group. `data` must contain multiple groups."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14622[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'aggregate_groups_(data=head(df_medium, 3), f...'     ####


})

test_that("testing get_redistribution_info()", {
  xpectr::set_test_seed(42)

  df_medium <- data.frame(
    "participant" = factor(1:29),
    "score" = c(79, 85, 140, 69, 87, 92, 32, 56, 76, 23, 59, 18, 49, 22, 28, 23, 98, 101, 57, 30,
                71, 82, 167, 201, 8, 91, 48, 100, 81),
    "rearrange_var" = factor(c(rep(1:9, each=3), c(10,10)))
  )

  # Check aggregates
  # We see that the two largest scores have 1+ SD between them,
  # so at least one excessive ID should (if possible) be considered excessive
  aggregates <- aggregate_groups_(data=df_medium, final_rearrange_var="rearrange_var", num_col="score", optimize_for="mean")

  # Check number of excess IDs
  expect_equal(
    calculate_excessive_ids_(
      data = df_medium,
      id_col = 'rearrange_var',
      num_groups = 4,
      num_known_excessive = 1
    ),
    2
  )

  # In the below, we see that the largest group is set to be redistributed (as expected)
  # and that we have 2 excessive IDs in total
  # also the smallest and largest group IDs match those from the aggregates
  # The largest possible number of IDs to redistribute is 10 (num ids) - 4 (num groups) == 6

  ## Testing 'get_redistribution_info_( data = df_medium, ...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- get_redistribution_info_(
      data = df_medium,
      num_col = "score",
      num_final_groups = 4,
      final_rearrange_var = "rearrange_var",
      optimize_for = "mean"
    )
  # Testing class
  expect_equal(
    class(output_19148),
    "list",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19148,
    type = "list")
  # Testing values
  expect_equal(
    output_19148,
    list(has_excessive_rearrange_ids = TRUE, redistribute_smallest_group = FALSE,
      redistribute_largest_group = TRUE, num_excessive_rearrange_ids = 2,
      smallest_group_id = 5, largest_group_id = 8, num_possible_to_redistribute = 6))
  # Testing names
  expect_equal(
    names(output_19148),
    c("has_excessive_rearrange_ids", "redistribute_smallest_group",
      "redistribute_largest_group", "num_excessive_rearrange_ids",
      "smallest_group_id", "largest_group_id", "num_possible_to_redistribute"),
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    7L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19148)),
    7L)
  ## Finished testing 'get_redistribution_info_( data = df_medium, ...'     ####


  df_medium_27 <- head(df_medium, 27)

  # Check aggregates
  output_19148 <- aggregate_groups_(data=df_medium_27, final_rearrange_var="rearrange_var", num_col="score", optimize_for="mean")
  expect_equal(
    output_19148[["sd_aggregate"]],
    112.49086,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["two_smallest"]],
    c(99, 100),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["two_largest"]],
    c(304, 450),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["smallest_group_id"]],
    5,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["largest_group_id"]],
    8,
    tolerance = 1e-4)

  # So there's 1+ SD between the two largest groups
  # Check number of excess IDs
  expect_equal(
    calculate_excessive_ids_(
      data = df_medium_27,
      id_col = 'rearrange_var',
      num_groups = 4,
      num_known_excessive = 1
    ),
    1
  )

  # The below correctly states to redistribute the largest group
  # has the same number of excessive IDs as above
  # and correctly states the IDs of the smallest and largest groups
  # The largest possible number of IDs to redistribute is 9 (num ids) - 4 (num groups) == 5

  ## Testing 'output_19148 <- get_redistribution_info_( da...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- output_19148 <- get_redistribution_info_(
      data = df_medium_27,
      num_col = "score",
      num_final_groups = 4,
      final_rearrange_var = "rearrange_var",
      optimize_for = "mean"
    )
  # Testing class
  expect_equal(
    class(output_19148),
    "list",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19148,
    type = "list")
  # Testing values
  expect_equal(
    output_19148,
    list(has_excessive_rearrange_ids = TRUE, redistribute_smallest_group = FALSE,
      redistribute_largest_group = TRUE, num_excessive_rearrange_ids = 1,
      smallest_group_id = 5, largest_group_id = 8, num_possible_to_redistribute = 5))
  # Testing names
  expect_equal(
    names(output_19148),
    c("has_excessive_rearrange_ids", "redistribute_smallest_group",
      "redistribute_largest_group", "num_excessive_rearrange_ids",
      "smallest_group_id", "largest_group_id", "num_possible_to_redistribute"),
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    7L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19148)),
    7L)
  ## Finished testing 'output_19148 <- get_redistribution_info_( da...'     ####


  # With fewer unique IDs than the number of final groups

  df_too_small <- head(df_medium, 9)

  ## Testing 'redist_info <- get_redistribution_info_( dat...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_11404 <- xpectr::capture_side_effects(redist_info <- get_redistribution_info_(
      data = df_too_small,
      num_col = "score",
      num_final_groups = 7,
      final_rearrange_var = "rearrange_var",
      optimize_for = "mean"
    ), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_11404[['error']]),
    xpectr::strip("Found fewer unique IDs in `data[[num_col]]` than the number of groups to create."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_11404[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'redist_info <- get_redistribution_info_( dat...'     ####


  #### Num groups add to up the number of IDs ####

  # But the last group is 1SD+ smaller than the second next

  df_medium_22_edited <- df_medium %>%
    head(22) %>%
    dplyr::mutate(score = ifelse(.data$rearrange_var == 8, 1, .data$score))

  redist_info <- get_redistribution_info_(
    data = df_medium_22_edited,
    num_col = "score",
    num_final_groups = 4,
    final_rearrange_var = "rearrange_var",
    optimize_for = "mean"
  )

  # Due to the redistribution of the smallest group (8)
  # We need to redistribute 3 more (4 in total)
  # The largest possible number of IDs to redistribute is 8 (num ids) - 4 (num groups) == 4

  ## Testing 'redist_info'                                                  ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(redist_info),
    "list",
    fixed = TRUE)
  # Testing type
  expect_type(
    redist_info,
    type = "list")
  # Testing values
  expect_equal(
    redist_info,
    list(has_excessive_rearrange_ids = TRUE, redistribute_smallest_group = TRUE,
      redistribute_largest_group = FALSE, num_excessive_rearrange_ids = 4,
      smallest_group_id = 8, largest_group_id = 1, num_possible_to_redistribute = 4))
  # Testing names
  expect_equal(
    names(redist_info),
    c("has_excessive_rearrange_ids", "redistribute_smallest_group",
      "redistribute_largest_group", "num_excessive_rearrange_ids",
      "smallest_group_id", "largest_group_id", "num_possible_to_redistribute"),
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(redist_info),
    7L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(redist_info)),
    7L)
  ## Finished testing 'redist_info'                                         ####


  # When the last group is 1SD+ smaller than the second next
  # BUT we can't redistribute any as num_final_groups == num IDs
  # I.e. the largest possible number of IDs to redistribute is 8 (num ids) - 8 (num groups) == 0

  redist_info <- get_redistribution_info_(
    data = df_medium_22_edited,
    num_col = "score",
    num_final_groups = 8,
    final_rearrange_var = "rearrange_var",
    optimize_for = "mean"
  )

  # We see that num_excessive_rearrange_ids is 0 as expected:


  ## Testing 'redist_info'                                                  ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(redist_info),
    "list",
    fixed = TRUE)
  # Testing type
  expect_type(
    redist_info,
    type = "list")
  # Testing values
  expect_equal(
    redist_info,
    list(has_excessive_rearrange_ids = FALSE, redistribute_smallest_group = FALSE,
      redistribute_largest_group = FALSE, num_excessive_rearrange_ids = 0,
      smallest_group_id = NULL, largest_group_id = NULL, num_possible_to_redistribute = 0))
  # Testing names
  expect_equal(
    names(redist_info),
    c("has_excessive_rearrange_ids", "redistribute_smallest_group",
      "redistribute_largest_group", "num_excessive_rearrange_ids",
      "smallest_group_id", "largest_group_id", "num_possible_to_redistribute"),
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(redist_info),
    7L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(redist_info)),
    5L)
  ## Finished testing 'redist_info'                                         ####


})

test_that("testing get_excessive_rows_()", {
  xpectr::set_test_seed(42)

  df_medium <- data.frame(
    "participant" = factor(1:29),
    "score" = c(79, 85, 140, 69, 87, 92, 32, 56, 76, 23, 59, 18, 49, 22, 28, 23, 98, 101, 57, 30,
                71, 82, 167, 201, 8, 91, 48, 100, 81),
    "rearrange_var" = factor(c(rep(1:9, each=3), c(10,10)))
  )

  df_medium_27 <- df_medium %>% head(27)

  redist_info <- get_redistribution_info_(
    data = df_medium_27,
    num_col = "score",
    num_final_groups = 4,
    final_rearrange_var = "rearrange_var",
    optimize_for = "mean"
  )

  expect_false(redist_info$redistribute_smallest_group)
  expect_true(redist_info$redistribute_largest_group)
  expect_equal(redist_info$largest_group_id, 8)
  expect_equal(redist_info$num_excessive_rearrange_ids, 1)

  # Based on the above the excessive rows should contain the
  # members of the largest group (8) only

  excess <- get_excessive_rows_(
    data = df_medium_27,
    num_col = "score",
    num_final_groups = 4,
    final_rearrange_var = "rearrange_var",
    nrows_divisible = FALSE,
    nrows_nondivisible_id = 1,
    optimize_for = "mean"
  )

  expect_equal(
    names(excess),
    c("has_excessive_rearrange_ids", "data", "rows_to_distribute"),
    fixed = TRUE
  )

  ## Testing 'excess[["has_excessive_rearrange_ids"]]'                      ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- excess[["has_excessive_rearrange_ids"]]
  # Testing class
  expect_equal(
    class(output_19148),
    "logical",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19148,
    type = "logical")
  # Testing values
  expect_equal(
    output_19148,
    TRUE)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19148)),
    1L)
  ## Finished testing 'excess[["has_excessive_rearrange_ids"]]'             ####

  # Check the non-excess data

  ## Testing 'excess[["data"]]'                                             ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(excess[["data"]]),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    excess[["data"]][["participant"]],
    structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L,
      14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 25L, 26L, 27L), .Label = c("1",
      "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
      "14", "15", "16", "17", "18", "19", "20", "21", "22", "23",
      "24", "25", "26", "27", "28", "29"), class = "factor"))
  expect_equal(
    excess[["data"]][["score"]],
    c(79, 85, 140, 69, 87, 92, 32, 56, 76, 23, 59, 18, 49, 22, 28, 23,
      98, 101, 57, 30, 71, 8, 91, 48),
    tolerance = 1e-4)
  expect_equal(
    excess[["data"]][["rearrange_var"]],
    structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
      5L, 5L, 6L, 6L, 6L, 7L, 7L, 7L, 9L, 9L, 9L), .Label = c("1",
      "2", "3", "4", "5", "6", "7", "8", "9", "10"), class = "factor"))
  # Testing column names
  expect_equal(
    names(excess[["data"]]),
    c("participant", "score", "rearrange_var"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(excess[["data"]]),
    c("factor", "numeric", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(excess[["data"]]),
    c("integer", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(excess[["data"]]),
    c(24L, 3L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(excess[["data"]])),
    character(0),
    fixed = TRUE)
  ## Finished testing 'excess[["data"]]'                                    ####

  # Check that the largest group is not in the non-excess data
  expect_true("8" %ni% as.character(excess[["data"]][["rearrange_var"]]))

  # Check the excess data
  # We correctly see that the largest group (8)
  # had all three elements placed in the excess rows data frame

  ## Testing 'excess[["rows_to_distribute"]]'                               ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(excess[["rows_to_distribute"]]),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    excess[["rows_to_distribute"]][["participant"]],
    structure(22:24, .Label = c("1", "2", "3", "4", "5", "6", "7", "8",
      "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
      "20", "21", "22", "23", "24", "25", "26", "27", "28", "29"),
      class = "factor"))
  expect_equal(
    excess[["rows_to_distribute"]][["score"]],
    c(82, 167, 201),
    tolerance = 1e-4)
  expect_equal(
    excess[["rows_to_distribute"]][["rearrange_var"]],
    structure(c(8L, 8L, 8L), .Label = c("1", "2", "3", "4", "5", "6",
      "7", "8", "9", "10"), class = "factor"))
  # Testing column names
  expect_equal(
    names(excess[["rows_to_distribute"]]),
    c("participant", "score", "rearrange_var"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(excess[["rows_to_distribute"]]),
    c("factor", "numeric", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(excess[["rows_to_distribute"]]),
    c("integer", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(excess[["rows_to_distribute"]]),
    c(3L, 3L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(excess[["rows_to_distribute"]])),
    character(0),
    fixed = TRUE)
  ## Finished testing 'excess[["rows_to_distribute"]]'                      ####


  # Too many groups for the number of IDs

  df_medium_22 <- head(df_medium, 22)

  redist_info <- get_redistribution_info_(
    data = df_medium_22,
    num_col = "score",
    num_final_groups = 7,
    final_rearrange_var = "rearrange_var",
    optimize_for = "mean"
  )

  # Based on the above the excessive rows should contain the
  # nrows_nondivisible_id (3) only

  excess <- get_excessive_rows_(
    data = df_medium_22,
    num_col = "score",
    num_final_groups = 7,
    final_rearrange_var = "rearrange_var",
    nrows_divisible = FALSE,
    nrows_nondivisible_id = 3,
    optimize_for = "mean"
  )

  expect_equal(
    names(excess),
    c("has_excessive_rearrange_ids", "data", "rows_to_distribute"),
    fixed = TRUE
  )

  # Check ID 3 not in the non-excess data
  expect_true("3" %ni% as.character(excess[["data"]][["rearrange_var"]]))

  ## Testing 'excess[["rows_to_distribute"]]'                               ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(excess[["rows_to_distribute"]]),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    excess[["rows_to_distribute"]][["participant"]],
    structure(7:9, .Label = c("1", "2", "3", "4", "5", "6", "7", "8",
      "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
      "20", "21", "22", "23", "24", "25", "26", "27", "28", "29"),
      class = "factor"))
  expect_equal(
    excess[["rows_to_distribute"]][["score"]],
    c(32, 56, 76),
    tolerance = 1e-4)
  expect_equal(
    excess[["rows_to_distribute"]][["rearrange_var"]],
    structure(c(3L, 3L, 3L), .Label = c("1", "2", "3", "4", "5", "6",
      "7", "8", "9", "10"), class = "factor"))
  # Testing column names
  expect_equal(
    names(excess[["rows_to_distribute"]]),
    c("participant", "score", "rearrange_var"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(excess[["rows_to_distribute"]]),
    c("factor", "numeric", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(excess[["rows_to_distribute"]]),
    c("integer", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(excess[["rows_to_distribute"]]),
    c(3L, 3L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(excess[["rows_to_distribute"]])),
    character(0),
    fixed = TRUE)
  ## Finished testing 'excess[["rows_to_distribute"]]'                      ####


  #### Num groups add to up the number of IDs ####

  # The last group is 1SD+ smaller than the second next
  # BUT we cannot redistribute any elements as
  # num IDs == num_final_groups

  df_medium_22_edited <- df_medium_22 %>%
    dplyr::mutate(score = ifelse(.data$rearrange_var == 8, 1, .data$score))

  excess <- get_excessive_rows_(
    data = df_medium_22_edited,
    num_col = "score",
    num_final_groups = 8,
    final_rearrange_var = "rearrange_var",
    nrows_divisible = FALSE,
    nrows_nondivisible_id = 3,
    optimize_for = "mean"
  )

  expect_equal(
    names(excess),
    "has_excessive_rearrange_ids",
    fixed = TRUE
  )
  expect_true(!excess[["has_excessive_rearrange_ids"]])



  ## nrows are divisible by 3

  df_medium_18 <- head(df_medium, 18)

  redist_info <- get_redistribution_info_(
    data = df_medium_18,
    num_col = "score",
    num_final_groups = 4,
    final_rearrange_var = "rearrange_var",
    optimize_for = "mean"
  )

  expect_false(redist_info$redistribute_smallest_group)
  expect_false(redist_info$redistribute_largest_group)
  expect_equal(redist_info$num_excessive_rearrange_ids, 2)

  xpectr::set_test_seed(1)
  excess <- get_excessive_rows_(
    data = df_medium_18,
    num_col = "score",
    num_final_groups = 4,
    final_rearrange_var = "rearrange_var",
    nrows_divisible = TRUE,
    nrows_nondivisible_id = 3,
    optimize_for = "mean"
  )

  expect_true(excess$has_excessive_rearrange_ids)
  expect_true(all(c(2, 6) %ni% excess$data$rearrange_var))


  ## Testing 'excess$rows_to_distribute'                                    ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(excess$rows_to_distribute),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    excess$rows_to_distribute[["participant"]],
    structure(c(16L, 4L, 5L, 6L, 17L, 18L), .Label = c("1", "2", "3",
      "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14",
      "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
      "25", "26", "27", "28", "29"), class = "factor"))
  expect_equal(
    excess$rows_to_distribute[["score"]],
    c(23, 69, 87, 92, 98, 101),
    tolerance = 1e-4)
  expect_equal(
    excess$rows_to_distribute[["rearrange_var"]],
    structure(c(6L, 2L, 2L, 2L, 6L, 6L), .Label = c("1", "2", "3", "4",
      "5", "6", "7", "8", "9", "10"), class = "factor"))
  # Testing column names
  expect_equal(
    names(excess$rows_to_distribute),
    c("participant", "score", "rearrange_var"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(excess$rows_to_distribute),
    c("factor", "numeric", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(excess$rows_to_distribute),
    c("integer", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(excess$rows_to_distribute),
    c(6L, 3L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(excess$rows_to_distribute)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'excess$rows_to_distribute'                           ####


  expect_equal(
    names(excess),
    "has_excessive_rearrange_ids",
    fixed = TRUE
  )

})





