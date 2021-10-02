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
