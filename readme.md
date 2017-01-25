
<!-- README.md is generated from README.Rmd. Please edit that file -->
groupdata2
==========

R package: Subsetting Methods for Balanced Cross-Validation, Time Series Windowing, and General Grouping and Splitting of Data.

By Ludvig R. Olsen
Cognitive Science, Aarhus University
Started in Oct. 2016

Contact at: <r-pkgs@ludvigolsen.dk>

Main functions:
\* group\_factor
\* group
\* splt
\* fold

Other tools:
\* %staircase%

Installation
------------

Development version:
install.packages("devtools")
devtools::install\_github("LudvigOlsen/groupdata2")

To do
-----

-   Refine vignettes
-   fold() - implement force\_equal (n.b. should be special for greedy and staircasing)
-   datatables
-   Find suitable license
-   Change version number
-   Send to CRAN

Features
--------

Write up a motivation for why you would want to use groupdata2.

force\_equal
randomize
etc.

Examples
--------

``` r
# Attach packages
library(groupdata2)
library(dplyr)
library(knitr)
```

### group()

``` r
# Create dataframe
df <- data.frame("x"=c(1:12),
  "species" = rep(c('cat','pig', 'human'), 4),
  "age" = sample(c(1:100), 12))

# Using group()
group(df, 5, method = 'n_dist') %>%
  kable()
```

|    x| species |  age| .groups |
|----:|:--------|----:|:--------|
|    1| cat     |   75| 1       |
|    2| pig     |   56| 1       |
|    3| human   |   90| 2       |
|    4| cat     |   37| 2       |
|    5| pig     |   49| 3       |
|    6| human   |   34| 3       |
|    7| cat     |   70| 3       |
|    8| pig     |   20| 4       |
|    9| human   |   10| 4       |
|   10| cat     |   35| 5       |
|   11| pig     |   47| 5       |
|   12| human   |    9| 5       |

``` r

# Using group() with dplyr pipeline to get mean age
df %>%
  group(5, method = 'n_dist') %>%
  dplyr::summarise(mean_age = mean(age)) %>%
  kable()
```

| .groups |  mean\_age|
|:--------|----------:|
| 1       |   65.50000|
| 2       |   63.50000|
| 3       |   51.00000|
| 4       |   15.00000|
| 5       |   30.33333|

### fold()

``` r
# Create dataframe
df <- data.frame(
  "participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
  "age" = rep(sample(c(1:100), 6), 3),
  "diagnosis" = rep(c('a', 'b', 'a', 'a', 'b', 'b'), 3),
  "score" = sample(c(1:100), 3*6))
df <- df[order(df$participant),]
df$session <- rep(c('1','2', '3'), 6)

# Using fold()

# With cat_col and id_col
df_folded <- fold(df, 3, cat_col = 'diagnosis',
                  id_col = 'participant', method = 'n_dist')

# Show df_folded ordered by folds
df_folded[order(df_folded$.folds),] %>%
  kable()
```

| participant |  age| diagnosis |  score| session | .folds |
|:------------|----:|:----------|------:|:--------|:-------|
| 4           |   41| a         |     26| 1       | 1      |
| 4           |   41| a         |     49| 2       | 1      |
| 4           |   41| a         |     71| 3       | 1      |
| 2           |   34| b         |     29| 1       | 1      |
| 2           |   34| b         |     54| 2       | 1      |
| 2           |   34| b         |     47| 3       | 1      |
| 1           |   12| a         |     76| 1       | 2      |
| 1           |   12| a         |     95| 2       | 2      |
| 1           |   12| a         |     46| 3       | 2      |
| 6           |   64| b         |     82| 1       | 2      |
| 6           |   64| b         |     10| 2       | 2      |
| 6           |   64| b         |     98| 3       | 2      |
| 3           |   30| a         |     74| 1       | 3      |
| 3           |   30| a         |     13| 2       | 3      |
| 3           |   30| a         |      6| 3       | 3      |
| 5           |   26| b         |     30| 1       | 3      |
| 5           |   26| b         |      8| 2       | 3      |
| 5           |   26| b         |     61| 3       | 3      |

``` r

# Show distribution of diagnoses and participants
df_folded %>% 
  group_by(.folds) %>% 
  count(diagnosis, participant) %>% 
  kable()
```

| .folds | diagnosis | participant |    n|
|:-------|:----------|:------------|----:|
| 1      | a         | 4           |    3|
| 1      | b         | 2           |    3|
| 2      | a         | 1           |    3|
| 2      | b         | 6           |    3|
| 3      | a         | 3           |    3|
| 3      | b         | 5           |    3|
