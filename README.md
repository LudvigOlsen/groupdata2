
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
|    1| cat     |   44| 1       |
|    2| pig     |    4| 1       |
|    3| human   |    3| 2       |
|    4| cat     |   81| 2       |
|    5| pig     |   32| 3       |
|    6| human   |    9| 3       |
|    7| cat     |   40| 3       |
|    8| pig     |   26| 4       |
|    9| human   |    6| 4       |
|   10| cat     |   53| 5       |
|   11| pig     |   41| 5       |
|   12| human   |   91| 5       |

``` r

# Using group() with dplyr pipeline to get mean age
df %>%
  group(5, method = 'n_dist') %>%
  dplyr::summarise(mean_age = mean(age)) %>%
  kable()
```

| .groups |  mean\_age|
|:--------|----------:|
| 1       |   24.00000|
| 2       |   42.00000|
| 3       |   27.00000|
| 4       |   16.00000|
| 5       |   61.66667|

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
| 4           |    1| a         |     40| 1       | 1      |
| 4           |    1| a         |     41| 2       | 1      |
| 4           |    1| a         |     80| 3       | 1      |
| 5           |   14| b         |     88| 1       | 1      |
| 5           |   14| b         |     65| 2       | 1      |
| 5           |   14| b         |     57| 3       | 1      |
| 3           |   48| a         |      9| 1       | 2      |
| 3           |   48| a         |     38| 2       | 2      |
| 3           |   48| a         |     87| 3       | 2      |
| 2           |   42| b         |     99| 1       | 2      |
| 2           |   42| b         |     81| 2       | 2      |
| 2           |   42| b         |     63| 3       | 2      |
| 1           |   97| a         |      3| 1       | 3      |
| 1           |   97| a         |     83| 2       | 3      |
| 1           |   97| a         |     14| 3       | 3      |
| 6           |   40| b         |      4| 1       | 3      |
| 6           |   40| b         |     96| 2       | 3      |
| 6           |   40| b         |    100| 3       | 3      |

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
| 1      | b         | 5           |    3|
| 2      | a         | 3           |    3|
| 2      | b         | 2           |    3|
| 3      | a         | 1           |    3|
| 3      | b         | 6           |    3|
