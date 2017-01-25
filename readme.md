
<!-- README.md is generated from README.Rmd. Please edit that file -->
groupdata2
==========

R package: Subsetting Methods for Balanced Cross-Validation, Timeseries Windowing, and General Grouping and Splitting of Data.

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
|    1| cat     |   35| 1       |
|    2| pig     |   59| 1       |
|    3| human   |    2| 2       |
|    4| cat     |   47| 2       |
|    5| pig     |   41| 3       |
|    6| human   |   97| 3       |
|    7| cat     |   39| 3       |
|    8| pig     |   54| 4       |
|    9| human   |   78| 4       |
|   10| cat     |   30| 5       |
|   11| pig     |   38| 5       |
|   12| human   |   99| 5       |

``` r

# Using group() with dplyr pipeline to get mean age
df %>%
  group(5, method = 'n_dist') %>%
  dplyr::summarise(mean_age = mean(age)) %>%
  kable()
```

| .groups |  mean\_age|
|:--------|----------:|
| 1       |   47.00000|
| 2       |   24.50000|
| 3       |   59.00000|
| 4       |   66.00000|
| 5       |   55.66667|

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
| 1           |   15| a         |     79| 1       | 1      |
| 1           |   15| a         |     97| 2       | 1      |
| 1           |   15| a         |     94| 3       | 1      |
| 5           |    4| b         |     69| 1       | 1      |
| 5           |    4| b         |     67| 2       | 1      |
| 5           |    4| b         |     59| 3       | 1      |
| 3           |    9| a         |     31| 1       | 2      |
| 3           |    9| a         |     18| 2       | 2      |
| 3           |    9| a         |      3| 3       | 2      |
| 6           |   56| b         |     54| 1       | 2      |
| 6           |   56| b         |     26| 2       | 2      |
| 6           |   56| b         |     53| 3       | 2      |
| 4           |   48| a         |     56| 1       | 3      |
| 4           |   48| a         |     47| 2       | 3      |
| 4           |   48| a         |     22| 3       | 3      |
| 2           |   78| b         |     96| 1       | 3      |
| 2           |   78| b         |     43| 2       | 3      |
| 2           |   78| b         |     25| 3       | 3      |

``` r

# Show distribution of diagnoses and participants
df_folded %>% 
  group_by(.folds) %>% 
  count(diagnosis, participant) %>% 
  kable()
```

| .folds | diagnosis | participant |    n|
|:-------|:----------|:------------|----:|
| 1      | a         | 1           |    3|
| 1      | b         | 5           |    3|
| 2      | a         | 3           |    3|
| 2      | b         | 6           |    3|
| 3      | a         | 4           |    3|
| 3      | b         | 2           |    3|
