
<!-- README.md is generated from README.Rmd. Please edit that file -->

# groupdata2 <a href='https://github.com/LudvigOlsen/groupdata2'><img src='man/figures/groupdata2_logo_242x280_250dpi.png' align="right" height="140" /></a>

**Author:** [Ludvig R. Olsen](http://ludvigolsen.dk/) (
<r-pkgs@ludvigolsen.dk> ) <br/> **License:**
[MIT](https://opensource.org/licenses/MIT) <br/> **Started:** October
2016

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/groupdata2)](https://cran.r-project.org/package=groupdata2)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/groupdata2)](https://cran.r-project.org/package=groupdata2)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5-6666ff.svg)](https://cran.r-project.org/)
[![Codecov test
coverage](https://codecov.io/gh/ludvigolsen/groupdata2/branch/master/graph/badge.svg)](https://codecov.io/gh/ludvigolsen/groupdata2?branch=master)
[![Travis build
status](https://travis-ci.com/LudvigOlsen/groupdata2.svg?branch=master)](https://travis-ci.com/LudvigOlsen/groupdata2)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/LudvigOlsen/groupdata2?branch=master&svg=true)](https://ci.appveyor.com/project/LudvigOlsen/groupdata2)
[![DOI](https://zenodo.org/badge/72371128.svg)](https://zenodo.org/badge/latestdoi/72371128)

## Overview

R package for dividing data into groups.

  - Create **balanced partitions** and cross-validation **folds**.
  - Perform time series **windowing** and general **grouping** and
    **splitting** of data.
  - **Balance** existing groups with **up- and downsampling**.
  - Finds values, or indices of values, that **differ** from the
    previous value by some threshold(s).
  - Check if two grouping factors have the same groups,
**memberwise**.

### Main functions

| Function         | Description                                                                                                                                                                               |
| :--------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `group_factor()` | Divides data into groups by a range of methods.                                                                                                                                           |
| `group()`        | Creates grouping factor and adds to the given data frame.                                                                                                                                 |
| `splt()`         | Creates grouping factor and splits the data by these groups.                                                                                                                              |
| `partition()`    | Splits data into partitions. Balances a given categorical variable and/or numerical variable between partitions and keeps all data points with a shared ID in the same partition.         |
| `fold()`         | Creates folds for (repeated) cross-validation. Balances a given categorical variable and/or numerical variable between folds and keeps all data points with a shared ID in the same fold. |
| `balance()`      | Uses up- and/or downsampling to equalize group sizes. Can balance on ID level. See wrappers: `downsample()`, `upsample()`.                                                                |

### Other tools

| Function                  | Description                                                                                   |
| :------------------------ | :-------------------------------------------------------------------------------------------- |
| `all_groups_identical()`  | Checks whether two grouping factors contain the same groups, *memberwise*.                    |
| `differs_from_previous()` | Finds values, or indices of values, that differ from the previous value by some threshold(s). |
| `find_starts()`           | Finds values or indices of values that are not the same as the previous value.                |
| `find_missing_starts()`   | Finds missing starts for the `l_starts` method.                                               |
| `summarize_group_cols()`  | Calculates summary statistics about group columns (i.e. `factor`s).                           |
| `%primes%`                | Finds remainder for the `primes` method.                                                      |
| `%staircase%`             | Finds remainder for the `staircase` method.                                                   |

## Table of Contents

  - [groupdata2](#groupdata2)
      - [Overview](#overview)
          - [Main functions](#main-functions)
          - [Other tools](#other-tools)
      - [Installation](#installation)
      - [Vignettes](#vignettes)
      - [Data for examples](#data-for-examples)
      - [Functions](#functions)
          - [group\_factor()](#group_factor\(\))
          - [group()](#group\(\))
          - [splt()](#splt\(\))
          - [partition()](#partition\(\))
          - [fold()](#fold\(\))
          - [balance()](#balance\(\))
      - [Grouping Methods](#grouping-methods)
          - [Specify group size](#specify-group-size)
          - [Specify number of groups](#specify-number-of-groups)
          - [Specify list](#specify-list)
          - [Specify step size](#specify-step-size)
          - [Specify start at](#specify-start-at)
      - [Balancing ID Methods](#balancing-id-methods)

## Installation

CRAN version:

> `install.packages("groupdata2")`

Development version:

> `install.packages("devtools")`  
> `devtools::install_github("LudvigOlsen/groupdata2")`

## Vignettes

`groupdata2` contains a number of vignettes with relevant use cases and
descriptions:

> `vignette(package = "groupdata2")` \# for an overview  
> `vignette("introduction_to_groupdata2")` \# begin here

## Data for examples

``` r
# Attach packages
library(groupdata2)
library(dplyr)       # %>% filter() arrange() summarize()
library(knitr)       # kable()
```

``` r
# Create small data frame
df_small <- data.frame(
  "x" = c(1:12),
  "species" = rep(c('cat', 'pig', 'human'), 4),
  "age" = sample(c(1:100), 12),
  stringsAsFactors = FALSE
)
```

``` r
# Create medium data frame
df_medium <- data.frame(
  "participant" = factor(rep(c('1', '2', '3', '4', '5', '6'), 3)),
  "age" = rep(c(20, 33, 27, 21, 32, 25), 3),
  "diagnosis" = factor(rep(c('a', 'b', 'a', 'b', 'b', 'a'), 3)),
  "score" = c(10, 24, 15, 35, 24, 14, 24, 40, 30, 
              50, 54, 25, 45, 67, 40, 78, 62, 30))
df_medium <- df_medium %>% arrange(participant)
df_medium$session <- rep(c('1','2', '3'), 6)
```

## Functions

### group\_factor()

Returns a factor with group numbers, e.g.
`factor(c(1,1,1,2,2,2,3,3,3))`.

This can be used to subset, aggregate, group\_by, etc.

Create equally sized groups by setting `force_equal = TRUE`

Randomize grouping factor by setting `randomize = TRUE`

``` r
# Create grouping factor
group_factor(
  data = df_small, 
  n = 5, 
  method = "n_dist"
)
#>  [1] 1 1 2 2 3 3 3 4 4 5 5 5
#> Levels: 1 2 3 4 5
```

### group()

Creates a grouping factor and adds it to the given data frame. The data
frame is grouped by the grouping factor for easy use in `magrittr`
(`%>%`) pipelines.

``` r
# Use group()
group(data = df_small, n = 5, method = 'n_dist') %>%
  kable()
```

|  x | species | age | .groups |
| -: | :------ | --: | :------ |
|  1 | cat     |  68 | 1       |
|  2 | pig     |  39 | 1       |
|  3 | human   |   1 | 2       |
|  4 | cat     |  34 | 2       |
|  5 | pig     |  87 | 3       |
|  6 | human   |  43 | 3       |
|  7 | cat     |  14 | 3       |
|  8 | pig     |  82 | 4       |
|  9 | human   |  59 | 4       |
| 10 | cat     |  51 | 5       |
| 11 | pig     |  85 | 5       |
| 12 | human   |  21 | 5       |

``` r
# Use group() in a pipeline 
# Get average age per group
df_small %>%
  group(n = 5, method = 'n_dist') %>% 
  dplyr::summarise(mean_age = mean(age)) %>%
  kable()
```

| .groups | mean\_age |
| :------ | --------: |
| 1       |  53.50000 |
| 2       |  17.50000 |
| 3       |  48.00000 |
| 4       |  70.50000 |
| 5       |  52.33333 |

``` r
# Using group() with 'l_starts' method
# Starts group at the first 'cat', 
# then skips to the second appearance of "pig" after "cat",
# then starts at the following "cat".
df_small %>%
  group(n = list("cat", c("pig", 2), "cat"),
        method = 'l_starts',
        starts_col = "species") %>%
  kable()
```

|  x | species | age | .groups |
| -: | :------ | --: | :------ |
|  1 | cat     |  68 | 1       |
|  2 | pig     |  39 | 1       |
|  3 | human   |   1 | 1       |
|  4 | cat     |  34 | 1       |
|  5 | pig     |  87 | 2       |
|  6 | human   |  43 | 2       |
|  7 | cat     |  14 | 3       |
|  8 | pig     |  82 | 3       |
|  9 | human   |  59 | 3       |
| 10 | cat     |  51 | 3       |
| 11 | pig     |  85 | 3       |
| 12 | human   |  21 | 3       |

### splt()

Creates the specified groups with `group_factor()` and splits the given
data by the grouping factor with `base::split`. Returns the splits in a
list.

``` r
splt(data = df_small,
     n = 3,
     method = 'n_dist') %>%
  kable()
```

<table class="kable_wrapper">

<tbody>

<tr>

<td>

| x | species | age |
| -: | :------ | --: |
| 1 | cat     |  68 |
| 2 | pig     |  39 |
| 3 | human   |   1 |
| 4 | cat     |  34 |

</td>

<td>

|   | x | species | age |
| :- | -: | :------ | --: |
| 5 | 5 | pig     |  87 |
| 6 | 6 | human   |  43 |
| 7 | 7 | cat     |  14 |
| 8 | 8 | pig     |  82 |

</td>

<td>

|    |  x | species | age |
| :- | -: | :------ | --: |
| 9  |  9 | human   |  59 |
| 10 | 10 | cat     |  51 |
| 11 | 11 | pig     |  85 |
| 12 | 12 | human   |  21 |

</td>

</tr>

</tbody>

</table>

### partition()

Creates (optionally) balanced partitions (e.g. training/test sets).
Balance partitions on categorical variable(s) and/or a numerical
variable. Make sure that all datapoints sharing an ID is in the same
partition.

``` r
# First set seed to ensure reproducibility
set.seed(1)

# Use partition() with categorical and numerical balancing,
# while ensuring all rows per ID are in the same partition
df_partitioned <- partition(
  data = df_medium, 
  p = 0.7,
  cat_col = 'diagnosis',
  num_col = "age",
  id_col = 'participant'
)

df_partitioned %>% 
  kable()
```

<table class="kable_wrapper">

<tbody>

<tr>

<td>

| participant | age | diagnosis | score | session |
| :---------- | --: | :-------- | ----: | :------ |
| 1           |  20 | a         |    10 | 1       |
| 1           |  20 | a         |    24 | 2       |
| 1           |  20 | a         |    45 | 3       |
| 4           |  21 | b         |    35 | 1       |
| 4           |  21 | b         |    50 | 2       |
| 4           |  21 | b         |    78 | 3       |
| 5           |  32 | b         |    24 | 1       |
| 5           |  32 | b         |    54 | 2       |
| 5           |  32 | b         |    62 | 3       |
| 6           |  25 | a         |    14 | 1       |
| 6           |  25 | a         |    25 | 2       |
| 6           |  25 | a         |    30 | 3       |

</td>

<td>

| participant | age | diagnosis | score | session |
| :---------- | --: | :-------- | ----: | :------ |
| 2           |  33 | b         |    24 | 1       |
| 2           |  33 | b         |    40 | 2       |
| 2           |  33 | b         |    67 | 3       |
| 3           |  27 | a         |    15 | 1       |
| 3           |  27 | a         |    30 | 2       |
| 3           |  27 | a         |    40 | 3       |

</td>

</tr>

</tbody>

</table>

### fold()

Creates (optionally) balanced folds for use in cross-validation. Balance
folds on categorical variable(s) and/or a numerical variable. Ensure
that all datapoints sharing an ID is in the same fold. Create multiple
unique fold columns at once, e.g. for repeated cross-validation.

``` r
# First set seed to ensure reproducibility
set.seed(1)

# Use fold() with categorical and numerical balancing,
# while ensuring all rows per ID are in the same fold
df_folded <- fold(
  data = df_medium, 
  k = 3,
  cat_col = 'diagnosis',
  num_col = "age",
  id_col = 'participant'
)

# Show df_folded ordered by folds
df_folded %>% 
  arrange(.folds) %>%
  kable()
```

| participant | age | diagnosis | score | session | .folds |
| :---------- | --: | :-------- | ----: | :------ | :----- |
| 1           |  20 | a         |    10 | 1       | 1      |
| 1           |  20 | a         |    24 | 2       | 1      |
| 1           |  20 | a         |    45 | 3       | 1      |
| 2           |  33 | b         |    24 | 1       | 1      |
| 2           |  33 | b         |    40 | 2       | 1      |
| 2           |  33 | b         |    67 | 3       | 1      |
| 5           |  32 | b         |    24 | 1       | 2      |
| 5           |  32 | b         |    54 | 2       | 2      |
| 5           |  32 | b         |    62 | 3       | 2      |
| 6           |  25 | a         |    14 | 1       | 2      |
| 6           |  25 | a         |    25 | 2       | 2      |
| 6           |  25 | a         |    30 | 3       | 2      |
| 3           |  27 | a         |    15 | 1       | 3      |
| 3           |  27 | a         |    30 | 2       | 3      |
| 3           |  27 | a         |    40 | 3       | 3      |
| 4           |  21 | b         |    35 | 1       | 3      |
| 4           |  21 | b         |    50 | 2       | 3      |
| 4           |  21 | b         |    78 | 3       | 3      |

``` r
# Show distribution of diagnoses and participants
df_folded %>% 
  group_by(.folds) %>% 
  count(diagnosis, participant) %>% 
  kable()
```

| .folds | diagnosis | participant | n |
| :----- | :-------- | :---------- | -: |
| 1      | a         | 1           | 3 |
| 1      | b         | 2           | 3 |
| 2      | a         | 6           | 3 |
| 2      | b         | 5           | 3 |
| 3      | a         | 3           | 3 |
| 3      | b         | 4           | 3 |

``` r
# Show age representation in folds
# Notice that we would get a more even distribution if we had more data.
# As age is fixed per ID, we only have 3 ages per category to balance with.
df_folded %>% 
  group_by(.folds) %>% 
  summarize(mean_age = mean(age),
            sd_age = sd(age)) %>% 
  kable()
```

| .folds | mean\_age |  sd\_age |
| :----- | --------: | -------: |
| 1      |      26.5 | 7.120393 |
| 2      |      28.5 | 3.834058 |
| 3      |      24.0 | 3.286335 |

**Notice**, that the we now have the opportunity to include the
*session* variable and/or use *participant* as a random effect in our
model when doing cross-validation, as any participant will only appear
in one fold.

We also have a balance in the representation of each diagnosis, which
could give us better, more consistent results.

### balance()

Uses up- and/or downsampling to fix the group sizes to the min, max,
mean, or median group size or to a specific number of rows. Balancing
can also happen on the ID level, e.g. to ensure the same number of IDs
in each category.

``` r
# Lets first unbalance the dataset by removing some rows
df_b <- df_medium %>% 
  arrange(diagnosis) %>% 
  filter(!row_number() %in% c(5,7,8,13,14,16,17,18))

# Show distribution of diagnoses and participants
df_b %>% 
  count(diagnosis, participant) %>% 
  kable()
```

| diagnosis | participant | n |
| :-------- | :---------- | -: |
| a         | 1           | 3 |
| a         | 3           | 2 |
| a         | 6           | 1 |
| b         | 2           | 3 |
| b         | 4           | 1 |

``` r
# First set seed to ensure reproducibility
set.seed(1)

# Downsampling by diagnosis
balance(
  data = df_b, 
  size = "min", 
  cat_col = "diagnosis"
) %>% 
  count(diagnosis, participant) %>% 
  kable()
```

| diagnosis | participant | n |
| :-------- | :---------- | -: |
| a         | 1           | 2 |
| a         | 3           | 1 |
| a         | 6           | 1 |
| b         | 2           | 3 |
| b         | 4           | 1 |

``` r
# Downsampling the IDs
balance(
  data = df_b, 
  size = "min", 
  cat_col = "diagnosis", 
  id_col = "participant", 
  id_method = "n_ids"
) %>% 
  count(diagnosis, participant) %>% 
  kable()
```

| diagnosis | participant | n |
| :-------- | :---------- | -: |
| a         | 1           | 3 |
| a         | 3           | 2 |
| b         | 2           | 3 |
| b         | 4           | 1 |

## Grouping Methods

There are currently 9 methods available. They can be divided into 5
categories.

*Examples of group sizes are based on a vector with 57 elements.*

### Specify group size

##### Method: greedy

Divides up the data greedily given a specified group size.

E.g. group sizes: 10, 10, 10, 10, 10, 7

### Specify number of groups

##### Method: n\_dist (Default)

Divides the data into a specified number of groups and distributes
excess data points across groups.

E.g. group sizes: 11, 11, 12, 11, 12

##### Method: n\_fill

Divides the data into a specified number of groups and fills up groups
with excess data points from the beginning.

E.g. group sizes: 12, 12, 11, 11, 11

##### Method: n\_last

Divides the data into a specified number of groups. The algorithm finds
the most equal group sizes possible, using all data points. Only the
last group is able to differ in size.

E.g. group sizes: 11, 11, 11, 11, 13

##### Method: n\_rand

Divides the data into a specified number of groups. Excess data points
are placed randomly in groups (only 1 per group).

E.g. group sizes: 12, 11, 11, 11, 12

### Specify list

##### Method: l\_sizes

Uses a list / vector of group sizes to divide up the data.  
Excess data points are placed in an extra group.

E.g. `n = c(11, 11)` returns group sizes: 11, 11, 35

##### Method: l\_starts

Uses a list of starting positions to divide up the data.  
Starting positions are values in a vector (e.g. column in data frame).
Skip to a specific nth appearance of a value by using `c(value,
skip_to)`.

E.g. `n = c(11, 15, 27, 43)` returns group sizes: 10, 4, 12, 16, 15

Identical to `n = list(11, 15, c(27, 1), 43` where `1` specifies that we
want the first appearance of 27 after the previous value 15.

If passing `n = "auto"` starting positions are automatically found with
`find_starts()`.

### Specify step size

##### Method: staircase

Uses step\_size to divide up the data. Group size increases with 1 step
for every group, until there is no more data.

E.g. group sizes: 5, 10, 15, 20, 7

### Specify start at

##### Method: primes

Creates groups with sizes corresponding to prime numbers.  
Starts at `n` (prime number). Increases to the the next prime number
until there is no more data.

E.g. group sizes: 5, 7, 11, 13, 17, 4

## Balancing ID Methods

There are currently 4 methods for balancing on ID level in `balance()`.

##### ID method: n\_ids

Balances on ID level only. It makes sure there are the same number of
IDs in each category. This might lead to a different number of rows
between categories.

##### ID method: n\_rows\_c

Attempts to level the number of rows per category, while only
removing/adding entire IDs. This is done with repetition and by
iteratively picking the ID with the number of rows closest to the
lacking/excessive number of rows in the category.

##### ID method: distributed

Distributes the lacking/excess rows equally between the IDs. If the
number to distribute can not be equally divided, some IDs will have 1
row more/less than the others.

##### ID method: nested

Balances the IDs within their categories, meaning that all IDs in a
category will have the same number of rows.
