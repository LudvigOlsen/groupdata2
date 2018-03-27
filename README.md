
<!-- README.md is generated from README.Rmd. Please edit that file -->
groupdata2
==========

R package: Subsetting methods for balanced cross-validation, time series windowing, and general grouping and splitting of data.

By Ludvig R. Olsen,
Cognitive Science, Aarhus University.
Started in Oct. 2016

Contact at: <r-pkgs@ludvigolsen.dk>

Main functions:

-   group\_factor
-   group
-   splt
-   partition
-   fold
-   balance (\*new, \*github only)

Other tools:

-   find\_starts
-   %staircase%
-   %primes%

Installation
------------

CRAN version:

> install.packages("groupdata2")

Development version:

> install.packages("devtools")
> devtools::install\_github("LudvigOlsen/groupdata2")

Vignettes
---------

groupdata2 contains a number of vignettes with relevant use cases and descriptions.

> vignette(package='groupdata2') \# for an overview
> vignette("introduction\_to\_groupdata2") \# begin here

Functions
---------

### group\_factor()

Returns a factor with group numbers, e.g. (1,1,1,2,2,2,3,3,3).

This can be used to subset, aggregate, group\_by, etc.

Create equally sized groups by setting force\_equal = TRUE

Randomize grouping factor by setting randomize = TRUE

### group()

Returns the given data as a dataframe with added grouping factor made with group\_factor(). The dataframe is grouped by the grouping factor for easy use with dplyr pipelines.

### splt()

Creates the specified groups with group\_factor() and splits the given data by the grouping factor with base::split. Returns the splits in a list.

### partition()

Creates (optionally) balanced partitions (e.g. training/test sets). Balance partitions on one categorical variable and/or one numerical variable. Make sure that all datapoints sharing an ID is in the same partition.

### fold()

Creates (optionally) balanced folds for use in cross-validation. Balance folds on one categorical variable and/or one numerical variable. Make sure that all datapoints sharing an ID is in the same fold.

### balance()

Uses up- or downsampling to fix the group size to the min, max, mean, or median group size or to a specific number of rows. Balancing can also happen on the ID level, e.g. to ensure the same number of IDs in each category.

Grouping Methods
----------------

There are currently 9 methods available. They can be divided into 5 categories.

*Examples of group sizes are based on a vector with 57 elements.*

### Specify group size

##### Method: greedy

Divides up the data greedily given a specified group size.

E.g. group sizes: 10, 10, 10, 10, 10, 7

### Specify number of groups

##### Method: n\_dist (Default)

Divides the data into a specified number of groups and distributes excess data points across groups.

E.g. group sizes: 11, 11, 12, 11, 12

##### Method: n\_fill

Divides the data into a specified number of groups and fills up groups with excess data points from the beginning.

E.g. group sizes: 12, 12, 11, 11, 11

##### Method: n\_last

Divides the data into a specified number of groups. The algorithm finds the most equal group sizes possible, using all data points. Only the last group is able to differ in size.

E.g. group sizes: 11, 11, 11, 11, 13

##### Method: n\_rand

Divides the data into a specified number of groups. Excess data points are placed randomly in groups (only 1 per group).

E.g. group sizes: 12, 11, 11, 11, 12

### Specify list

##### Method: l\_sizes

Uses a list / vector of group sizes to divide up the data.
Excess data points are placed in an extra group.

E.g. *n = c(11, 11)* returns group sizes: 11, 11, 35

##### Method: l\_starts

Uses a list of starting positions to divide up the data.
Starting positions are values in a vector (e.g. column in dataframe). Skip to a specific nth appearance of a value by using c(value, skip\_to).

E.g. *n = c(11, 15, 27, 43)* returns group sizes: 10, 4, 12, 16, 15

Identical to *n = list(11, 15, c(27, 1), 43)* where 1 specifies that we want the first appearance of 27 after the previous value 15.

If passing *n = 'auto'* starting posititions are automatically found with find\_starts().

### Specify step size

##### Method: staircase

Uses step\_size to divide up the data. Group size increases with 1 step for every group, until there is no more data.

E.g. group sizes: 5, 10, 15, 20, 7

### Specify start at

##### Method: primes

Creates groups with sizes corresponding to prime numbers.
Starts at n (prime number). Increases to the the next prime number until there is no more data.

E.g. group sizes: 5, 7, 11, 13, 17, 4

Balancing ID Methods
--------------------

There are currently 4 methods for balancing on ID level.

##### ID method: n\_ids

Balances on ID level only. It makes sure there are the same number of IDs in each category. This might lead to a different number of rows between categories.

##### ID method: n\_rows\_c

Attempts to level the number of rows per category, while only removing/adding entire IDs. This is done with repetition and by iteratively picking the ID with the number of rows closest to the lacking/excessive number of rows in the category.

##### ID method: distributed

Distributes the lacking/excess rows equally between the IDs. If the number to distribute can not be equally divided, some IDs will have 1 row more/less than the others.

##### ID method: nested

Balances the IDs within their categories, meaning that all IDs in a category will have the same number of rows.

Examples
--------

``` r
# Attach packages
library(groupdata2)
library(dplyr)
library(knitr)
```

``` r
# Create dataframe
df <- data.frame("x"=c(1:12),
  "species" = rep(c('cat','pig', 'human'), 4),
  "age" = sample(c(1:100), 12))
```

### group()

``` r
# Using group()
group(df, n = 5, method = 'n_dist') %>%
  kable()
```

|    x| species |  age| .groups |
|----:|:--------|----:|:--------|
|    1| cat     |   90| 1       |
|    2| pig     |   92| 1       |
|    3| human   |   79| 2       |
|    4| cat     |   86| 2       |
|    5| pig     |   10| 3       |
|    6| human   |   26| 3       |
|    7| cat     |   60| 3       |
|    8| pig     |   70| 4       |
|    9| human   |   21| 4       |
|   10| cat     |   32| 5       |
|   11| pig     |   84| 5       |
|   12| human   |   34| 5       |

``` r

# Using group() with dplyr pipeline to get mean age
df %>%
  group(n = 5, method = 'n_dist') %>%
  dplyr::summarise(mean_age = mean(age)) %>%
  kable()
```

| .groups |  mean\_age|
|:--------|----------:|
| 1       |       91.0|
| 2       |       82.5|
| 3       |       32.0|
| 4       |       45.5|
| 5       |       50.0|

``` r

# Using group() with 'l_starts' method
# Starts group at the first 'cat', 
# then skips to the second appearance of "pig" after "cat",
# then starts at the following "cat".
df %>%
  group(n = list("cat", c("pig",2), "cat"), 
        method = 'l_starts',
        starts_col = "species") %>%
  kable()
```

|    x| species |  age| .groups |
|----:|:--------|----:|:--------|
|    1| cat     |   90| 1       |
|    2| pig     |   92| 1       |
|    3| human   |   79| 1       |
|    4| cat     |   86| 1       |
|    5| pig     |   10| 2       |
|    6| human   |   26| 2       |
|    7| cat     |   60| 3       |
|    8| pig     |   70| 3       |
|    9| human   |   21| 3       |
|   10| cat     |   32| 3       |
|   11| pig     |   84| 3       |
|   12| human   |   34| 3       |

### fold()

``` r
# Create dataframe
df <- data.frame(
  "participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
  "age" = rep(c(20,33,27,21,32,25), 3),
  "diagnosis" = rep(c('a', 'b', 'a', 'b', 'b', 'a'), 3),
  "score" = c(10,24,15,35,24,14,24,40,30,50,54,25,45,67,40,78,62,30))
df <- df %>% arrange(participant)
df$session <- rep(c('1','2', '3'), 6)
```

``` r
# Using fold() 

# First set seed to ensure reproducibility
set.seed(1)

# Use fold() with cat_col, num_col and id_col
df_folded <- fold(df, k = 3, cat_col = 'diagnosis',
                  num_col = "age", 
                  id_col = 'participant')

# Show df_folded ordered by folds
df_folded %>% 
  arrange(.folds) %>%
  kable()
```

| participant |  age| diagnosis |  score| session | .folds |
|:------------|----:|:----------|------:|:--------|:-------|
| 1           |   20| a         |     10| 1       | 1      |
| 1           |   20| a         |     24| 2       | 1      |
| 1           |   20| a         |     45| 3       | 1      |
| 4           |   21| b         |     35| 1       | 1      |
| 4           |   21| b         |     50| 2       | 1      |
| 4           |   21| b         |     78| 3       | 1      |
| 5           |   32| b         |     24| 1       | 2      |
| 5           |   32| b         |     54| 2       | 2      |
| 5           |   32| b         |     62| 3       | 2      |
| 6           |   25| a         |     14| 1       | 2      |
| 6           |   25| a         |     25| 2       | 2      |
| 6           |   25| a         |     30| 3       | 2      |
| 2           |   33| b         |     24| 1       | 3      |
| 2           |   33| b         |     40| 2       | 3      |
| 2           |   33| b         |     67| 3       | 3      |
| 3           |   27| a         |     15| 1       | 3      |
| 3           |   27| a         |     30| 2       | 3      |
| 3           |   27| a         |     40| 3       | 3      |

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
| 1      | b         | 4           |    3|
| 2      | a         | 6           |    3|
| 2      | b         | 5           |    3|
| 3      | a         | 3           |    3|
| 3      | b         | 2           |    3|

``` r

# Show age representation in folds
df_folded %>% 
  group_by(.folds) %>% 
  summarize(sum_of_age = sum(age)) %>% 
  kable()
```

| .folds |  sum\_of\_age|
|:-------|-------------:|
| 1      |           123|
| 2      |           171|
| 3      |           180|

**Notice** that the we now have the opportunity to include the *session* variable and/or use *participant* as a random effect in our model when doing cross-validation, as any participant will only appear in one fold.

We also have a balance in the representation of each diagnosis, which could give us better, more consistent results.

### balance()

``` r
# Lets first unbalance the dataset by removing some rows
df_b <- df %>% 
  arrange(diagnosis) %>% 
  filter(!row_number() %in% c(5,7,8,13,14,16,17,18))

# Show distribution of diagnoses and participants
df_b %>% 
  count(diagnosis, participant) %>% 
  kable()
```

| diagnosis | participant |    n|
|:----------|:------------|----:|
| a         | 1           |    3|
| a         | 3           |    2|
| a         | 6           |    1|
| b         | 2           |    3|
| b         | 4           |    1|

``` r

# First set seed to ensure reproducibility
set.seed(1)

# Downsampling by diagnosis
balance(df_b, size="min", cat_col = "diagnosis") %>% 
  count(diagnosis, participant) %>% 
  kable()
```

| diagnosis | participant |    n|
|:----------|:------------|----:|
| a         | 1           |    2|
| a         | 3           |    1|
| a         | 6           |    1|
| b         | 2           |    3|
| b         | 4           |    1|

``` r

# Downsampling the IDs
balance(df_b, size="min", cat_col = "diagnosis", 
        id_col = "participant", id_method = "n_ids") %>% 
  count(diagnosis, participant) %>% 
  kable()
```

| diagnosis | participant |    n|
|:----------|:------------|----:|
| a         | 1           |    3|
| a         | 3           |    2|
| b         | 2           |    3|
| b         | 4           |    1|
