# groupdata2 1.0.0.9000

* New main function: balance() used for up- and downsampling of data to balance sample size within categories and IDs. 
Thanks for the request from jjesusfilho #3.

* New wrapper function: upsample() wraps balance() with size="max".  

* New wrapper function: downsample() wraps balance() with size="min".    

* Adds parameter "num_col" to fold() and partition() for balancing on a numerical column.  

* Adds parameter "id_aggregation_fn" to fold() and partition(). Used when balancing on both id_col and num_col.  

* Adds helper tool 'differs_from_previous'. Finds values in a vector that differs from the previous value by some threshold. Similar to find_starts().

* Adds parameter "num_fold_cols" to fold(). Useful for creating multiple fold columns for repeated cross-validation.

* Adds parameter "unique_fold_cols_only" to fold(). Whether to only include unique fold columns or not.   

* Adds parameter "max_iters" to fold(). How many times to attempt creating unique fold columns. Note that it is possible to get fewer fold columns than specified in "num_fold_cols".

* Adds parameter "parallel" to fold(). When creating multiple *unique* fold columns, we can run the column comparisons in parallel. Requires registered parallel backend.

* Adds parameter "handle_existing_fold_cols" to fold(). When calling fold() on a data frame that already contains columns with names starting with ".folds", we can either keep them and add more, or replace them.

* Fixed behavior in fold() when k is a percentage (between 0-1). It is now interpreted as the approximate size of each fold and used to calculate the number of folds. E.g. k=0.2 will lead to 5 folds.  


# groupdata2 1.0.0

* New main function: partition() - used for creating balanced partitions by partition sizes.  

* New method category: l_ methods - n is passed as a list.  

* New method: 'l_sizes' - Uses list of group sizes to create grouping factor. Can be used for partitioning (e.g. n = c(0.2, 0.3) returns 3 groups with 0.2 (20\%), 0.3 (30\%) and the exceeding 0.5 (50\%) of the data points).  

* New method: 'l_starts' - Uses list of start positions to create groups. Define which values from a vector to start a new group at. Skip to later appearances of a value. Use n = 'auto' to automatically find starts using find_starts().  

* New helper tool: 'find_starts' - Finds start positions in a vector. I.e. values that differ from the previous value. Get the values or indices of the values. Output can be used as n in 'l_starts' method.  

* New helper tool: 'find_missing_starts' - Returns the start posititions that would be recursively removed when using the 'l_starts' with remove_missing_starts set to TRUE.

* Added argument 'remove_missing_starts' to grouping functions. Recursively remove the starting positions not found with 'l_starts' method.

* New method: 'primes' - similar to 'staircase' but with primes as steps (e.g. group sizes 2,3,5,7..).  

* New remainder tool: '%primes%' - similar to %staircase% but for the new primes method.  


# groupdata2 0.1.0

* Submitted package to CRAN.  

* Main functions and tools of this version is group_factor(), group(), splt(), fold(), and %staircase%.  


# groupdata2 0.0.0.9000

* Created package :)  
