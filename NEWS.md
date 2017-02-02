# groupdata2 0.1.0.9000

* New main function: partition() - used for creating balanced partitions by partition sizes

* New method category: l_ methods - n is passed as a list  

* New method: 'l_sizes' - Uses list of group sizes to create grouping factor. Can be used for fast partitioning (e.g. n = c(0.2, 0.3) returns 3 groups with 0.2 (20\%), 0.3 (30\%) and the exceeding 0.5 (50\%) of the data points)  

* New method: 'l_starts' - Uses list of start positions to create groups. Define which values from a vector to start a new group at. Skip to later appearances of a value.  

* New method: 'primes' - similar to 'staircase' but with primes as steps (e.g. group sizes 2,3,5,7..)  

* New remainder tool: '%primes%' - similar to %staircase% but for the new primes method  



# groupdata2 0.1.0

* Submitted package to CRAN  

* Main functions and tools of this version is group_factor(), group(), splt(), fold(), and %staircase%  


# groupdata2 0.0.0.9000

* Created package :)  
