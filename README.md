# R-splitters

A set of functions for splitting dataframes and vectors in R.  

By Ludvig R. Olsen
Cognitive Science, Aarhus University  
Started in Oct. 2016  

Main functions:  
* gsplit_grouping_factor  
* gsplit  
* nsplit_grouping_factor  
* nsplit  

  
gsplit is a greedy splitter. You give it some data (vector or dataframe) and a window size. It returns a list of windows (vectors or dataframes).  

nsplit tries to split the data (vector or dataframe) as equally as possible into on a given number of windows. It returns a list of windows (vectors or dataframe).  

grouping_factor functions create a factor with 1s for window 1, 2s for window 2, etc. This can be used to subset, aggregate, group_by, etc.  
  
Please read using_splitters.pdf for further information on how to use the functions.  

