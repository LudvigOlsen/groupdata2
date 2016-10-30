# R-splitters

A set of functions for splitting dataframes and vectors in R.  

By Ludvig R. Olsen
Cognitive Science, Aarhus University  
Started in Oct. 2016  

Main functions:  
* gsplit_grouping_factor  
* gdsplit  
* gvsplit  
* nsplit_grouping_factor  
* ndsplit  
* nvsplit  
  
gsplit functions are greedy splitters. You give it some data (vector (gvsplit) or dataframe (gdsplit)) and a window size. They return a list of windows (vectors or dataframes).  

nsplit functions try to split as the data (vector (nvsplit) or dataframe (ndsplit)) as equally as possible based on a given number of windows. They return a list of windows (vectors or dataframe).  

grouping_factor functions create a factor with 1s for window 1, 2s for window 2, etc. This can be used to subset, aggregate, group_by, etc.  
  
Please read using_splitters.pdf for further information on how to use the functions.  

