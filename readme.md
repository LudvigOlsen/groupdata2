# groupdata2

R package: Subsetting Methods for Balanced Cross-Validation, 
Timeseries Windowing, and General Grouping and Splitting of Data.  

By Ludvig R. Olsen  
Cognitive Science, Aarhus University  
Started in Oct. 2016  

Main functions:  
* group_factor  
* group 
* splt  
* fold  
  
Other tools:  
* %staircase%  

## Installation  
install.packages("devtools")  
devtools::install_github("LudvigOlsen/R-splitters")  

## To do  
* Refine vignette  
* Set up testing  
* Change/add aliases for functions  
* fold() - implement force_equal (n.b. should be special for greedy and staircasing)  
