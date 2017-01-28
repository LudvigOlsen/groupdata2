## Test environments
* local OS X install, R 3.3.1  
* local OS X install, R 3.3.2  
* win-builder

## R CMD check results  
There were no ERRORs, WARNINGs or NOTEs.  

## Downstream dependencies
I used devtools::revdep_check() to run R CMD check on all downstream dependencies and it returned:  
  No ERRORs or WARNINGs found  

This is my first submission.  

## Resubmission
This is a resubmission. CRAN's comments and my actions towards it:  

"Please only use the CRAN template for this license." - fixed

"Vignette sources in 'inst/doc' missing from the 'vignettes' 
directory: 'l_methods_showcase.Rmd', 'the_fake_party.Rmd'"  
- These are work in progress and should not be built. 
I thought listing the Rmd files in .Rbuildignore would take care of that. 
Instead I've moved them to a new folder and listed that folder in .Rbuildignore.  

