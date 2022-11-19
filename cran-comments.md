## Test environments
* local OS X install, R 3.6.1
* GitHub Actions on MacOS, Windows and Ubuntu-18.04, R latest, devel and oldrel
* win-builder

## R CMD check results  
There were no ERRORs, WARNINGs or NOTEs.

NOTE: When testing without suggested packages, it raises and ERROR when needing 
rmarkdown to build vignettes. I don't know how (and whether) to avoid this.

## Downstream dependencies
I used revdepcheck::revdep_check() to run R CMD check on all downstream dependencies 
and it found no problems.
