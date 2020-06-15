## Test environments
* local OS X install, R 3.6.1  
* ubuntu 14.04.5 (on travis-ci), R 3.6.3, 4.0.0 and devel
* win-builder

## R CMD check results  
There were no ERRORs, WARNINGs or NOTEs.  

## Downstream dependencies
I used revdepcheck::revdep_check() to run R CMD check on all downstream dependencies 
and it found no problems.

## Release notes
I fixed the warning with broom::tidy on summary.lm by using tidy directly on the lm.
The note about the link is a problem with the opensource.org site, not the groupdata2.
