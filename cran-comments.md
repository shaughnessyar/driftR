## Release summary
This is our initial CRAN submission.

## Test environments
* local macOS install, R 3.4.2
* ubuntu 14.04 (on Travis CI), R-release, R-oldrel, R-devel
* macOS (on Travis CI), R-release, R-oldrel
* windows (on Appveyor), R-release, R-oldrel
* winbuilder, R-devel

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs with local checks or on Travis CI/Appveyor.

On devtools::release()'s R CMD check we get one NOTE:

* checking CRAN incoming feasibility ... NOTE

  This notes that the package is a new submission. 

On winbuilder, we get one NOTE:

* checking CRAN incoming feasibility ... NOTE

  This notes that the package is a new submission. It also suggests that there may 
  be some mis-spelled words in DESCRIPTION. We have checked these and they are all
  abbreviations that are spelled correctly.

## Reverse dependencies
Not applicable.
