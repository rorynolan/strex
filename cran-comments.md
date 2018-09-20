


## Test environments


* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.5.1
* Windows server 2012 (on AppVeyor), R 3.5.1
* win-builder (devel and release)



## R CMD check results


0 errors | 0 warnings | 0 nots



## Reverse Dependencies


There is 1 reverse dependency: `filesstrings`. This update does not break it. See https://github.com/rorynolan/strex/blob/master/revdep/checks.rds for full check results.



## Fix


The package was failing mac tests on CRAN due to unexpected differences in error messages across platforms. This is the fix for that.
