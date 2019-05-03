# filesstrings

Version: 3.0.0

## Newly broken

*   checking whether package ‘filesstrings’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/rnolan/Dropbox/R/strex/revdep/checks.noindex/filesstrings/new/filesstrings.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘filesstrings’ ...
** package ‘filesstrings’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/rnolan/Dropbox/R/strex/revdep/library.noindex/filesstrings/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/rnolan/Dropbox/R/strex/revdep/library.noindex/filesstrings/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c strings.cpp -o strings.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o filesstrings.so RcppExports.o strings.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/rnolan/Dropbox/R/strex/revdep/checks.noindex/filesstrings/new/filesstrings.Rcheck/filesstrings/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error : 'str_get_currencies' is not an exported object from 'namespace:strex'
Error : unable to load R code in package ‘filesstrings’
ERROR: lazy loading failed for package ‘filesstrings’
* removing ‘/Users/rnolan/Dropbox/R/strex/revdep/checks.noindex/filesstrings/new/filesstrings.Rcheck/filesstrings’

```
### CRAN

```
* installing *source* package ‘filesstrings’ ...
** package ‘filesstrings’ successfully unpacked and MD5 sums checked
** libs
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/rnolan/Dropbox/R/strex/revdep/library.noindex/filesstrings/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/rnolan/Dropbox/R/strex/revdep/library.noindex/filesstrings/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c strings.cpp -o strings.o
clang++ -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o filesstrings.so RcppExports.o strings.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
installing to /Users/rnolan/Dropbox/R/strex/revdep/checks.noindex/filesstrings/old/filesstrings.Rcheck/filesstrings/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded
* DONE (filesstrings)

```
