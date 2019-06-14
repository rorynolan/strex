#' @import stringr
#' @importFrom magrittr '%>%' '%<>%' '%T>%'
#' @importFrom processx curl_fds
#' @importFrom stats as.dendrogram
NULL

## usethis namespace: start
#' @useDynLib strex, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom tibble tibble
## usethis namespace: end
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}

.onUnload <- function(libpath) library.dynam.unload("strex", libpath)


#' `strex`: extra string manipulation functions
#'
#' There are some things that I wish were easier with the `stringr` or `stringi`
#' packages. The foremost of these is the extraction of numbers from strings.
#' `stringr` makes you figure out the regex for yourself; `strex` takes care of
#' this for you. There are many more useful functionalities in `strex`. In
#' particular, there's a `match_arg()` function which is more flexible than the
#' base `match.arg()`. Contributions to this package are encouraged: it is
#' intended as a miscellany of string manipulation functions which cannot be
#' found in `stringi` or `stringr`.
#'
#' @docType package
#' @name strex
#' @aliases strex-package
#' @references Rory Nolan and Sergi Padilla-Parra (2017). filesstrings: An R
#'   package for file and string manipulation. The Journal of Open Source
#'   Software, 2(14).  \doi{10.21105/joss.00260}.
NULL
