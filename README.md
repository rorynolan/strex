
<!-- README.md is generated from README.Rmd. Please edit that file -->

# strex <img src="man/figures/logo.png" align="right" height=140/>

There are some things that I wish were easier with the `stringr` or
`stringi` packages. The foremost of these is the extraction of numbers
from strings. `stringr` lets you figure this out the regex for yourself;
`strex` takes care of this for you. There are many more useful
functionalities in `strex`. In particular, there’s a `match_arg()`
function which is more flexible than the base `match.arg()`.
Contributions to this package are encouraged: it is intended as a
miscellany of string manipulation functions which cannot be found in
`stringi` or `stringr`.

[![R-CMD-check](https://github.com/rorynolan/strex/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rorynolan/strex/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/rorynolan/strex/branch/master/graph/badge.svg)](https://app.codecov.io/gh/rorynolan/strex?branch=master)

![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

[![CRAN
status](https://www.r-pkg.org/badges/version/strex)](https://cran.r-project.org/package=strex)
[![RStudio CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/strex)](https://cranlogs.r-pkg.org/badges/grand-total/strex)
[![RStudio CRAN monthly
downloads](https://cranlogs.r-pkg.org/badges/strex)](https://cranlogs.r-pkg.org/badges/strex)

## Installation

You can install the release version of `strex` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("strex")
```

You can install the development version of `strex` from
[GitHub](https://github.com/rorynolan/strex/) with:

``` r
devtools::install_github("rorynolan/strex")
```

## How to use the package

See the package website at <https://rorynolan.github.io/strex/>.

## Contribution

The preferred method of contribution is by GitHub pull request.

Please note that the `strex` project is released with a [Contributor
Code of Conduct](inst/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
