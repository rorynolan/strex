
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

The github repo of `strex` is at <https://github.com/rorynolan/strex>.

## Installation

You can install the released version of strex from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("strex")
```

You can install the released version of strex from
[CRAN](https://CRAN.R-project.org) with:

``` r
devtools::install_github("rorynolan/strex")
```

## How to use the package

The following articles contain all you need to get going:

  - [Alphordering
    Numbers](https://rorynolan.github.io/strex/articles/alphordering-numbers.html)
    tells you how to fix the pesky problem of numbers in file names not
    complying with alphabetical order.
  - [Argument
    Matching](https://rorynolan.github.io/strex/articles/argument-matching.html)
    showcases `strex::match_arg()`, an improvement on
    `base::match.arg()` which allows one to ignore case during argument
    matching.
  - [Before and
    After](https://rorynolan.github.io/strex/articles/before-and-after.html)
    is for the common problem where you want to get the bit of a string
    before or after an occurrence of a pattern.
  - [Numbers Within
    Strings](https://rorynolan.github.io/strex/articles/numbers-in-strings.html)
    shows how to deal with the common problem of extracting numeric
    information contained within larger strings.
  - [Important
    Miscellany](https://rorynolan.github.io/strex/articles/important-miscellany.html)
    is the rest, and there’s a lot.
