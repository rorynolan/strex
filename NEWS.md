# `strex` 1.6.0

## MINOR IMPROVEMENTS
* Remove usage of `magrittr`'s `%<>%` operator.
* Now using the new `rlang::abort()` error message formatting.


# `strex` 1.5.1

## BUG FIXES
* `purrr` should not have been in `Imports`. It is now in `Suggests`.
* Some lines in `stringi-imports.h` needed to be uncommented.


# `strex` 1.5.0

## NEW FEATURES
* `str_detect_all()` and `str_detect_any()`.


# `strex` 1.4.4

## BUG FIXES
* Fix for dev version of `stringr`.


# `strex` 1.4.3

## BUG FIXES
* Insist on latest, bug-fixed `stringi` v1.7.8.


# `strex` 1.4.2

## BUG FIXES
* Remove `LazyData` from `DESCRIPTION` (was causing CRAN note).


# `strex` 1.4.1

## BUG FIXES
* R version 3.5 or greater is needed because `INTEGER_GET_REGION` is used, which was introduced with R's `ALTREP` stuff.


# `strex` 1.4.0

## MINOR IMPROVEMENTS
* `str_trim_anything()` got a speedup.
* `str_match_arg()` now has better error messaging.

## BUG FIXES
* `utils` was wrongly imported.


# `strex` 1.3.1

## BUG FIXES
* Fix a garbage collection issue.


# `strex` 1.3.0

## MINOR IMPROVEMENTS
* Move from `Rcpp` to `C`.


# `strex` 1.2.0

## MINOR IMPROVEMENTS
* Include more examples to cover all common functionality.


# `strex` 1.1.1

## BUG FIXES
* Insist on necessary versions of `stringr` and `processx`.
* Fix a C++ sanitizer warning.


# `strex` 1.1.0

## MINOR IMPROVEMENTS
* The `pattern` argument of `str_trim_anything()` now supports regular expression.

## BUG FIXES
* The new `configure` step opens up the package to people with GCC < 4.9; previously installation failed for them.


# `strex` 1.0.3

## BUG FIXES  
* Make internal function `all_equal()` more consistent.


# `strex` 1.0.2

## BUG FIXES  
* C++ sanitizer issues.


# `strex` 1.0.1

## BUG FIXES  
* There was a pervasive bug related to handling of zero-length inputs.


# `strex` 1.0.0

## BREAKING CHANGES
* `str_split_by_nums()` has been renamed to `str_split_by_numbers()` for consistency with `str_extract_numbers()`.
* `str_get_currencies()` has been renamed to `str_extract_currencies()` and been greatly improved.
* `str_get_currency()` has been replaced by `str_nth_currency()` with siblings `str_first_currency()` and `str_last_currency()`.
* `str_match_arg()` has been updated to behave more like `base::match_arg()`.

## NEW FEATURES
* `str_elems()` has been added. It is a vectorized version of `str_elem()`.
* The number (and non-numeric) extraction functions can now deal with scientific notation and with comma-separated numbers. This makes `str_first_number()` more versatile than `as.numeric()` since `as.numeric("1,000")` returns `NA` but `str_first_number("1,000")` returns the number 1000.


# `strex` 0.1.3

## BUG FIXES
* Require necessary version of `glue`.


# `strex` 0.1.2

## BUG FIXES
* Require necessary version of `ore`.


# `strex` 0.1.1

## BUG FIXES
* Allow for unexpected error message on mac.


# `strex` 0.1.0

* The first incarnation.
