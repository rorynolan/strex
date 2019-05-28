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



