#' Extract text before or after `n`th occurrence of pattern.
#'
#' Extract the part of a string which is before or after the `n`th occurrence of
#' a specified pattern, vectorized over the string.
#'
#' \itemize{ \item `str_after_first(...)` is just `str_after_nth(..., n = 1)`.
#' \item `str_after_last(...)` is just `str_after_nth(..., n = -1)`. \item
#' `str_before_first(...)` is just `str_before_nth(..., n = 1)`. \item
#' `str_before_last(...)` is just `str_before_nth(..., n = -1)`. }
#'
#' @param string A character vector.
#' @param pattern The pattern to look for.
#'
#'   The default interpretation is a regular expression, as described in
#'   [stringi::stringi-search-regex].
#'
#'   To match a without regular expression (i.e. as a human would), use
#'   [coll()][stringr::coll]. For details see [stringr::regex()].
#'
#' @param n A vector of integerish values. Must be either length 1 or
#'   have length equal to the length of `string`. Negative indices count from
#'   the back: while `n = 1` and `n = 2` correspond to first and second, `n =
#'   -1` and `n = -2` correspond to last and second-last. `n = 0` will return
#'   `NA`.
#'
#' @return A character vector.
#' @examples
#' string <- "abxxcdxxdexxfgxxh"
#' str_after_nth(string, "xx", 3)
#' str_before_nth(string, "e", 1:2)
#' str_before_nth(string, "xx", -3)
#' str_before_nth(string, ".", -3)
#' str_before_nth(rep(string, 2), "..x", -3)
#' string <- c("abc", "xyz.zyx")
#' str_after_first(string, ".") # using regex
#' str_after_first(string, coll(".")) # using human matching
#' @name before-and-after
#' @family bisectors
NULL

#' @rdname before-and-after
#' @export
str_after_nth <- function(string, pattern, n) {
  if (is_l0_char(string)) {
    return(character())
  }
  verify_string_pattern_n(string, pattern, n)
  nth_instance_indices <- str_locate_nth(string, pattern, n)
  str_sub(string, nth_instance_indices[, "end"] + 1)
}

#' @rdname before-and-after
#' @export
str_after_first <- function(string, pattern) {
  str_after_nth(string, pattern, n = 1)
}

#' @rdname before-and-after
#' @export
str_after_last <- function(string, pattern) {
  str_after_nth(string, pattern, n = -1)
}
