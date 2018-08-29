#' Text before or after \eqn{n}th occurrence of pattern.
#'
#' Extract the part of a string which is before or after the *n*th occurrence of
#' a specified pattern, vectorized over the string. `n` can be negatively
#' indexed. See 'Arguments'.
#'
#' \itemize{ \item `str_after_first(...)` is just `str_after_nth(..., n = 1)`.
#' \item `str_after_last(...)` is just `str_after_nth(..., n = -1)`. \item
#' `str_before_first(...)` is just `str_before_nth(..., n = 1)`. \item
#' `str_before_last(...)` is just `str_before_nth(..., n = -1)`. }
#'
#' @param strings A character vector.
#' @inheritParams str_singleize
#' @param n A natural number to identify the \eqn{n}th occurrence (defaults to
#'   first (`n = 1`)). This can be negatively indexed, so if you wish to select
#'   the *last* occurrence, you need `n = -1`, for the second-last, you
#'   need `n = -2` and so on.
#' @return A character vector of the desired strings.
#' @examples
#' string <- "ab..cd..de..fg..h"
#' str_after_nth(string, "\\.\\.", 3)
#' str_before_nth(string, "e", 1)
#' str_before_nth(string, "\\.", -3)
#' str_before_nth(string, ".", -3)
#' str_before_nth(rep(string, 2), fixed("."), -3)
#' @export
str_after_nth <- function(strings, pattern, n) {
  nth_instance_indices <- str_locate_nth(strings, pattern, n)
  str_sub(strings, nth_instance_indices[, "end"] + 1)
}

#' @rdname str_after_nth
#' @export
str_after_first <- function(strings, pattern) {
  str_after_nth(strings = strings, pattern = pattern, n = 1)
}

#' @rdname str_after_nth
#' @export
str_after_last <- function(strings, pattern) {
  str_after_nth(strings = strings, pattern = pattern, n = -1)
}
