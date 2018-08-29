#' Locate the braces in a string.
#'
#' Give the positions of `(`, `)`, `[`, `]`, `\{`, `\}` within a string.
#'
#' @param string A character vector
#'
#' @return A list of data frames, one for each member of the string character
#'   vector. Each data frame has a "position" and "brace" column which give the
#'   positions and types of braces in the given string.
#'
#' @examples
#' str_locate_braces(c("a{](kkj)})", "ab(]c{}"))
#' @export
str_locate_braces <- function(string) {
  locations <- str_locate_all(string, "[\\(\\)\\[\\]\\{\\}]") %>%
    int_lst_first_col()
  braces <- str_elems(string, locations)
  lst_df_pos_brace(locations, braces)
}

#' Locate the indices of the \eqn{n}th instance of a pattern.
#'
#' The \eqn{n}th instance of an pattern will cover a series of character
#' indices. These functions tell you which indices those are.
#'
#' \itemize{ \item `str_locate_first(...)` is just
#' `str_locate_nth(..., n = 1)`. \item
#' `str_locate_last(...)` is just `str_locate_nth(..., n =
#' -1)`. }
#'
#' @param string A character vector. These functions are vectorized over this
#'   argument.
#' @inheritParams str_singleize
#' @param n Then \eqn{n} for the \eqn{n}th instance of the pattern.
#'
#' @return A two-column matrix. The \eqn{i}th row of this matrix gives the start
#'   and end indices of the \eqn{n}th instance of `pattern` in the \eqn{i}th
#'   element of `string`.
#'
#' @examples
#' str_locate_nth(c("abcdabcxyz", "abcabc"), "abc", 2)
#'
#' @export
str_locate_nth <- function(string, pattern, n) {
  checkmate::assert_number(n)
  checkmate::assert_number(abs(n), lower = 1)
  instances <- str_locate_all(string, pattern)
  n_instances <- lengths(instances) / 2
  bad <- n_instances < abs(n)
  if (any(bad)) {
    first_bad <- match(T, bad)
    custom_stop("
      There aren't {abs(n)} instances of your `pattern`, \"{pattern}\" in one or
      more of the strings.
      ", "
      The first such bad string is string {first_bad} \"{string[first_bad]}\"
      which has {n_instances[first_bad]} instances.
      ")
  }
  l <- length(string)
  if (n > 0) {
    n %<>% rep(l)
  } else {
    n <- n_instances + n + 1
  }
  intmat_list_bind_nth_rows(instances, n - 1) %>%
    magrittr::set_colnames(c("start", "end"))
}

#' @rdname str_locate_nth
#' @export
str_locate_first <- function(string, pattern) {
  str_locate_nth(string = string, pattern = pattern, n = 1)
}

#' @rdname str_locate_nth
#' @export
str_locate_last <- function(string, pattern) {
  str_locate_nth(string = string, pattern = pattern, n = -1)
}
