#' Locate the braces in a string.
#'
#' Give the positions of `(`, `)`, `[`, `]`, `\{`, `\}` within a string.
#'
#' @param string A character vector
#'
#' @return A [tibble][tibble::tibble-package] with 4 columns: `string_num`,
#'   `string`, `position` and `brace`. Every extracted brace amount gets its
#'   own row in the tibble detailing the string number and string that it was
#'   extracted from, the position in its string and the brace.
#'
#' @examples
#' str_locate_braces(c("a{](kkj)})", "ab(]c{}"))
#' @export
str_locate_braces <- function(string) {
  pattern <- "[\\(\\)\\[\\]\\{\\}]"
  locations <- str_locate_all(string, pattern) %>%
    int_lst_first_col()
  braces <- str_extract_all(string, pattern)
  string_num <- rep(seq_along(string), lengths(braces))
  list(
    string_num = string_num, string = string[string_num],
    position = unlist(locations), brace = unlist(braces)
  ) %>%
    tibble::new_tibble(nrow = length(string_num))
}

#' Locate the indices of the \eqn{n}th instance of a pattern.
#'
#' The \eqn{n}th instance of an pattern will cover a series of character
#' indices. These functions tell you which indices those are. These functions
#' are vectorised over all arguments.
#'
#' \itemize{ \item `str_locate_first(...)` is just `str_locate_nth(..., n = 1)`.
#' \item `str_locate_last(...)` is just `str_locate_nth(..., n = -1)`. }
#'
#' @param string A character vector. These functions are vectorized over this
#'   argument.
#' @inheritParams str_singleize
#' @param n A character vector of length 1 or the same length as `string`.
#'
#' @return A two-column matrix. The \eqn{i}th row of this matrix gives the start
#'   and end indices of the \eqn{n}th instance of `pattern` in the \eqn{i}th
#'   element of `string`.
#'
#' @examples
#' str_locate_nth(c("abcdabcxyz", "abcabc"), "abc", 2)
#' str_locate_nth(c("This old thing.", "That beautiful thing there."),
#' "\\w+", c(2, -2))
#' @export
str_locate_nth <- function(string, pattern, n) {
  checkmate::assert_character(string, min.len = 1)
  checkmate::assert_character(pattern, min.len = 1)
  checkmate::assert_integerish(n, min.len = 1)
  l <- length(string)
  if (length(pattern) > 1 && length(pattern) != l) {
    custom_stop(
      "`pattern` must either be length 1 or have the same length as `string`",
      "Your `pattern` has length {length(pattern)}.",
      "Your `string` has length {l}."
    )
  }
  if (length(n) > 1 && length(n) != l) {
    custom_stop(
      "`n` must either be length 1 or have the same length as `string`",
      "Your `n` has length {length(n)}.",
      "Your `string` has length {l}."
    )
  }
  locs <- str_locate_all(string, pattern)
  locs_n_matches <- lengths(locs) / 2
  n_negs <- n < 0
  if (any(n_negs)) {
    if (length(n) == 1) {
      n <- locs_n_matches + n + 1
    } else {
      n[n_negs] <- locs_n_matches[n_negs] + n[n_negs] + 1
    }
  }
  out <- matrix(NA, nrow = l, ncol = 2) %>%
    magrittr::set_colnames(c("start", "end"))
  good <- (abs(n) <= locs_n_matches) & (n != 0)
  if (any(good)) {
    if (length(n) > 1) n <- n[good]
    out[good, ] <- locs[good] %>%
      lst_rbind_nth_rows(n)
  }
  out
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
