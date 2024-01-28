#' Find the `n`th number before the `m`th occurrence of a pattern.
#'
#' Given a string, a pattern and natural numbers `n` and `m`, find the `n`th
#' number that comes before the `m`th occurrence of the pattern.
#'
#' @inheritParams str_nth_number_after_mth
#'
#' @return A numeric or character vector.
#'
#' @examples
#' string <- c(
#'   "abc1abc2abc3abc4def5abc6abc7abc8abc9",
#'   "abc1def2ghi3abc4def5ghi6abc7def8ghi9"
#' )
#' str_nth_number_before_mth(string, "def", 1, 1)
#' str_nth_number_before_mth(string, "abc", 2, 3)
#' @family numeric extractors
#' @export
str_nth_number_before_mth <- function(string, pattern, n, m,
                                      decimals = FALSE,
                                      leading_decimals = decimals,
                                      negs = FALSE,
                                      sci = FALSE, big_mark = "",
                                      leave_as_string = FALSE,
                                      commas = FALSE) {
  if (!isFALSE(commas)) {
    lifecycle::deprecate_stop(
      "2.0.0", "strex::str_nth_number_before_mth(commas)",
      details = "Use the `big_mark` argument instead."
    )
  }
  checkmate::assert_flag(leave_as_string)
  if (is_l0_char(string)) {
    return(vector(mode = ifelse(leave_as_string, "character", "numeric")))
  }
  verify_string_pattern_n_m(string, pattern, n, m)
  checkmate::assert_flag(decimals)
  checkmate::assert_flag(leading_decimals)
  checkmate::assert_flag(negs)
  checkmate::assert_flag(sci)
  checkmate::assert_character(big_mark)
  string %>%
    str_before_nth(pattern, m) %>%
    str_nth_number(n,
      decimals = decimals, leading_decimals = leading_decimals,
      negs = negs, leave_as_string = leave_as_string, sci = sci,
      big_mark = big_mark
    )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_nth_number_before_first(string, "def", 2)
#' @export
str_nth_number_before_first <- function(string, pattern, n,
                                        decimals = FALSE,
                                        leading_decimals = decimals,
                                        negs = FALSE,
                                        sci = FALSE, big_mark = "",
                                        leave_as_string = FALSE,
                                        commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = n, m = 1,
    decimals = decimals, sci = sci, big_mark = big_mark,
    leading_decimals = leading_decimals,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_nth_number_before_last(string, "def", -1)
#' @export
str_nth_number_before_last <- function(string, pattern, n,
                                       decimals = FALSE,
                                       leading_decimals = decimals,
                                       negs = FALSE,
                                       sci = FALSE, big_mark = "",
                                       leave_as_string = FALSE,
                                       commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = n, m = -1,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_first_number_before_mth(string, "abc", 2)
#' @export
str_first_number_before_mth <- function(string, pattern, m,
                                        decimals = FALSE,
                                        leading_decimals = decimals,
                                        negs = FALSE,
                                        sci = FALSE, big_mark = "",
                                        leave_as_string = FALSE,
                                        commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = 1, m = m,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_last_number_before_mth(string, "def", 1)
#' @export
str_last_number_before_mth <- function(string, pattern, m,
                                       decimals = FALSE,
                                       leading_decimals = decimals,
                                       negs = FALSE,
                                       sci = FALSE, big_mark = "",
                                       leave_as_string = FALSE,
                                       commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = -1, m = m,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_first_number_before_first(string, "def")
#' @export
str_first_number_before_first <- function(string, pattern,
                                          decimals = FALSE,
                                          leading_decimals = decimals,
                                          negs = FALSE,
                                          sci = FALSE, big_mark = "",
                                          leave_as_string = FALSE,
                                          commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = 1, m = 1,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_first_number_before_last(string, "def")
#' @export
str_first_number_before_last <- function(string, pattern,
                                         decimals = FALSE,
                                         leading_decimals = decimals,
                                         negs = FALSE,
                                         sci = FALSE, big_mark = "",
                                         leave_as_string = FALSE,
                                         commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = 1, m = -1,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_last_number_before_first(string, "def")
#' @export
str_last_number_before_first <- function(string, pattern,
                                         decimals = FALSE,
                                         leading_decimals = decimals,
                                         negs = FALSE,
                                         sci = FALSE, big_mark = "",
                                         leave_as_string = FALSE,
                                         commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = -1, m = 1,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}

#' @rdname str_nth_number_before_mth
#' @examples
#' str_last_number_before_last(string, "def")
#' @export
str_last_number_before_last <- function(string, pattern,
                                        decimals = FALSE,
                                        leading_decimals = decimals,
                                        negs = FALSE,
                                        sci = FALSE, big_mark = "",
                                        leave_as_string = FALSE,
                                        commas = FALSE) {
  str_nth_number_before_mth(string, pattern,
    n = -1, m = -1,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, big_mark = big_mark,
    negs = negs, leave_as_string = leave_as_string,
    commas = commas
  )
}
