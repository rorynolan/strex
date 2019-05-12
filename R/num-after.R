#' Find the `n`th number after the `m`th occurrence of a pattern.
#'
#' Given a string, a pattern and natural numbers `n` and `m`, find the `n`th
#' number after the `m`th occurrence of the pattern.
#'
#' @param n,m Vectors of integerish values. Must be either length 1 or have
#'   length equal to the length of `string`. Negative indices count from the
#'   back: while `1` and `2` correspond to first and second, `-1` and `-2`
#'   correspond to last and second-last. `0` will return `NA`.
#' @inheritParams str_after_nth
#' @inheritParams str_extract_numbers
#'
#' @return A numeric or character vector.
#'
#' @examples
#' string <- c(
#'   "abc1abc2abc3abc4abc5abc6abc7abc8abc9",
#'   "abc1def2ghi3abc4def5ghi6abc7def8ghi9"
#' )
#' str_nth_number_after_mth(string, "abc", 1, 3)
#' str_nth_number_after_mth(string, "abc", 2, 3)
#'
#' @family numeric extractors
#' @export
str_nth_number_after_mth <- function(string, pattern, n, m,
                                     decimals = FALSE,
                                     leading_decimals = decimals,
                                     negs = FALSE, sci = FALSE, commas = FALSE,
                                     leave_as_string = FALSE) {
  checkmate::assert_flag(leave_as_string)
  if (is_l0_char(string))
    return(vector(mode = ifelse(leave_as_string, "character", "numeric")))
  verify_string_pattern_n_m(string, pattern, n, m)
  checkmate::assert_flag(decimals)
  checkmate::assert_flag(leading_decimals)
  checkmate::assert_flag(negs)
  checkmate::assert_flag(sci)
  checkmate::assert_flag(commas)
  string %>%
    str_after_nth(pattern, m) %>%
    str_nth_number(n,
      decimals = decimals, leading_decimals = leading_decimals,
      negs = negs, leave_as_string = leave_as_string, sci = sci,
      commas = commas
    )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_nth_number_after_first(string, "abc", 2)
#' @export
str_nth_number_after_first <- function(string, pattern, n, decimals = FALSE,
                                       leading_decimals = decimals,
                                       negs = FALSE, sci = FALSE,
                                       commas = FALSE,
                                       leave_as_string = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = n, m = 1,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, commas = commas,
    negs = negs, leave_as_string = leave_as_string
  )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_nth_number_after_last(string, "abc", -1)
#' @export
str_nth_number_after_last <- function(string, pattern, n,
                                      decimals = FALSE,
                                      leading_decimals = decimals,
                                      negs = FALSE, sci = FALSE,
                                      commas = FALSE,
                                      leave_as_string = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = n, m = -1,
    decimals = decimals,
    leading_decimals = leading_decimals,
    negs = negs, leave_as_string = leave_as_string
  )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_first_number_after_mth(string, "abc", 2)
#' @export
str_first_number_after_mth <- function(string, pattern, m,
                                       decimals = FALSE,
                                       leading_decimals = decimals,
                                       negs = FALSE, sci = FALSE,
                                       commas = FALSE,
                                       leave_as_string = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = 1, m = m,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, commas = commas,
    negs = negs, leave_as_string = leave_as_string
  )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_last_number_after_mth(string, "abc", 1)
#' @export
str_last_number_after_mth <- function(string, pattern, m,
                                      decimals = FALSE,
                                      leading_decimals = decimals,
                                      negs = FALSE, sci = FALSE,
                                      commas = FALSE,
                                      leave_as_string = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = -1, m = m,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, commas = commas,
    negs = negs, leave_as_string = leave_as_string
  )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_first_number_after_first(string, "abc")
#' @export
str_first_number_after_first <- function(string, pattern,
                                         decimals = FALSE,
                                         leading_decimals = decimals,
                                         negs = FALSE, sci = FALSE,
                                         commas = FALSE,
                                         leave_as_string = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = 1, m = 1,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, commas = commas,
    negs = negs, leave_as_string = leave_as_string
  )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_first_number_after_last(string, "abc")
#' @export
str_first_number_after_last <- function(string, pattern,
                                        decimals = FALSE,
                                        leading_decimals = decimals,
                                        negs = FALSE,
                                        sci = FALSE, commas = FALSE,
                                        leave_as_string = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = 1, m = -1,
    decimals = decimals, sci = sci, commas = commas,
    leading_decimals = leading_decimals,
    negs = negs, leave_as_string = leave_as_string
  )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_last_number_after_first(string, "abc")
#' @export
str_last_number_after_first <- function(string, pattern, decimals = FALSE,
                                        leading_decimals = decimals,
                                        negs = FALSE,
                                        sci = FALSE, commas = FALSE,
                                        leave_as_string = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = -1, m = 1,
    decimals = decimals, sci = sci, commas = commas,
    leading_decimals = leading_decimals,
    negs = negs, leave_as_string = leave_as_string
  )
}

#' @rdname str_nth_number_after_mth
#' @examples
#' str_last_number_after_last(string, "abc")
#' @export
str_last_number_after_last <- function(string, pattern,
                                       decimals = FALSE,
                                       leading_decimals = decimals,
                                       negs = FALSE,
                                       sci = FALSE, commas = FALSE,
                                       leave_as_string = FALSE) {
  str_nth_number_after_mth(string, pattern,
    n = -1, m = -1, decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, commas = commas,
    negs = negs, leave_as_string = leave_as_string
  )
}
