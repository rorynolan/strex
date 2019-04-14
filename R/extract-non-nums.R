#' Extract non-numerics assuming no number ambiguity.
#'
#' Sometimes the strings have ambiguous numbers in them e.g. 2.5.3. These have
#' to be dealt with by strex (which it does by returning `NA` in those cases).
#' This helper to `str_extract_non_numerics()` assumes that the input has
#' no such ambiguities.
#'
#' @param string A character vector.
#' @param num_pattern The regex defining a numer in the current context.
#'
#' @return A list of character vectors.
#'
#' @noRd
str_extract_non_numerics_no_ambigs <- function(string, num_pattern) {
  stringi::stri_split_regex(string, num_pattern, omit_empty = TRUE)
}

#' Extract non-numbers from a string.
#'
#' `str_extract_non_numerics` extracts the bits of the string that aren't
#' extracted by `extract_numbers`. `str_nth_non_numeric` is a convenient wrapper
#' for `str_extract_non_numerics`, allowing you to choose which number you want.
#' Please run the examples at the bottom of this page to ensure that you
#' understand how these functions work, and their limitations. These functions
#' are vectorized over `string`.
#'
#' \itemize{ \item `str_first_non_numeric(...)` is just
#' `str_nth_non_numeric(..., n = 1)`. \item `str_last_non_numeric(...)` is just
#' `str_nth_non_numeric(..., n = -1)`. }
#'
#' @inheritParams str_extract_numbers
#'
#' @examples
#' str_extract_non_numerics("abc123abc456")
#' str_extract_non_numerics("abc1.23abc456")
#' str_extract_non_numerics("abc1.23abc456", decimals = TRUE)
#' str_extract_non_numerics("abc1..23abc456", decimals = TRUE)
#' str_extract_non_numerics("abc1..23abc456",
#'   decimals = TRUE,
#'   leading_decimals = TRUE
#' )
#' str_extract_non_numerics(c("-123abc456", "ab1c"))
#' str_extract_non_numerics("-123abc456", negs = TRUE)
#' str_extract_non_numerics("--123abc456", negs = TRUE)
#' str_extract_non_numerics("--123abc456", negs = TRUE)
#' str_nth_non_numeric("--123abc456", 1)
#' str_nth_non_numeric("--123abc456", -2)
#' @export
str_extract_non_numerics <- function(string, decimals = FALSE,
                                     leading_decimals = FALSE, negs = FALSE,
                                     sci = FALSE) {
  checkmate::assert_character(string)
  num_pattern <- num_regex(
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci
  )
  ambig_pattern <- ambig_num_regex(
    decimals = decimals,
    leading_decimals = leading_decimals,
    sci = sci
  )
  ambigs <- num_ambigs(string,
                       decimals = decimals,
                       leading_decimals = leading_decimals, sci = sci
  )
  out <- vector(mode = "list", length = length(string))
  if (any(ambigs)) {
    ambig_warn(string, ambigs)
    out[ambigs] <- NA_character_
    not_ambigs <- !ambigs
    out[not_ambigs] <- str_extract_non_numerics_no_ambigs(string[not_ambigs],
                                                          num_pattern)
  } else {
    out[] <- str_extract_non_numerics_no_ambigs(string, num_pattern)
  }
  out
}

#' Extract the `n`th non-numeric substring from a string.
#'
#' This is a helper for `str_nth_non_numeric()` which assums non-ambiguous input.
#'
#' @param string A character vector.
#' @param pattern The regex pattern defining a number.
#' @inheritParams str_nth_number
#'
#' @return A character vector
#'
#' @noRd
str_nth_non_numeric_no_ambigs <- function(string, num_pattern, n) {
  if (length(n) == 1 && n >= 0) {
    out <- stringi::stri_split_regex(string, num_pattern,
                              n = n + 1,
                              omit_empty = TRUE, simplify = TRUE)[, n]
    out %T>% {
      .[!str_length(.)] <- NA_character_
    }
  } else {
    stringi::stri_split_regex(string, num_pattern, omit_empty = TRUE) %>%
      str_list_nth_elems(n)
  }
}

#' @rdname str_extract_non_numerics
#' @export
str_nth_non_numeric <- function(string, n, decimals = FALSE,
                                leading_decimals = FALSE, negs = FALSE,
                                sci = FALSE) {
  checkmate::assert_numeric(n)
  checkmate::assert_numeric(abs(n), lower = 1)
  checkmate::assert_character(string)
  num_pattern <- num_regex(
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci
  )
  ambig_pattern <- ambig_num_regex(
    decimals = decimals,
    leading_decimals = leading_decimals,
    sci = sci
  )
  ambigs <- num_ambigs(string,
                       decimals = decimals,
                       leading_decimals = leading_decimals, sci = sci
  )
  out <- character(length(string))
  if (any(ambigs)) {
    ambig_warn(string, ambigs)
    out[ambigs] <- NA_character_
    not_ambigs <- !ambigs
    out[not_ambigs] <- str_nth_non_numeric_no_ambigs(string[not_ambigs],
                                                     num_pattern, n)
  } else {
    out[] <- str_nth_non_numeric_no_ambigs(string, num_pattern, n)
  }
  out
}

#' @rdname str_extract_non_numerics
#' @export
str_first_non_numeric <- function(string, decimals = FALSE,
                                  leading_decimals = FALSE, negs = FALSE,
                                  sci = FALSE) {
  str_nth_non_numeric(string,
    n = 1,
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci
  )
}

#' @rdname str_extract_non_numerics
#' @export
str_last_non_numeric <- function(string, decimals = FALSE,
                                 leading_decimals = FALSE, negs = FALSE,
                                 sci = FALSE) {
  str_nth_non_numeric(string,
    n = -1,
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci
  )
}
