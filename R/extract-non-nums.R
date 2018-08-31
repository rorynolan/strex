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
#' str_extract_non_numerics("abc1..23abc456", decimals = TRUE,
#'                      leading_decimals = TRUE)
#' str_extract_non_numerics(c("-123abc456", "ab1c"))
#' str_extract_non_numerics("-123abc456", negs = TRUE)
#' str_extract_non_numerics("--123abc456", negs = TRUE)
#' str_extract_non_numerics("--123abc456", negs = TRUE)
#' str_nth_non_numeric("--123abc456", 1)
#' str_nth_non_numeric("--123abc456", -2)
#'
#' @export
str_extract_non_numerics <- function(string, decimals = FALSE,
                                     leading_decimals = FALSE, negs = FALSE) {
  if (leading_decimals == TRUE && decimals == FALSE) {
    custom_stop(
      "To allow leading decimals, you need to first allow decimals.",
      "To allow decimals, use `decimals = TRUE`."
    )
  }
  checkmate::assert_character(string)
  if (decimals) {
    pattern <- "(?:[0-9]+(?:\\.?[0-9]+)*)+"
    if (leading_decimals) pattern <- str_c("\\.?", pattern)
  } else {
    pattern <- "[0-9]+"
  }
  if (negs) pattern <- str_c("-?", pattern)
  non_numerics <- str_split(string, pattern) %>% str_list_remove_empties()
  numerics <- str_extract_numbers(string,
    decimals = decimals,
    leading_decimals = leading_decimals, negs = negs
  )
  na_pos <- purrr::map_lgl(numerics, anyNA)
  non_numerics[na_pos] <- NA_character_
  non_numerics
}

#' @rdname str_extract_non_numerics
#' @export
str_nth_non_numeric <- function(string, n, decimals = FALSE,
                                leading_decimals = FALSE, negs = FALSE) {
  checkmate::assert_numeric(n)
  checkmate::assert_numeric(abs(n), lower = 1)
  non_numerics <- str_extract_non_numerics(string,
    decimals = decimals, negs = negs,
    leading_decimals = leading_decimals
  )
  str_list_nth_elems(non_numerics, n)
}

#' @rdname str_extract_non_numerics
#' @export
str_first_non_numeric <- function(string, decimals = FALSE,
                                  leading_decimals = FALSE, negs = FALSE) {
  str_nth_non_numeric(string,
    n = 1,
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs
  )
}

#' @rdname str_extract_non_numerics
#' @export
str_last_non_numeric <- function(string, decimals = FALSE,
                                 leading_decimals = FALSE, negs = FALSE) {
  str_nth_non_numeric(string,
    n = -1,
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs
  )
}
