#' Extract numbers from a string.
#'
#' `str_extract_numbers` extracts the numbers (or non-numbers) from a string
#' where decimals are optionally allowed. `str_nth_number` is a convenient
#' wrapper for `str_extract_numbers`, allowing you to choose which number you
#' want. Please run the examples at the bottom of this page to ensure that you
#' understand how these functions work, and their limitations. These functions
#' are vectorized over `string`.
#'
#' If any part of a string contains an ambiguous number (e.g. `1.2.3` would be
#' ambiguous if `decimals = TRUE` (but not otherwise)), the value returned for
#' that string will be `NA`. Note that these functions do not know about
#' scientific notation (e.g. `1e6` for 1000000).
#'
#' \itemize{ \item `str_first_number(...)` is just `str_nth_number(..., n = 1)`.
#' \item `str_last_number(...)` is just `str_nth_number(..., n = -1)`. }
#'
#' @param string A string.
#' @param leave_as_string Do you want to return the number as a string (`TRUE`)
#'   or as numeric (`FALSE`, the default)?
#' @param decimals Do you want to include the possibility of decimal numbers
#'   (`TRUE`) or not (`FALSE`, the default).
#' @param leading_decimals Do you want to allow a leading decimal point to be
#'   the start of a number?
#' @param negs Do you want to allow negative numbers? Note that double negatives
#'   are not handled here (see the examples).
#' @return For `str_extract_numbers` and `extract_non_numerics`, a list of
#'   numeric or character vectors, one list element for each element of
#'   `string`. For `str_nth_number` and `nth_non_numeric`, a vector the same
#'   length as `string` (as in `length(string)`, not `nchar(string)`).
#' @examples
#' str_extract_numbers(c("abc123abc456", "abc1.23abc456"))
#' str_extract_numbers(c("abc1.23abc456", "abc1..23abc456"), decimals = TRUE)
#' str_extract_numbers("abc1..23abc456", decimals = TRUE)
#' str_extract_numbers("abc1..23abc456", decimals = TRUE, leading_decimals = TRUE)
#' str_extract_numbers("abc1..23abc456", decimals = TRUE, leading_decimals = TRUE,
#'                 leave_as_string = TRUE)
#' str_extract_numbers("-123abc456")
#' str_extract_numbers("-123abc456", negs = TRUE)
#' str_extract_numbers("--123abc456", negs = TRUE)
#' str_extract_numbers(c(rep("abc1.2.3", 2), "a1b2.2.3", "e5r6"), decimals = TRUE)
#' str_extract_numbers("ab.1.2", decimals = TRUE, leading_decimals = TRUE)
#' str_nth_number("abc1.23abc456", 2:3)
#' str_nth_number("abc1.23abc456", 2, decimals = TRUE)
#' str_nth_number("-123abc456", -2, negs = TRUE)
#'
#' @export
str_extract_numbers <- function(string, leave_as_string = FALSE,
                                decimals = FALSE, leading_decimals = FALSE,
                                negs = FALSE) {
  if (leading_decimals == TRUE && decimals == FALSE) {
    custom_stop(
      "To allow leading decimals, you need to first allow decimals.",
      "To allow decimals, use `decimals = TRUE`."
    )
  }
  stopifnot(is.character(string))
  if (decimals) {
    pattern <- "(?:[0-9]+(?:\\.?[0-9]+)*)+"
    if (leading_decimals) pattern <- str_c("\\.?", pattern)
  } else {
    pattern <- "[0-9]+"
  }
  if (negs) pattern <- str_c("-?", pattern)
  numbers <- str_extract_all(string, pattern)
  numerics <- suppressWarnings(lst_char_to_num(numbers))
  if (!decimals && isTRUE(checkmate::check_integerish(unlist(numerics)))) {
    numerics %<>% purrr::map(as.integer)
  }
  na_pos <- purrr::map_lgl(numerics, anyNA)
  if (leave_as_string) {
    numbers[na_pos] <- NA_character_
  } else {
    numbers <- numerics
    if (decimals) {
      numbers[na_pos] <- NA_real_
    } else {
      numbers[na_pos] <- NA_integer_
    }
  }
  numbers
}

#' @rdname str_extract_numbers
#' @param n The index of the number (or non-numeric) that you seek. Negative
#'   indexing is allowed i.e. `n = 1` (the default) will give you the first
#'   number (or non-numeric) whereas `n = -1` will give you the last number (or
#'   non-numeric), `n = -2` will give you the second last number and so on. The
#'   function is vectorized over this argument.
#' @export
str_nth_number <- function(string, n, leave_as_string = FALSE, decimals = FALSE,
                           leading_decimals = FALSE, negs = FALSE) {
  checkmate::assert_numeric(n)
  checkmate::assert_numeric(abs(n), lower = 1)
  numbers <- str_extract_numbers(string,
    leave_as_string = TRUE, negs = negs,
    decimals = decimals,
    leading_decimals = leading_decimals
  )
  str_nth_numbers <- str_list_nth_elems(numbers, n)
  if (leave_as_string) {
    str_nth_numbers
  } else {
    if (decimals) {
      as.numeric(str_nth_numbers)
    } else {
      as.integer(str_nth_numbers)
    }
  }
}

#' @rdname str_extract_numbers
#' @export
str_first_number <- function(string, leave_as_string = FALSE, decimals = FALSE,
                             leading_decimals = FALSE, negs = FALSE) {
  str_nth_number(string,
    n = 1, leave_as_string = leave_as_string,
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs
  )
}

#' @rdname str_extract_numbers
#' @export
str_last_number <- function(string, leave_as_string = FALSE, decimals = FALSE,
                            leading_decimals = FALSE, negs = FALSE) {
  str_nth_number(string,
    n = -1, leave_as_string = leave_as_string,
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs
  )
}
