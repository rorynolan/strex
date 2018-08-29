#' Get the currencies of numbers within a string.
#'
#' The currency of a number is defined as the character coming before the number
#' in the string. If nothing comes before (i.e. if the number is the first thing
#' in the string), the currency is the empty string, similarly the currency can
#' be a space, comma or any manner of thing. \itemize{ \item `get_currency`
#' takes a string and returns the currency of the first number therein. It is
#' vectorized over string. \item `get_currencies` takes a string and returns the
#' currencies of all of the numbers within that string. It is not vectorized. }
#'
#' These functions do not allow for leading decimal points.
#'
#' @name currency
#'
#' @param string A string for `get_currencies()` and a character vector for
#'   `get_currency()`.
#'
#' @return \itemize{ \item `get_currency` returns a character vector. \item
#' `get_currencies` returns a data frame with one column for the currency symbol
#' and one for the amount. }
#' @examples
#' str_get_currencies("35.00 $1.14 abc5 $3.8 77")
#' @export
str_get_currencies <- function(string) {
  checkmate::assert_string(string)
  ssbn <- str_split_by_nums(string, decimals = TRUE, negs = TRUE)[[1]]
  num_indices <- which(str_can_be_numeric(ssbn))
  numbers <- as.numeric(ssbn[num_indices])
  before_num_indices <- num_indices - 1
  before_num_strings <- purrr::map_chr(
    before_num_indices,
    ~ifelse(. %in% num_indices || . < 1, "", ssbn[.])
  )
  currencies <- str_elem(before_num_strings, -1)
  tibble::tibble(currency = currencies, amount = numbers)
}

#' @rdname currency
#' @examples
#' str_get_currency(c("ab3 13", "$1"))
#' @export
str_get_currency <- function(string) {
  checkmate::assert_character(string)
  num_starts <- str_locate(string, "[0-9]")[, "start"]
  before_indices <- num_starts - 1
  str_elem(string, before_indices)
}
