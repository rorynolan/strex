#' Number pattern for currency.
#'
#' @return A string.
#'
#' @noRd
curr_pattern <- function() {
  num_regex(decimals = TRUE, sci = TRUE, negs = TRUE, commas = TRUE)
}

#' Helper for currency extration.
#'
#' Given string numbers, strings and amount locations, output the required tibble.
#'
#' @param string_num The string number.
#' @param string A character vector.
#' @param locs An integer matrix. The amount locations.
#'
#' @return A tibble.
#'
#' @noRd
extract_curr_helper <- function(string_num, string, locs) {
  amount <- string %>%
    str_sub(locs[, 1], locs[, 2]) %>%
    str_replace_all(stringr::coll(","), "") %>%
    as.numeric()
  curr_sym_pos <- locs[, 1] - 1
  curr_sym <- str_elem(string, curr_sym_pos)
  sign_sym_pos <- ifelse(curr_sym_pos == 0, 0, curr_sym_pos - 1)
  curr_sym_sign <- ifelse(str_elem(string, sign_sym_pos) == "-", -1, 1)
  amount <- amount * curr_sym_sign
  tibble::new_tibble(list(
    string_num = string_num, string = string,
    curr_sym = curr_sym, amount = amount
  ),
  nrow = length(string)
  )
}

#' Extract currency amounts from a string.
#'
#' The currency of a number is defined as the character coming before the number
#' in the string. If nothing comes before (i.e. if the number is the first thing
#' in the string), the currency is the empty string, similarly the currency can
#' be a space, comma or any manner of thing. `str_extract_currencies()` takes a
#' string and returns all of the currency amounts numbers within that string. It
#' is vectorized over `string`.
#'
#' \itemize{\item "-$2.00" and "$-2.00" are interpreted as negative two dollars.
#' \item If you request e.g. the 5th currency amount but there are only 3
#' currency amounts, you get an amount and currency symbol of `NA`.}
#'
#' @param string A character vector.
#'
#' @return A [tibble][tibble::tibble-package] with 4 columns: `string_num`,
#'   `string`, `curr_sym` and `amount`. Every extracted currency amount gets its
#'   own row in the tibble detailing the string number and string that it was
#'   extracted from, the currency symbol and the amount.
#'
#' @seealso [str_nth_currency()]
#'
#' @examples
#' str_extract_currencies(c(
#'   "35.00 $1.14", "abc5 $3.8 77",
#'   "-$1.5e6", "over £1,000"
#' ))
#' @export
str_extract_currencies <- function(string) {
  checkmate::assert_character(string, min.len = 1)
  locs <- str_locate_all(string, curr_pattern())
  locs_lens <- lengths(locs)
  string_num <- rep(seq_along(string), locs_lens / 2)
  string <- string[string_num]
  locs %<>% lst_rbind(locs_lens)
  extract_curr_helper(string_num, string, locs)
}

#' Extract the `n`th currency amount from a string.
#'
#' These functions are vectorized over `string` and `n`.
#' [str_extract_currencies()] extracts all currency amounts but
#' `str_nth_currency()` just gets the `n`th currency amount from each string.
#' `str_first_currency(string)` and `str_last_currency(string)` are just
#' wrappers for `str_nth_currency(string, n = 1)` and `str_nth_currency(string,
#' n = -1)`.
#'
#' \itemize{\item "-$2.00" and "$-2.00" are interpreted as negative two dollars.
#' \item If you request e.g. the 5th currency amount but there are only 3
#' currency amounts, you get an amount and currency symbol of `NA`.}
#'
#' @param string A character vector.
#' @param n A numeric vector of length 1 or the same length as `string`.
#'
#' @return A [tibble][tibble::tibble-package] with the same style as the return
#'   from [str_extract_currencies()]. 4 columns: `string_num`, `string`,
#'   `curr_sym` and `amount`. Every extracted currency amount gets its own row
#'   in the tibble detailing the string number and string that it was extracted
#'   from, the currency symbol and the amount.
#'
#' @examples
#' string <- c("ab3 13", "$1", "35.00 $1.14", "abc5 $3.8", "stuff")
#' str_nth_currency(string, n = 2)
#' str_nth_currency(string, c(1, 2, 1, 2, 1))
#' str_first_currency(string)
#' str_last_currency(string)
#' @export
str_nth_currency <- function(string, n) {
  checkmate::assert_character(string, min.len = 1)
  checkmate::assert_integerish(n)
  if (length(n) > 1 && length(n) != length(string)) {
    custom_stop(
      "`n` must either be length 1 or have the same length as `string`",
      "Your `n` has length {length(n)}.",
      "Your `string` has length {length(string)}."
    )
  }
  if (length(n) == 1 && abs(n) == 1) {
    if (n == 1) {
      locs <- stringi::stri_locate_first_regex(string, curr_pattern())
    } else {
      locs <- stringi::stri_locate_last_regex(string, curr_pattern())
    }
  } else {
    locs <- matrix(NA, ncol = 2, nrow = length(string))
    interim_locs <- str_locate_all(string, curr_pattern())
    interim_locs_n_matches <- lengths(interim_locs) / 2
    n_negs <- n < 0
    if (any(n_negs)) n[n_negs] <- interim_locs_n_matches[n_negs] + n[n_negs] + 1
    good <- interim_locs_n_matches >= n
    if (any(good)) {
      if (length(n) > 1) n <- n[good]
      locs[good] <- interim_locs[good] %>%
        lst_rbind_nth_rows(n)
    }
  }
  extract_curr_helper(seq_along(string), string, locs)
}

#' @rdname str_nth_currency
#' @export
str_first_currency <- function(string) {
  str_nth_currency(string, n = 1)
}

#' @rdname str_nth_currency
#' @export
str_last_currency <- function(string) {
  str_nth_currency(string, n = -1)
}
