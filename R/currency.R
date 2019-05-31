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
#' Given string numbers, strings and amount locations, output the required
#' tibble.
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
#' be a space, comma or any manner of thing.
#'
#' These functions are vectorized over `string` and `n`.
#'
#' [str_extract_currencies()] extracts all currency amounts.
#'
#' `str_nth_currency()` just gets the `n`th currency amount from each string.
#' `str_first_currency(string)` and `str_last_currency(string)` are just
#' wrappers for `str_nth_currency(string, n = 1)` and `str_nth_currency(string,
#' n = -1)`.
#'
#' "-$2.00" and "$-2.00" are interpreted as negative two dollars.
#'
#' If you request e.g. the 5th currency amount but there are only 3 currency
#' amounts, you get an amount and currency symbol of `NA`.
#'
#' @inheritParams str_after_nth
#'
#' @return A [tibble][tibble::tibble-package] with 4 columns: `string_num`,
#'   `string`, `curr_sym` and `amount`. Every extracted currency amount gets its
#'   own row in the tibble detailing the string number and string that it was
#'   extracted from, the currency symbol and the amount.
#'
#' @examples
#' string <- c("ab3 13", "$1", "35.00 $1.14", "abc5 $3.8", "stuff")
#' str_extract_currencies(string)
#' str_nth_currency(string, n = 2)
#' str_nth_currency(string, n = -2)
#' str_nth_currency(string, c(1, -2, 1, 2, -1))
#' str_first_currency(string)
#' str_last_currency(string)
#' @name currency
#' @family currency extractors
NULL

#' @rdname currency
#' @export
str_extract_currencies <- function(string) {
  if (is_l0_char(string)) {
    return(extract_curr_helper(
      integer(), character(),
      matrix(ncol = 2, nrow = 0)
    ))
  }
  checkmate::assert_character(string)
  locs <- str_locate_all(string, curr_pattern())
  locs_lens <- lengths(locs)
  string_num <- rep(seq_along(string), locs_lens / 2)
  string <- string[string_num]
  locs %<>% lst_rbind(locs_lens)
  extract_curr_helper(string_num, string, locs)
}


#' @rdname currency
#' @export
str_nth_currency <- function(string, n) {
  if (is_l0_char(string)) {
    checkmate::assert_integerish(n)
    return(extract_curr_helper(
      integer(), character(),
      matrix(ncol = 2, nrow = 0)
    ))
  }
  verify_string_n(string, n)
  abs_n <- abs(n)
  if (length(n) == 1 && abs_n == 1) {
    if (n == 1) {
      locs <- stringi::stri_locate_first_regex(string, curr_pattern())
    } else {
      locs <- stringi::stri_locate_last_regex(string, curr_pattern())
    }
  } else {
    locs <- matrix(NA_integer_, ncol = 2, nrow = length(string))
    interim_locs <- str_locate_all(string, curr_pattern())
    interim_locs_n_matches <- lengths(interim_locs) / 2
    good <- interim_locs_n_matches >= abs_n
    n_negs <- n < 0
    if (any(n_negs)) {
      if (length(n) == 1) {
        n <- interim_locs_n_matches + n + 1
      } else {
        n[n_negs] <- interim_locs_n_matches[n_negs] + n[n_negs] + 1
      }
    }
    if (any(good)) {
      if (length(n) > 1) n <- n[good]
      locs[good, ] <- interim_locs[good] %>%
        lst_rbind_nth_rows(n)
    }
  }
  extract_curr_helper(seq_along(string), string, locs)
}

#' @rdname currency
#' @export
str_first_currency <- function(string) {
  str_nth_currency(string, n = 1)
}

#' @rdname currency
#' @export
str_last_currency <- function(string) {
  str_nth_currency(string, n = -1)
}
