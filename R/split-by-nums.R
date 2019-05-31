#' Split a string by its numeric characters assuming no number ambiguity.
#'
#' Sometimes the strings have ambiguous numbers in them e.g. 2.5.3. These have
#' to be dealt with by strex (which it does by returning `NA` in those cases).
#' This helper to `str_split_by_numbers()` assumes that the input has
#' no such ambiguities.
#'
#' @param string A character vector.
#' @param num_pattern The regex defining a numer in the current context.
#'
#' @return A list of character vectors.
#'
#' @noRd
str_split_by_numbers_no_ambigs <- function(string, num_pattern) {
  num_locs <- str_locate_all(string, num_pattern)
  fullocated_substrs(string, num_locs)
}

#' Split a string by its numeric characters.
#'
#' Break a string wherever you go from a numeric character to a non-numeric or
#' vice-versa. Keep the whole string, just split it up. Vectorised over
#' `string`.
#'
#' @inheritParams str_extract_numbers
#'
#' @return A list of character vectors.
#'
#' @examples
#' str_split_by_numbers(c("abc123def456.789gh", "a1b2c344"))
#' str_split_by_numbers("abc123def456.789gh", decimals = TRUE)
#' str_split_by_numbers("22")
#' @family splitters
#' @export
str_split_by_numbers <- function(string, decimals = FALSE,
                                 leading_decimals = FALSE, negs = FALSE,
                                 sci = FALSE, commas = FALSE) {
  if (is_l0_char(string)) {
    return(list())
  }
  checkmate::assert_character(string)
  checkmate::assert_flag(decimals)
  checkmate::assert_flag(leading_decimals)
  checkmate::assert_flag(negs)
  checkmate::assert_flag(sci)
  checkmate::assert_flag(commas)
  num_pattern <- num_regex(
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci, commas = commas
  )
  ambig_pattern <- ambig_num_regex(
    decimals = decimals,
    leading_decimals = leading_decimals,
    sci = sci, commas = commas
  )
  ambigs <- num_ambigs(string,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, commas = commas
  )
  out <- vector(mode = "list", length = length(string))
  if (any(ambigs)) {
    ambig_warn(string, ambigs, ambig_pattern)
    out[ambigs] <- NA_character_
    not_ambigs <- !ambigs
    out[not_ambigs] <- str_split_by_numbers_no_ambigs(
      string[not_ambigs],
      num_pattern
    )
  } else {
    out[] <- str_split_by_numbers_no_ambigs(string, num_pattern)
  }
  out
}
