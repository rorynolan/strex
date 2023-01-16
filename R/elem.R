#' Extract a single character from a string, using its index.
#'
#' If the element does not exist, this function returns the empty string. This
#' is consistent with [stringr::str_sub()]. This function is vectorised over
#' both arguments.
#'
#' @inheritParams str_after_nth
#' @param index An integer. Negative indexing is allowed as in
#'   [stringr::str_sub()].
#'
#' @return A one-character string.
#'
#' @examples
#' str_elem(c("abcd", "xyz"), 3)
#' str_elem("abcd", -2)
#' @family single element extractors
#' @export
str_elem <- function(string, index) {
  if (is_l0_char(string)) {
    return(character())
  }
  verify_string_n(string, index, "index")
  str_sub(string, index, index)
}

#' Helper for [str_elems()] and [str_paste_elems()].
#'
#' @return A list of elements of strings. Either with one list element per input
#'   string (orientation: bycol) or one string index (for multiple strings) per
#'   list element (orientation: byrow).
#'
#' @noRd
str_elems_helper <- function(string, indices, insist_bycol = FALSE) {
  indices <- as.integer(indices)
  # The following lapplys can only be easily and efficiently replaced if Rcpp
  # starts dealing with UTF-8 strings well.
  if (!insist_bycol && length(indices) > length(string)) {
    out <- lapply(indices, function(x) str_elem(string, x))
    attr(out, "strex__str_elems_helper__orientation") <- "byrow"
  } else {
    out <- lapply(string, function(x) str_elem(x, indices))
    attr(out, "strex__str_elems_helper__orientation") <- "bycol"
  }
  out
}

#' Extract several single elements from a string.
#'
#' Efficiently extract several elements from a string. See [str_elem()] for
#' extracting single elements. This function is vectorized over the first
#' argument.
#'
#' @inheritParams str_after_nth
#' @param indices A vector of integerish values. Negative indexing is allowed as
#'   in [stringr::str_sub()].
#' @param byrow Should the elements be organised in the matrix with one row per
#'   string (`byrow = TRUE`, the default) or one column per string (`byrow =
#'   FALSE`). See examples if you don't understand.
#'
#' @return A character matrix.
#'
#' @examples
#' string <- c("abc", "def", "ghi", "vwxyz")
#' str_elems(string, 1:2)
#' str_elems(string, 1:2, byrow = FALSE)
#' str_elems(string, c(1, 2, 3, 4, -1))
#' @family single element extractors
#' @export
str_elems <- function(string, indices, byrow = TRUE) {
  checkmate::assert_flag(byrow)
  if (is_l0_char(string)) {
    out <- matrix(character(), ncol = length(indices))
    if (!byrow) out <- t(out)
    return(out)
  }
  checkmate::assert_character(string, min.len = 1)
  checkmate::assert_integerish(indices, min.len = 1)
  out <- str_elems_helper(string, indices)
  if (attr(out, "strex__str_elems_helper__orientation") == "byrow") {
    byrow <- !byrow
  }
  stringi::stri_list2matrix(out, byrow = byrow)
}

#' Extract single elements of a string and paste them together.
#'
#' This is a quick way around doing a call to [str_elems()] followed by a call
#' of `apply(..., paste)`.
#'
#' Elements that don't exist e.g. element 5 of `"abc"` are ignored.
#'
#' @inheritParams str_after_nth
#' @inheritParams str_elems
#' @param sep A string. The separator for pasting `string` elements together.
#'
#' @return A character vector.
#'
#' @examples
#' string <- c("abc", "def", "ghi", "vwxyz")
#' str_paste_elems(string, 1:2)
#' str_paste_elems(string, c(1, 2, 3, 4, -1))
#' str_paste_elems("abc", c(1, 5, 55, 43, 3))
#' @family single element extractors
#' @export
str_paste_elems <- function(string, indices, sep = "") {
  if (is_l0_char(string)) {
    return(character())
  }
  checkmate::assert_character(string, min.len = 1)
  checkmate::assert_integerish(indices, min.len = 1)
  checkmate::assert_string(sep)
  out <- str_elems_helper(string, indices, insist_bycol = TRUE)
  stringi::stri_paste_list(out, sep = sep)
}
