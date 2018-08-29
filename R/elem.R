#' Extract a single character from a string, using its index.
#'
#' If the element does not exist, this function returns the empty string.
#'
#' @param string A string.
#' @param index An integer. Negative indexing is allowed as in
#'   [stringr::str_sub()].
#' @return A one-character string.
#' @examples
#' str_elem(c("abcd", "xyz"), 3)
#' str_elem("abcd", -2)
#' @export
str_elem <- function(string, index) {
  checkmate::assert_character(string)
  str_sub(string, index, index)
}

#' Extract bits of a string and paste them together
#'
#' Extract characters - specified by their indices - from a string and paste
#' them together
#' @param string A string.
#' @param indices A numeric vector of positive integers detailing the indices of
#'   the characters of `string` that we wish to paste together.
#' @return A string.
#' @examples
#' str_paste_elems("abcdef", c(2, 5:6))
#' @export
str_paste_elems <- function(string, indices) {
  checkmate::assert_character(string)
  stopifnot(length(string) == 1)
  elems <- str_elem(string, indices)
  pasted <- paste(elems, collapse = "")
  return(pasted)
}
