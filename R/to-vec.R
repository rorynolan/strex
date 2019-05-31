#' Convert a string to a vector of characters
#'
#' Go from a string to a vector whose \eqn{i}th element is the \eqn{i}th
#' character in the string.
#'
#' @inheritParams str_after_nth
#'
#' @return A character vector.
#'
#' @examples
#' str_to_vec("abcdef")
#' @family converters
#' @export
str_to_vec <- function(string) {
  if (is_l0_char(string)) {
    return(character())
  }
  checkmate::assert_character(string)
  strsplit(string, NULL)[[1]]
}
