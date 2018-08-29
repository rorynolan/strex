#' Convert a string to a vector of characters
#'
#' Go from a string to a vector whose \eqn{i}th element is the \eqn{i}th
#' character in the string.
#' @param string A string.
#' @return A character vector.
#' @examples
#' str_to_vec("abcdef")
#' @export
str_to_vec <- function(string) {
  checkmate::assert_character(string)
  strsplit(string, NULL)[[1]]
}
