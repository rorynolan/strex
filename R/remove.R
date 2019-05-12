#' Remove the quoted parts of a string.
#'
#' If any parts of a string are quoted (between quotation marks), remove those
#' parts of the string, including the quotes. Run the examples and you'll know
#' exactly how this function works.
#'
#' @param string A character vector.
#'
#' @return A character vector.
#' @examples
#' string <- "\"abc\"67a\'dk\'f"
#' cat(string)
#' str_remove_quoted(string)
#'
#' @family removers
#' @export
str_remove_quoted <- function(string) {
  if (is_l0_char(string)) return(character())
  checkmate::assert_character(string)
  string <- str_replace_all(string, "(?:\".*?\")", "")
  string <- str_replace_all(string, "(?:\'.*?\')", "")
  string
}
