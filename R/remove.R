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
#' @export
str_remove_quoted <- function(string) {
  string <- str_replace_all(string, "(?:\".*?\")", "")
  string <- str_replace_all(string, "(?:\'.*?\')", "")
  string
}
