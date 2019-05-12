#' Split a string based on CamelCase.
#'
#' Vectorized over `string`.
#'
#' @inheritParams str_after_nth
#' @param lower Do you want the output to be all lower case (or as is)?
#'
#' @return A list of character vectors, one list element for each element of
#'   `string`.
#'
#' @references Adapted from Ramnath Vaidyanathan's answer at
#' http://stackoverflow.com/questions/8406974/splitting-camelcase-in-r.
#'
#' @examples
#' str_split_camel_case(c("RoryNolan", "NaomiFlagg", "DepartmentOfSillyHats"))
#'
#' @family splitters
#' @export
str_split_camel_case <- function(string, lower = FALSE) {
  if (is_l0_char(string)) return(list())
  checkmate::assert_character(string)
  checkmate::assert_flag(lower)
  string %<>% gsub("^[^[:alnum:]]+|[^[:alnum:]]+$", "", .) %>%
    gsub("(?!^)(?=[[:upper:]])", " ", ., perl = TRUE)
  if (lower) string %<>% str_to_lower()
  str_split(string, " ")
}
