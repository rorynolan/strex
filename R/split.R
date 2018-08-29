#' Split a string based on CamelCase
#'
#' Vectorized over `string`.
#'
#' @param string A character vector.
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
#' @export
str_split_camel_case <- function(string, lower = FALSE) {
  string %<>% gsub("^[^[:alnum:]]+|[^[:alnum:]]+$", "", .) %>%
    gsub("(?!^)(?=[[:upper:]])", " ", ., perl = TRUE)
  if (lower) string %<>% str_to_lower()
  str_split(string, " ")
}
