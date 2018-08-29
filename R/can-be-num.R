#' Check if a string could be considered as numeric.
#'
#' After padding is removed, could the input string be considered to be numeric,
#' i.e. could it be coerced to numeric. This function is vectorized over its one
#' argument.
#' @param string A character vector.
#' @return A character vector. `TRUE` if the argument can be considered to be
#'   numeric or `FALSE` otherwise.
#' @examples
#' str_can_be_numeric("3")
#' str_can_be_numeric("5 ")
#' str_can_be_numeric(c("1a", "abc"))
#' @export
str_can_be_numeric <- function(string) {
  !is.na(suppressWarnings(as.numeric(string)))
}
