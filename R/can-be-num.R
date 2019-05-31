#' Check if a string could be considered as numeric.
#'
#' After padding is removed, could the input string be considered to be numeric,
#' i.e. could it be coerced to numeric. This function is vectorized over its one
#' argument.
#'
#' @inheritParams str_after_nth
#'
#' @return A logical vector.
#'
#' @examples
#' str_can_be_numeric("3")
#' str_can_be_numeric("5 ")
#' str_can_be_numeric(c("1a", "abc"))
#' @family type converters
#' @export
str_can_be_numeric <- function(string) {
  checkmate::assert(
    checkmate::check_character(string),
    checkmate::check_numeric(string)
  )
  !is.na(suppressWarnings(as.numeric(string)))
}
