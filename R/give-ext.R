#' Ensure a file name has the intended extension.
#'
#' Say you want to ensure a name is fit to be the name of a csv file. Then, if
#' the input doesn't end with ".csv", this function will tack ".csv" onto the
#' end of it. This is vectorized over the first argument.
#'
#' @param string The intended file name.
#' @param ext The intended file extension (with or without the ".").
#' @param replace If the file has an extension already, replace it (or append
#'   the new extension name)?
#'
#' @return A string: the file name in your intended form.
#'
#' @examples
#' str_give_ext(c("abc", "abc.csv"), "csv")
#' str_give_ext("abc.csv", "pdf")
#' str_give_ext("abc.csv", "pdf", replace = TRUE)
#'
#' @family appenders
#' @export
str_give_ext <- function(string, ext, replace = FALSE) {
  if (is_l0_char(string)) return(character())
  checkmate::assert_character(string)
  checkmate::assert_string(ext)
  checkmate::assert_flag(replace)
  ext <- str_match(ext, "^\\.*(.*)")[, 2]
  if (replace) {
    string %<>% str_before_last_dot()
  } else {
    correct_ext <- str_detect(string, str_c("\\.", ext, "$"))
    string[correct_ext] %<>% str_before_last_dot()
  }
  str_c(string, ".", ext)
}
