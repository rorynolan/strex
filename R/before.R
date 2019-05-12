#' @rdname before-and-after
#' @export
str_before_nth <- function(string, pattern, n) {
  if (is_l0_char(string)) return(character())
  verify_string_pattern_n(string, pattern, n)
  nth_instance_indices <- str_locate_nth(string, pattern, n)
  str_sub(string, 1, nth_instance_indices[, "start"] - 1)
}

#' @rdname before-and-after
#' @export
str_before_first <- function(string, pattern) {
  str_before_nth(string = string, pattern = pattern, n = 1)
}

#' @rdname before-and-after
#' @export
str_before_last <- function(string, pattern) {
  str_before_nth(string = string, pattern = pattern, n = -1)
}

#' Extract the part of a string before the last period.
#'
#' This is usually used to get the part of a file name that doesn't include the
#' file extension. It is vectorized over `string`. If there is no period in
#' `string`, the input is returned.
#'
#' @inheritParams before-and-after
#'
#' @return A character vector.
#'
#' @examples
#' str_before_last_dot(c("spreadsheet1.csv", "doc2.doc", ".R"))
#'
#' @family bisectors
#' @export
str_before_last_dot <- function(string) {
  if (is_l0_char(string)) return(character())
  checkmate::assert_character(string)
  string %>%
    tools::file_path_sans_ext() %T>% {
      .[(string == .) & (str_elem(., 1) == ".")] <- ""
    }
}
