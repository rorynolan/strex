#' @rdname str_after_nth
#' @export
str_before_nth <- function(strings, pattern, n) {
  nth_instance_indices <- str_locate_nth(strings, pattern, n)
  str_sub(strings, 1, nth_instance_indices[, "start"] - 1)
}

#' @rdname str_after_nth
#' @export
str_before_first <- function(strings, pattern) {
  str_before_nth(strings = strings, pattern = pattern, n = 1)
}

#' @rdname str_after_nth
#' @export
str_before_last <- function(strings, pattern) {
  str_before_nth(strings = strings, pattern = pattern, n = -1)
}

#' Get the part of a string before the last period.
#'
#' This is usually used to get the part of a file name that doesn't include the
#' file extension. It is vectorized over `string`. If there is no period in
#' `string`, the input is returned.
#'
#' @param string A character vector.
#'
#' @return A character vector.
#'
#' @examples
#' str_before_last_dot(c("spreadsheet1.csv", "doc2.doc", ".R"))
#'
#' @export
str_before_last_dot <- function(string) {
  string %>%
    tools::file_path_sans_ext() %T>% {
      .[(string == .) & (str_elem(., 1) == ".")] <- ""
    }
}
