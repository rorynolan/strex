#' @rdname str_after_nth
#' @export
str_before_nth <- function(string, pattern, n) {
  nth_instance_indices <- str_locate_nth(string, pattern, n)
  str_sub(string, 1, nth_instance_indices[, "start"] - 1)
}

#' @rdname str_after_nth
#' @export
str_before_first <- function(string, pattern) {
  str_before_nth(string = string, pattern = pattern, n = 1)
}

#' @rdname str_after_nth
#' @export
str_before_last <- function(string, pattern) {
  str_before_nth(string = string, pattern = pattern, n = -1)
}

#' Get the part of a string before the last period.
#'
#' This is usually used to get the part of a file name that doesn't include the
#' file extension. It is vectorized over `string`. If there is no period in
#' `string`, the input is returned.
#'
#' @inheritParams str_after_nth
#'
#' @return A character vector.
#'
#' @examples
#' str_before_last_dot(c("spreadsheet1.csv", "doc2.doc", ".R"))
#' @export
str_before_last_dot <- function(string) {
  string %>%
    tools::file_path_sans_ext() %T>% {
      .[(string == .) & (str_elem(., 1) == ".")] <- ""
    }
}
