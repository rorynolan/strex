#' Trim something other than whitespace
#'
#' The `stringi` and `stringr` packages let you trim whitespace, but
#' what if you want to trim something else from either (or both) side(s) of a
#' string? This function lets you select which pattern to trim and from which
#' side(s).
#'
#' @inheritParams str_after_nth
#' @param pattern A string. The pattern to be trimmed (*not* interpreted as
#'   regular expression). So to trim a period, use `char = "."` and not
#'   `char = "\\\\."`).
#' @param side Which side do you want to trim from? `"both"` is the
#'   default, but you can also have just either `"left"` or `"right"`
#'   (or optionally the shortened `"b"`, `"l"` and `"r"`).
#'
#' @return A string.
#' @examples
#' str_trim_anything("..abcd.", ".", "left")
#' str_trim_anything("-ghi--", "-")
#' str_trim_anything("-ghi--", "--")
#'
#' @family removers
#' @export
str_trim_anything <- function(string, pattern, side = "both") {
  if (all_equal(string, character())) return(character())
  verify_string_pattern(string, pattern)
  checkmate::assert_string(side)
  side %<>% match_arg(c("both", "left", "right"), ignore_case = TRUE)
  pattern <- ore::ore.escape(pattern) %>%
    str_c("(?:", ., ")")
  switch(side,
    both = str_replace(string, str_c("^", pattern, "*"), "") %>%
      str_replace(str_c(pattern, "*$"), ""),
    left = str_replace(string, str_c("^", pattern, "*"), ""),
    right = str_replace(string, str_c(pattern, "*$"), "")
  )
}
