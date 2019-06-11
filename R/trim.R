#' Trim something other than whitespace
#'
#' The `stringi` and `stringr` packages let you trim whitespace, but
#' what if you want to trim something else from either (or both) side(s) of a
#' string? This function lets you select which pattern to trim and from which
#' side(s).
#'
#' @inheritParams str_after_nth
#' @param side Which side do you want to trim from? `"both"` is the
#'   default, but you can also have just either `"left"` or `"right"`
#'   (or optionally the shortened `"b"`, `"l"` and `"r"`).
#'
#' @return A string.
#' @examples
#' str_trim_anything("..abcd.", ".", "left")
#' str_trim_anything("..abcd.", coll("."), "left")
#' str_trim_anything("-ghi--", "-")
#' str_trim_anything("-ghi--", "--")
#' str_trim_anything("-ghi--", "i-+")
#' @family removers
#' @export
str_trim_anything <- function(string, pattern, side = "both") {
  if (is_l0_char(string)) {
    return(character())
  }
  if (all(c("boundary", "pattern") %in% class(pattern))) {
    custom_stop(
      "`str_trim_anything()` cannot handle a `pattern` of type 'boundary'."
    )
  }
  verify_string_pattern(string, pattern)
  checkmate::assert_string(side)
  side %<>% match_arg(c("both", "left", "right"), ignore_case = TRUE)
  if (any(c("coll", "fixed") %in% class(pattern)) &&
      "pattern" %in% class(pattern)) {
    pattern %<>% ore::ore.escape()
  }
  pattern %<>% str_c("(", ., ")")
  switch(side,
    both = str_replace(string, str_c("^", pattern, "*"), "") %>%
      str_replace(str_c(pattern, "*$"), ""),
    left = str_replace(string, str_c("^", pattern, "*"), ""),
    right = str_replace(string, str_c(pattern, "*$"), "")
  )
}
