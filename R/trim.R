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
#' str_trim_anything("-ghi--", "-", "both")
#' str_trim_anything("-ghi--", "-")
#' str_trim_anything("-ghi--", "-", "right")
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

  out <- string
  checkmate::assert_string(side)
  side %<>% match_arg(c("both", "left", "right"), ignore_case = TRUE)
  type <- "regex"
  if (all(c("fixed", "pattern") %in% class(pattern))) {
    type <- "fixed"
  } else if (all(c("coll", "pattern") %in% class(pattern))) {
    type <- "coll"
  } else {
    pattern <- str_c("(", pattern, ")+")
  }
  if (side == "left") {
    starts <- which(str_starts(out, pattern))
    while (any(starts)) {
      out[starts] <- switch(
        type,
        regex = stringi::stri_replace_first_regex(
          out[starts],
          pattern[ifelse(length(pattern) == 1, 1, starts)],
          ""
        ),
        fixed = stringi::stri_replace_first_fixed(
          out[starts],
          pattern[ifelse(length(pattern) == 1, 1, starts)],
          ""
        ),
        coll = stringi::stri_replace_first_coll(
          out[starts],
          pattern[ifelse(length(pattern) == 1, 1, starts)],
          ""
        ),
      )
      starts <- starts[str_starts(out[starts], pattern)]
    }
  }
  if (side == "right") {
    ends <- which(str_ends(out, pattern))
    while (any(ends)) {
      out[ends] <- switch(
        type,
        regex = stringi::stri_replace_last_regex(
          out[ends],
          pattern[ifelse(length(pattern) == 1, 1, starts)],
          ""
        ),
        fixed = stringi::stri_replace_last_fixed(
          out[ends],
          pattern[ifelse(length(pattern) == 1, 1, starts)],
          ""
        ),
        coll = stringi::stri_replace_last_coll(
          out[ends],
          pattern[ifelse(length(pattern) == 1, 1, starts)],
          ""
        ),
      )
      ends <- ends[str_ends(out[ends], pattern)]
    }
  }
  if (side == "both") {
    out <- string %>%
      str_trim_anything(pattern, "left") %>%
      str_trim_anything(pattern, "right")
  }
  out
}
