#' Remove back-to-back duplicates of a pattern in a string.
#'
#' If a string contains a given pattern duplicated back-to-back a number of
#' times, remove that duplication, leaving the pattern appearing once in that
#' position (works if the pattern is duplicated in different parts of a string,
#' removing all instances of duplication). This is vectorized over string and
#' pattern.
#'
#' @inheritParams str_after_nth
#'
#' @return A character vector.
#'
#' @examples
#' str_singleize("abc//def", "/")
#' str_singleize("abababcabab", "ab")
#' str_singleize(c("abab", "cdcd"), "cd")
#' str_singleize(c("abab", "cdcd"), c("ab", "cd"))
#' @family removers
#' @export
str_singleize <- function(string, pattern) {
  if (is_l0_char(string)) {
    return(character())
  }
  verify_string_pattern(string, pattern)
  dup_patt <- str_c("(", pattern, ")+")
  str_replace_all(string, dup_patt, pattern)
}
