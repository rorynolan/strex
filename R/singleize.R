#' Remove back-to-back duplicates of a pattern in a string.
#'
#' If a string contains a given pattern duplicated back-to-back a number of
#' times, remove that duplication, leaving the pattern appearing once in that
#' position (works if the pattern is duplicated in different parts of a string,
#' removing all instances of duplication). This is vectorized over string and
#' pattern.
#' @param string A character vector. The string(s) to be purged of duplicates.
#' @param pattern A character vector. Pattern(s) specified like the pattern(s)
#'   in the stringr package (e.g. look at [stringr::str_locate()]). If
#'   this has length >1 its length must be the same as that of `string`.
#' @return The string with the duplicates fixed.
#' @examples
#' str_singleize("abc//def", "/")
#' str_singleize("abababcabab", "ab")
#' str_singleize(c("abab", "cdcd"), "cd")
#' str_singleize(c("abab", "cdcd"), c("ab", "cd"))
#' @export
str_singleize <- function(string, pattern) {
  dup_patt <- str_c("(", pattern, ")+")
  str_replace_all(string, dup_patt, pattern)
}
