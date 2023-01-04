#' Detect any or all patterns.
#'
#' Vectorized over `pattern`.
#'
#' @param string A character vector.
#' @param pattern A character vector. The patterns to look for. Default is
#'   `stringi`-style regular expression. [stringr::coll()] and
#'   [stringr::fixed()] are also permissible.
#' @param negate A flag. If `TRUE`, inverts the result.
#'
#' @return A character vector.
#'
#' @examples
#' str_detect_all("quick brown fox", c("x", "y", "z"))
#' str_detect_all(c(".", "-"), ".")
#' str_detect_all(c(".", "-"), coll("."))
#' str_detect_all(c(".", "-"), coll("."), negate = TRUE)
#' str_detect_all(c(".", "-"), c(".", ":"))
#' str_detect_all(c(".", "-"), coll(c(".", ":")))
#' str_detect_all("xyzabc", c("a", "c", "z"))
#' str_detect_all(c("xyzabc", "abcxyz"), c(".b", "^x"))
#'
#' @export
str_detect_all <- function(string, pattern, negate = FALSE) {
  checkmate::assert_character(string)
  if (inherits(pattern, "stringr_boundary")) {
    custom_stop("Function cannot handle a `pattern` of type 'boundary'.")
  }
  checkmate::assert_character(pattern, min.chars = 1)
  checkmate::assert_flag(negate)
  if (inherits(pattern, "stringr_coll") || inherits(pattern, "stringr_fixed")) {
    if (inherits(pattern, "stringr_coll")) {
      out <- str_detect_many_coll(string, pattern)
    } else {
      out <- str_detect_many_fixed(string, pattern)
    }
    out <- Reduce(`&`, out)
  } else {
    pattern <- pattern %>%
      str_c("(?=.*", ., ")") %>%
      str_flatten() %>%
      str_c("^", .)
    out <- stringr::str_detect(string, pattern)
  }
  if (negate) out <- !out
  out
}

#' @rdname str_detect_all
#'
#' @examples
#' str_detect_any("quick brown fox", c("x", "y", "z"))
#' str_detect_any(c(".", "-"), ".")
#' str_detect_any(c(".", "-"), coll("."))
#' str_detect_any(c(".", "-"), coll("."), negate = TRUE)
#' str_detect_any(c(".", "-"), c(".", ":"))
#' str_detect_any(c(".", "-"), coll(c(".", ":")))
#' str_detect_any(c("xyzabc", "abcxyz"), c(".b", "^x"))
#'
#' @export
str_detect_any <- function(string, pattern, negate = FALSE) {
  checkmate::assert_character(string)
  if (inherits(pattern, "stringr_boundary")) {
    custom_stop("Function cannot handle a `pattern` of type 'boundary'.")
  }
  checkmate::assert_character(pattern, min.chars = 1)
  checkmate::assert_flag(negate)
  if (inherits(pattern, "stringr_coll") || inherits(pattern, "stringr_fixed")) {
    if (inherits(pattern, "stringr_coll")) {
      out <- str_detect_many_coll(string, pattern)
    } else {
      out <- str_detect_many_fixed(string, pattern)
    }
    out <- Reduce(`|`, out)
  } else {
    out <- str_detect(string, str_flatten(pattern, "|"))
  }
  if (negate) out <- !out
  out
}
