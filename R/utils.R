#' Construct the bullet point bits for `custom_stop()`.
#'
#' @param string The message for the bullet point.
#'
#' @return A string with the bullet-pointed message nicely formatted for the
#'   console.
#'
#' @noRd
custom_bullet <- function(string) {
  checkmate::assert_string(string)
  string %>%
    stringr::str_replace_all("\\s+", " ") %>%
    {
      stringr::str_glue("    * {.}")
    }
}

custom_condition_prep <- function(main_message, ..., .envir = parent.frame()) {
  checkmate::assert_string(main_message)
  main_message %<>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_glue(.envir = .envir) %>%
    stringr::str_trim()
  out <- main_message
  dots <- unlist(list(...))
  if (length(dots)) {
    if (!is.character(dots)) {
      stop("\nThe arguments in ... must all be of character type.")
    }
    dots %<>%
      vapply(stringr::str_glue, character(1), .envir = .envir) %>%
      vapply(custom_bullet, character(1))
    out %<>% {
      stringr::str_c(c(., dots), collapse = "\n")
    }
  }
  out
}

#' Nicely formatted error message.
#'
#' Format an error message with bullet-pointed sub-messages with nice
#' line-breaks.
#'
#' Arguments should be entered as `glue`-style strings.
#'
#' @param main_message The main error message.
#' @param ... Bullet-pointed sub-messages.
#'
#' @noRd
custom_stop <- function(main_message, ..., .envir = parent.frame()) {
  rlang::abort(custom_condition_prep(main_message, ..., .envir = .envir))
}

custom_warn <- function(main_message, ..., .envir = parent.frame()) {
  rlang::warn(custom_condition_prep(main_message, ..., .envir = .envir))
}

#' Assert that two objects have compatible lengths.
#'
#' Compatible means that either both have length less than or equal to 1, or
#' both have the same length.
#'
#' @param x, y Objects
#'
#' @return `TRUE`, invisibly if the lengths are compatible. Otherwise an error
#'   is thrown.
#'
#' @noRd
assert_compatible_lengths <- function(x, y) {
  x_sym <- rlang::ensym(x)
  y_sym <- rlang::ensym(y)
  checkmate::assert_vector(x)
  checkmate::assert_vector(y)
  if (length(x) > 1 && length(y) > 1) {
    if (length(x) != length(y)) {
      custom_stop(
        "If both `{x_sym}` and `{y_sym}` have lengths greater than 1,
        then their lengths must be equal.",
        "`{x_sym}` has length {length(x)}.",
        "`{y_sym}` has length {length(y)}.",
        .envir = list(x = x, y = y, x_sym = x_sym, y_sym = y_sym)
      )
    }
  }
  invisible(TRUE)
}

#' Assert that the elements of a list have a common length.
#'
#' @param lst A list.
#'
#' @return `TRUE` (invisibly) if the elements have a common length. Otherwise,
#'   an error is thrown.
#'
#' @noRd
assert_lst_elems_common_length <- function(lst) {
  lst_sym <- rlang::ensym(lst)
  checkmate::assert_list(lst)
  l <- length(lst)
  if (l <= 1) {
    return(invisible(TRUE))
  }
  good <- lst_elems_common_length(lst, as.double(l))
  if (!good) {
    custom_stop("Elements of `{lst_sym}` do not have a common length.",
      .envir = list(lst_sym = lst_sym)
    )
  }
  invisible(TRUE)
}

#' Generate an error due to an incompatible combination of arguemnt lengths.
#'
#' @param string A character vector.
#' @param sym Another argument to a strex function.
#' @param replacement_sym A string to replace sym in the error message.
#'
#' @noRd
err_string_len <- function(string, sym, replacement_sym = NULL) {
  sym_sym <- rlang::enexpr(sym)
  if (!is.null(replacement_sym)) {
    sym_str <- replacement_sym
  } else {
    sym_str <- as.character(sym_sym)
  }
  custom_stop(
    "
    When `string` has length greater than 1,
    `{sym_str}` must either be length 1 or have the same length as `string`.
    ",
    "Your `string` has length {length(string)}.",
    "Your `{sym_str}` has length {sym_len}.",
    .envir = list(
      sym_str = sym_str, sym_len = length(sym),
      string = string
    )
  )
}

verify_string_pattern <- function(string, pattern) {
  checkmate::assert_character(string, min.len = 1)
  checkmate::assert_character(pattern, min.len = 1)
  if (length(pattern) > 1 && length(string) > 1 &&
    length(pattern) != length(string)) {
    err_string_len(string, pattern)
  }
  invisible(TRUE)
}

verify_string_n <- function(string, n, replacement_n_sym = NULL) {
  checkmate::assert_character(string, min.len = 1)
  checkmate::assert_integerish(n, min.len = 1)
  if (length(n) > 1 && length(string) > 1 &&
    length(n) != length(string)) {
    err_string_len(string, n, replacement_n_sym)
  }
  invisible(TRUE)
}

verify_string_pattern_n <- function(string, pattern, n,
                                    replacement_n_sym = NULL) {
  if (!is.null(replacement_n_sym)) {
    n_sym_str <- replacement_n_sym
  } else {
    n_sym_str <- as.character(rlang::ensym(n))
  }
  verify_string_n(string, n, replacement_n_sym)
  verify_string_pattern(string, pattern)
  if (length(pattern) > 1 && length(n) > 1 &&
    length(pattern) != length(n)) {
    custom_stop(
      "If `pattern` and `n` both have length greater than 1,
      their lengths must be equal.",
      "Your `pattern` has length {length(pattern)}.",
      "Your `{n_sym_str}` has length {length(n)}.",
      .envir = list(pattern = pattern, n = n, n_sym_str = n_sym_str)
    )
  }
  invisible(TRUE)
}

verify_string_pattern_n_m <- function(string, pattern, n, m) {
  verify_string_pattern_n(string, pattern, n)
  checkmate::assert_integerish(m, min.len = 1)
  verify_string_pattern_n(string, pattern, m, "m")
  if (length(n) > 1 && length(m) > 1 &&
    length(n) != length(m)) {
    custom_stop(
      "
                If `n` and `m` both have length greater than 1,
                their lengths must be equal.
                ",
      "Your `n` has length {length(n)}.",
      "Your `m` has length {length(m)}."
    )
  }
  invisible(TRUE)
}

is_l0_char <- function(x) isTRUE(checkmate::check_character(x, max.len = 0))
