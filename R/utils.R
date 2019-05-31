#' Get the nth element of each vector in a list of numeric or character vectors.
#'
#' These are faster implementations of procedures that could very easily be done
#' with [purrr::map_dbl] or [purrr::map_chr].
#'
#' @param char_list A list of character vectors.
#' @param n The index of the element that you want from each vector. If
#'   `char_list` (or `num_list`) is of length 1, this can be any length and
#'   those indices will be extracted from `char_list[[1]]` (or `num_list[[1]]`).
#'   Otherwise, this must either be of length 1 or the same length as
#'   `char_list`. All of this is to say that the function is vectorised over
#'   this argument.
#'
#' @return A list.
#'
#' @examples
#' str_list_nth_elems_(list(c("a", "b", "c"), c("d", "f", "a")), 2)
#' num_list_nth_elems_(list(1:5, 0:2), 4)
#' @noRd
str_list_nth_elems <- function(char_list, n) {
  checkmate::assert_list(char_list, min.len = 1)
  checkmate::assert_integerish(n, min.len = 1)
  lcl <- length(char_list)
  ln <- length(n)
  if (lcl > 1 && ln > 1 && lcl != ln) {
    custom_stop("
      If both `char_list` and `n` have lengths greater than 1,
      then their lengths must be equal.
      ", "
      Your `char_list` has length {length(char_list)} and
      your `n` has length {length(n)}.
    ")
  }
  str_list_nth_elems_helper(char_list, n)
}


#' @rdname str_list_nth_elems
#' @param num_list A list of numeric vectors.
#' @noRd
num_list_nth_elems <- function(num_list, n) {
  checkmate::assert_list(num_list, min.len = 1)
  checkmate::assert_integerish(n, min.len = 1)
  lnl <- length(num_list)
  ln <- length(n)
  if (lnl > 1 && ln > 1 && lnl != ln) {
    custom_stop("
      If both `num_list` and `n` have lengths greater than 1,
      then their lengths must be equal.
      ", "
      Your `num_list` has length {length(num_list)} and
      your `n` has length {length(n)}.
      ")
  }
  num_list_nth_elems_(num_list, n)
}

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
  string %<>% strwrap(width = 57)
  string[1] %<>% {
    glue::glue("    * {.}")
  }
  if (length(string) > 1) {
    string[-1] %<>% {
      glue::glue("      {.}")
    }
  }
  glue::glue_collapse(string, sep = "\n")
}

custom_condition_prep <- function(main_message, ..., .envir = parent.frame()) {
  checkmate::assert_string(main_message)
  main_message %<>% glue::glue(.envir = .envir)
  out <- strwrap(main_message, width = 63)
  dots <- unlist(list(...))
  if (length(dots)) {
    if (!is.character(dots)) {
      stop("\nThe arguments in ... must all be of character type.")
    }
    dots %<>% vapply(glue::glue, character(1), .envir = .envir) %>%
      vapply(custom_bullet, character(1))
    out %<>% {
      glue::glue_collapse(c(., dots), sep = "\n")
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

#' Generate an error due to an incompatible combination of arguemnt lengths.
#'
#' @param string A character vector.
#' @param sym Another argument to a strex function.
#' @param replacement_sym A string to replace sym in the error message.
#'
#' @noRd
err_string_len <- function(string, sym, replacement_sym = NULL) {
  sym_sym <- rlang::enexpr(sym)
  sym_str <- as.character(sym_sym)
  if (!is.null(replacement_sym)) sym_str <- replacement_sym
  sym_len <- length(sym)
  custom_stop(
    "
    When `string` has length greater than 1,
    `{sym_str}` must either be length 1 or have the same length as `string`.
    ",
    "Your `string` has length {length(string)}.",
    "Your `{sym_str}` has length {sym_len}."
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
  verify_string_pattern(string, pattern)
  verify_string_n(string, n, replacement_n_sym)
  n_sym_str <- "n"
  if (!is.null(replacement_n_sym)) n_sym_str <- replacement_n_sym
  if (length(pattern) > 1 && length(n) > 1 &&
    length(pattern) != length(n)) {
    custom_stop(
      "
                If `pattern` and `{n_sym_str}` both have length greater than 1,
                their lengths must be equal.
                ",
      "Your `pattern` has length {length(pattern)}.",
      "Your `{n_sym_str}` has length {length(n)}."
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
