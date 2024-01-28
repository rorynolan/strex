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
      rlang::abort(
        c(
          str_glue(
            "If both `{x_sym}` and `{y_sym}` have lengths greater ",
            "than 1, then their lengths must be equal."
          ),
          x = str_glue("`{x_sym}` has length {length(x)}."),
          x = str_glue("`{y_sym}` has length {length(y)}.")
        )
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
    rlang::abort(
      str_glue("Elements of `{lst_sym}` do not have a common length.")
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
  rlang::abort(
    c(
      str_glue(
        "When `string` has length greater than 1, `{sym_str}` ",
        "must either be length 1 or have the same length as `string`."
      ),
      x = str_glue("Your `string` has length {length(string)}."),
      x = str_glue("Your `{sym_str}` has length {length(sym)}.")
    )
  )
}

verify_string_pattern <- function(string, pattern, boundary_allowed = TRUE) {
  checkmate::assert_character(string, min.len = 1)
  checkmate::assert_flag(boundary_allowed)
  if (boundary_allowed) {
    if (inherits(pattern, "stringr_boundary")) {
      checkmate::assert_character(pattern, min.len = 0)
    } else {
      checkmate::assert_character(pattern, min.len = 1)
    }
  } else if (inherits(pattern, "stringr_boundary")) {
    rlang::abort("Function cannot handle a `pattern` of type 'boundary'.")
  } else {
    checkmate::assert_character(pattern, min.len = 1)
  }
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
    rlang::abort(
      c(
        paste(
          "If `pattern` and `n` both have length greater than 1,",
          "their lengths must be equal."
        ),
        x = str_glue("Your `pattern` has length {length(pattern)}."),
        x = str_glue("Your `{n_sym_str}` has length {length(n)}.")
      )
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
    rlang::abort(
      c(
        paste(
          "If `n` and `m` both have length greater than 1,",
          "their lengths must be equal."
        ),
        x = str_glue("Your `n` has length {length(n)}."),
        x = str_glue("Your `m` has length {length(m)}.")
      )
    )
  }
  invisible(TRUE)
}

is_l0_char <- function(x) isTRUE(checkmate::check_character(x, max.len = 0))
