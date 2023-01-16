#' Create a regex pattern for finding numbers in strings.
#'
#' There are options for finding decimal numbers, negative numbers and
#' scientific notation, but the default simply finds consecutive numeric
#' characters (0123456789). See the examples to understand how these options
#' work.
#'
#' @inheritParams str_extract_numbers
#'
#' @return A string. The appropriate regex pattern for searching for the chosen
#'   types of numbers in strings.
#'
#' @examples
#' num_regex()
#' num_regex(decimals = TRUE)
#' num_regex(decimals = TRUE, leading_decimals = TRUE)
#' num_regex(negs = TRUE)
#' num_regex(
#'   decimals = TRUE, leading_decimals = TRUE,
#'   negs = TRUE, sci = TRUE
#' )
#' @noRd
num_regex <- function(decimals = FALSE, leading_decimals = decimals,
                      negs = FALSE, sci = FALSE, commas = FALSE) {
  if (leading_decimals == TRUE && decimals == FALSE) {
    rlang::abort(
      c("To allow leading decimals, you need to first allow decimals.",
        i = "To allow decimals, use `decimals = TRUE`.")
    )
  }
  dec_pattern <- ifelse(commas, "\\d+(?:,?\\d+)*(?:\\.\\d+)?",
    "\\d+(?:\\.\\d+)?"
  )
  leading_dec_pattern <- ifelse(commas,
    str_c(
      "(?:(?:\\d+(?:,?\\d+)*(?:\\.\\d+)?)|",
      "(?:\\.?\\d+))"
    ),
    "(?:\\d+(?:\\.\\d+)?|\\.?\\d+)"
  )
  non_dec_pattern <- ifelse(commas, "\\d+(?:,?\\d+)*", "\\d+")
  pattern <- non_dec_pattern
  if (decimals) {
    pattern <- ifelse(leading_decimals, leading_dec_pattern, dec_pattern)
  }
  if (sci) {
    pattern <- stringr::str_glue(
      "(?:(?:{pattern}[eE][+-]?{non_dec_pattern})|",
      "(?:{pattern}))"
    )
  }
  if (negs) pattern <- stringr::str_glue("-?(?:{pattern})")
  pattern
}

ambig_num_regex <- function(decimals = FALSE, leading_decimals = decimals,
                            sci = FALSE, commas = FALSE) {
  out <- character(1)
  if (!any(decimals, leading_decimals, sci)) {
    return(out)
  }
  if (decimals) {
    if (commas) {
      out <- ifelse(leading_decimals, "\\.(?:\\d+,?)+\\.\\d",
        "(?:\\d+,?)+\\.(?:\\d+,?)+\\.\\d"
      )
    } else {
      out <- ifelse(leading_decimals, "\\.\\d+\\.\\d", "\\d\\.\\d+\\.\\d")
    }
  }
  if (sci) {
    if (commas) {
      sci_bit <- ifelse(
        decimals,
        "\\d\\.?\\d*[eE](?:\\d+(?:,\\d+)*)+(?:\\.\\d|\\.[eE]\\d|[eE]\\d)", ###
        "\\d[eE](?:\\d+(?:,\\d+)*)+[eE]\\d"
      )
    } else {
      sci_bit <- ifelse(
        decimals,
        "\\d\\.?\\d*[eE]\\d+(?:\\.\\d|\\.[eE]\\d|[eE]\\d)",
        "\\d[eE]\\d+[eE]\\d"
      )
    }
    out <- ifelse(decimals, stringr::str_glue("({sci_bit})|({out})"), sci_bit)
  }
  out
}

num_ambigs <- function(string, decimals = FALSE, leading_decimals = decimals,
                       sci = FALSE, commas = FALSE) {
  if (!any(c(decimals, sci))) {
    return(FALSE)
  }
  str_detect(string, ambig_num_regex(
    decimals = decimals, leading_decimals = leading_decimals,
    sci = sci, commas = commas
  ))
}

ambig_warn <- function(string, ambigs, ambig_regex) {
  first_offender <- match(T, ambigs) %>%
    list(., string[.])
  first_offender[[2]] <- ifelse(
    str_length(first_offender[[2]]) > 50,
    str_c(str_sub(first_offender[[2]], 1, 17), "..."),
    first_offender[[2]]
  )
  rlang::warn(
    c(
      "`NA`s introduced by ambiguity.",
      i = str_glue("The first such ambiguity is in string number ",
                   "{first_offender[[1]]} which is '{first_offender[[2]]}'."),
      x = str_glue("The offending part of that string is ",
                   "'{str_extract(first_offender[[2]], ambig_regex)}'.")
    )
  )
}

#' Extract numbers from a string.
#'
#' Extract the numbers from a string, where decimals, scientific notation and
#' commas (as separators, not as an alternative to the decimal point) are
#' optionally allowed.
#'
#' If any part of a string contains an ambiguous number (e.g. `1.2.3` would be
#' ambiguous if `decimals = TRUE` (but not otherwise)), the value returned for
#' that string will be `NA` and a `warning` will be issued.
#'
#' With scientific notation, it is assumed that the exponent is not a decimal
#' number e.g. `2e2.4` is unacceptable. Commas, however, are acceptable in the
#' exponent, so 2e1,100 is fine and equal to 2e1100 if the option to allow
#' commas in numbers has been turned on.
#'
#' Numbers outside the double precision floating point range (i.e. with absolute
#' value greater than 1.797693e+308) are read as `Inf` (or `-Inf` if they begin
#' with a minus sign). This is what `base::as.numeric()` does.
#'
#' @param string A string.
#' @param decimals Do you want to include the possibility of decimal numbers
#'   (`TRUE`) or not (`FALSE`, the default).
#' @param leading_decimals Do you want to allow a leading decimal point to be
#'   the start of a number?
#' @param negs Do you want to allow negative numbers? Note that double negatives
#'   are not handled here (see the examples).
#' @param sci Make the search aware of scientific notation e.g. 2e3 is the same
#'   as 2000.
#' @param commas Allow comma separators in numbers (i.e. interpret 1,100 as a
#'   single number (one thousand one hundred) rather than two numbers (one and
#'   one hundred)).
#' @param leave_as_string Do you want to return the number as a string (`TRUE`)
#'   or as numeric (`FALSE`, the default)?
#'
#'
#' @return For `str_extract_numbers` and `str_extract_non_numerics`, a list of
#'   numeric or character vectors, one list element for each element of
#'   `string`. For `str_nth_number` and `str_nth_non_numeric`, a numeric or
#'   character vector the same length as the vector `string`.
#' @examples
#' strings <- c(
#'   "abc123def456", "abc-0.12def.345", "abc.12e4def34.5e9",
#'   "abc1,100def1,230.5", "abc1,100e3,215def4e1,000"
#' )
#' str_extract_numbers(strings)
#' str_extract_numbers(strings, decimals = TRUE)
#' str_extract_numbers(strings, decimals = TRUE, leading_decimals = TRUE)
#' str_extract_numbers(strings, commas = TRUE)
#' str_extract_numbers(strings,
#'   decimals = TRUE, leading_decimals = TRUE,
#'   sci = TRUE
#' )
#' str_extract_numbers(strings,
#'   decimals = TRUE, leading_decimals = TRUE,
#'   sci = TRUE, commas = TRUE, negs = TRUE
#' )
#' str_extract_numbers(strings,
#'   decimals = TRUE, leading_decimals = FALSE,
#'   sci = FALSE, commas = TRUE, leave_as_string = TRUE
#' )
#' str_extract_numbers(c("22", "1.2.3"), decimals = TRUE)
#' @family numeric extractors
#' @export
str_extract_numbers <- function(string,
                                decimals = FALSE, leading_decimals = decimals,
                                negs = FALSE, sci = FALSE, commas = FALSE,
                                leave_as_string = FALSE) {
  checkmate::assert_character(string)
  checkmate::assert_flag(leave_as_string)
  checkmate::assert_flag(decimals)
  checkmate::assert_flag(leading_decimals)
  checkmate::assert_flag(negs)
  checkmate::assert_flag(sci)
  checkmate::assert_flag(commas)
  if (is_l0_char(string)) {
    return(list())
  }
  pattern <- num_regex(
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci, commas = commas
  )
  ambig_pattern <- ambig_num_regex(
    decimals = decimals,
    leading_decimals = leading_decimals,
    sci = sci, commas = commas
  )
  ambigs <- num_ambigs(string,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci, commas = commas
  )
  out <- vector(mode = "list", length = length(string))
  if (any(ambigs)) {
    ambig_warn(string, ambigs, ambig_regex = ambig_pattern)
    out[ambigs] <- NA_character_
    not_ambigs <- !ambigs
    out[not_ambigs] <- str_extract_all(string[not_ambigs], pattern)
  } else {
    out[] <- str_extract_all(string, pattern)
  }
  if (leave_as_string) {
    return(out)
  }
  lst_chr_to_dbl(out, commas = commas)
}

#' Extract the `n`th number from a string.
#'
#' Extract the `n`th number from a string, where decimals, scientific notation
#' and commas (as separators, not as an alternative to the decimal point) are
#' optionally allowed.
#'
#' \itemize{ \item `str_first_number(...)` is just `str_nth_number(..., n = 1)`.
#' \item `str_last_number(...)` is just `str_nth_number(..., n = -1)`. }
#'
#' For a detailed explanation of the number extraction, see
#' [str_extract_numbers()].
#'
#' @inheritParams str_extract_numbers
#' @inheritParams str_after_nth
#'
#' @return A numeric vector (or a character vector if `leave_as_string = TRUE`).
#'
#' @examples
#' strings <- c(
#'   "abc123def456", "abc-0.12def.345", "abc.12e4def34.5e9",
#'   "abc1,100def1,230.5", "abc1,100e3,215def4e1,000"
#' )
#' str_nth_number(strings, n = 2)
#' str_nth_number(strings, n = -2, decimals = TRUE)
#' str_first_number(strings, decimals = TRUE, leading_decimals = TRUE)
#' str_last_number(strings, commas = TRUE)
#' str_nth_number(strings,
#'   n = 1, decimals = TRUE, leading_decimals = TRUE,
#'   sci = TRUE
#' )
#' str_first_number(strings,
#'   decimals = TRUE, leading_decimals = TRUE,
#'   sci = TRUE, commas = TRUE, negs = TRUE
#' )
#' str_last_number(strings,
#'   decimals = TRUE, leading_decimals = FALSE,
#'   sci = FALSE, commas = TRUE, negs = TRUE, leave_as_string = TRUE
#' )
#' str_first_number(c("22", "1.2.3"), decimals = TRUE)
#' @family numeric extractors
#' @export
str_nth_number <- function(string, n, decimals = FALSE,
                           leading_decimals = decimals, negs = FALSE,
                           sci = FALSE, commas = FALSE,
                           leave_as_string = FALSE) {
  checkmate::assert_flag(leave_as_string)
  if (is_l0_char(string)) {
    return(vector(mode = ifelse(leave_as_string, "character", "numeric")))
  }
  verify_string_n(string, n)
  checkmate::assert_flag(decimals)
  checkmate::assert_flag(leading_decimals)
  checkmate::assert_flag(negs)
  checkmate::assert_flag(sci)
  checkmate::assert_flag(commas)
  out <- character(length(string))
  if (int_vec_all_value(n, 1) || int_vec_all_value(n, -1)) {
    pattern <- num_regex(
      decimals = decimals, leading_decimals = leading_decimals,
      negs = negs, sci = sci, commas = commas
    )
    ambig_pattern <- ambig_num_regex(
      decimals = decimals,
      leading_decimals = leading_decimals,
      sci = sci, commas = commas
    )
    ambigs <- FALSE
    if (str_length(ambig_pattern)) ambigs <- str_detect(string, ambig_pattern)
    if (any(ambigs)) {
      ambig_warn(string, ambigs, ambig_regex = ambig_pattern)
      not_ambigs <- !ambigs
      out[ambigs] <- NA_character_
      if (n[[1]] == 1) {
        out[not_ambigs] <- stringi::stri_extract_first_regex(
          string[not_ambigs],
          pattern
        )
      } else {
        out[not_ambigs] <- stringi::stri_extract_last_regex(
          string[not_ambigs],
          pattern
        )
      }
    } else {
      if (n[[1]] == 1) {
        out[] <- stringi::stri_extract_first_regex(string, pattern)
      } else {
        out[] <- stringi::stri_extract_last_regex(string, pattern)
      }
    }
  } else {
    numbers <- str_extract_numbers(string,
      leave_as_string = TRUE, negs = negs, sci = sci,
      decimals = decimals,
      leading_decimals = leading_decimals, commas = commas
    )
    out <- chr_lst_nth_elems(numbers, n)
  }
  if (leave_as_string) {
    return(out)
  }
  if (commas) out <- str_replace_all(out, ",", "")
  as.numeric(out)
}

#' @rdname str_nth_number
#' @export
str_first_number <- function(string, decimals = FALSE,
                             leading_decimals = decimals, negs = FALSE,
                             sci = FALSE, commas = FALSE,
                             leave_as_string = FALSE) {
  str_nth_number(string,
    n = 1, leave_as_string = leave_as_string,
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci, commas = commas
  )
}

#' @rdname str_nth_number
#' @export
str_last_number <- function(string, decimals = FALSE,
                            leading_decimals = decimals, negs = FALSE,
                            sci = FALSE, commas = FALSE,
                            leave_as_string = FALSE) {
  str_nth_number(string,
    n = -1, leave_as_string = leave_as_string,
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci, commas = commas
  )
}
