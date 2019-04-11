#' Create a regex pattern for finding numbers in strings.
#'
#' There are options for finding decimal numbers, negative numbers and
#' scientific notation, but the default simply finds consecutive numeric
#' characters (0123456789). See the examples to understand how these options
#' work.
#'
#' @param decimals Search for decimal numbers e.g. 12.5?
#' @param leading_decimals Include leading decimals i.e. .5 is found just the
#'   same as 0.5?
#' @param negs Search for negative numbers?
#' @param sci Make the search aware of scientific notation e.g. 2e3 is the same
#'   as 2000.
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
num_regex <- function(decimals = FALSE, leading_decimals = FALSE,
                      negs = FALSE, sci = FALSE) {
  if (leading_decimals == TRUE && decimals == FALSE) {
    custom_stop(
      "To allow leading decimals, you need to first allow decimals.",
      "To allow decimals, use `decimals = TRUE`."
    )
  }
  dec_pattern <- "[0-9]+(\\.[0-9]+)?" # Numbers with optional decimals
  leading_dec_pattern <- glue::glue("\\.?{dec_pattern}")
  non_dec_pattern <- "[0-9]+"
  pattern <- non_dec_pattern
  if (decimals) {
    pattern <- ifelse(leading_decimals, leading_dec_pattern, dec_pattern)
  }
  if (sci) {
    pattern <- glue::glue("({pattern}[eE][+-]?{non_dec_pattern})|({pattern})")
  }
  if (negs) pattern <- glue::glue("-?({pattern})")
  pattern
}

ambig_num_regex <- function(decimals = FALSE, leading_decimals = FALSE,
                            sci = FALSE) {
  out <- character(1)
  if (!any(decimals, leading_decimals, sci)) return(out)
  if (decimals) {
    out <- ifelse(leading_decimals,
      "\\.\\d+\\.\\d+", "\\d+\\.\\d+\\.\\d+"
    )
  }
  if (sci) {
    sci_bit <- ifelse(decimals, "\\d+\\.?[eE]\\d+\\.?\\d*[eE]\\d+",
      "\\d+[eE]\\d+[eE]\\d+"
    )
    out <- ifelse(decimals, glue::glue("({sci_bit})|({out})"), sci_bit)
  }
  out
}

num_ambigs <- function(string, decimals = FALSE, leading_decimals = FALSE,
                       sci = FALSE) {
  if (!any(c(decimals, sci))) return(FALSE)
  str_detect(string, ambig_num_regex(
    decimals = decimals, leading_decimals = leading_decimals, sci = sci
  ))
}

ambig_warn <- function(string, ambigs) {
  first_offender <- match(T, ambigs) %>%
    list(., string[.])
  first_offender[[2]] %<>% {
    ifelse(str_length(.) > 50, str_c(str_sub(., 1, 17), "..."), .)
  }
  custom_warn(
    "NAs introduced by ambiguity.",
    "
              The first such ambiguity is in string number
              {first_offender[[1]]} which is '{first_offender[[2]]}'.
              "
  )
}

#' Extract numbers from a string.
#'
#' `str_extract_numbers` extracts the numbers (or non-numbers) from a string
#' where decimals are optionally allowed. `str_nth_number` is a convenient
#' wrapper for `str_extract_numbers`, allowing you to choose which number you
#' want. Please run the examples at the bottom of this page to ensure that you
#' understand how these functions work, and their limitations. These functions
#' are vectorized over `string`.
#'
#' If any part of a string contains an ambiguous number (e.g. `1.2.3` would be
#' ambiguous if `decimals = TRUE` (but not otherwise)), the value returned for
#' that string will be `NA`. Note that these functions do not know about
#' scientific notation (e.g. `1e6` for 1000000).
#'
#' \itemize{ \item `str_first_number(...)` is just `str_nth_number(..., n = 1)`.
#' \item `str_last_number(...)` is just `str_nth_number(..., n = -1)`. }
#'
#' @param string A string.
#' @param leave_as_string Do you want to return the number as a string (`TRUE`)
#'   or as numeric (`FALSE`, the default)?
#' @param decimals Do you want to include the possibility of decimal numbers
#'   (`TRUE`) or not (`FALSE`, the default).
#' @param leading_decimals Do you want to allow a leading decimal point to be
#'   the start of a number?
#' @param negs Do you want to allow negative numbers? Note that double negatives
#'   are not handled here (see the examples).
#' @return For `str_extract_numbers` and `str_extract_non_numerics`, a list of
#'   numeric or character vectors, one list element for each element of
#'   `string`. For `str_nth_number` and `nth_non_numeric`, a vector the same
#'   length as `string` (as in `length(string)`, not `nchar(string)`).
#' @examples
#' str_extract_numbers(c("abc123abc456", "abc1.23abc456"))
#' str_extract_numbers(c("abc1.23abc456", "abc1..23abc456"), decimals = TRUE)
#' str_extract_numbers("abc1..23abc456", decimals = TRUE)
#' str_extract_numbers("abc1..23abc456",
#'   decimals = TRUE,
#'   leading_decimals = TRUE
#' )
#' str_extract_numbers("abc1..23abc456",
#'   decimals = TRUE,
#'   leading_decimals = TRUE, leave_as_string = TRUE
#' )
#' str_extract_numbers("-123abc456")
#' str_extract_numbers("-123abc456", negs = TRUE)
#' str_extract_numbers("--123abc456", negs = TRUE) # careful with this one
#' str_extract_numbers(c(rep("abc1.2.3", 2), "a1b2.2.3", "e5r6"),
#'   decimals = TRUE
#' )
#' str_first_number("abc1e5")
#' str_first_number("abc1e5", sci = TRUE)
#' str_extract_numbers("abc1.4e5", sci = TRUE) # careful
#' str_extract_numbers("abc1.4e5", sci = TRUE, decimals = TRUE)
#' str_first_number("abc-1.4e5", sci = TRUE, decimals = TRUE)
#' str_first_number("abc-1.4e5", sci = TRUE, decimals = TRUE, negs = TRUE)
#' str_first_number("ab.1.2", decimals = TRUE, leading_decimals = TRUE)
#' str_nth_number("abc1.23abc456", 2:3)
#' str_nth_number("abc1.23abc456", 2, decimals = TRUE)
#' str_nth_number("-123abc456", -2, negs = TRUE)
#' @export
str_extract_numbers <- function(string, leave_as_string = FALSE,
                                decimals = FALSE, leading_decimals = FALSE,
                                negs = FALSE, sci = FALSE) {
  checkmate::assert_character(string)
  pattern <- num_regex(
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci
  )
  ambig_pattern <- ambig_num_regex(
    decimals = decimals,
    leading_decimals = leading_decimals,
    sci = sci
  )
  ambigs <- num_ambigs(string,
    decimals = decimals,
    leading_decimals = leading_decimals, sci = sci
  )
  out <- vector(mode = "list", length = length(string))
  if (any(ambigs)) {
    ambig_warn(string, ambigs)
    out[ambigs] <- NA_character_
    not_ambigs <- !ambigs
    out[not_ambigs] <- str_extract_all(string[not_ambigs], pattern)
  } else {
    out[] <- str_extract_all(string, pattern)
  }
  if (leave_as_string) return(out)
  lst_char_to_num(out)
}

#' @rdname str_extract_numbers
#' @param n The index of the number (or non-numeric) that you seek. Negative
#'   indexing is allowed i.e. `n = 1` (the default) will give you the first
#'   number (or non-numeric) whereas `n = -1` will give you the last number (or
#'   non-numeric), `n = -2` will give you the second last number and so on. The
#'   function is vectorized over this argument.
#' @export
str_nth_number <- function(string, n, leave_as_string = FALSE, decimals = FALSE,
                           leading_decimals = FALSE, negs = FALSE,
                           sci = FALSE) {
  checkmate::assert_numeric(n)
  checkmate::assert_numeric(abs(n), lower = 1)
  out <- character(length(string))
  if (matrixStats::allValue(n, value = 1) ||
    matrixStats::allValue(n, value = -1)) {
    pattern <- num_regex(
      decimals = decimals, leading_decimals = leading_decimals,
      negs = negs, sci = sci
    )
    ambig_pattern <- ambig_num_regex(
      decimals = decimals,
      leading_decimals = leading_decimals,
      sci = sci
    )
    ambigs <- FALSE
    if (str_length(ambig_pattern)) ambigs <- str_detect(string, ambig_pattern)
    if (any(ambigs)) {
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
      leading_decimals = leading_decimals
    )
    out <- str_list_nth_elems(numbers, n)
  }
  if (!leave_as_string) out %<>% as.numeric()
  out
}

#' @rdname str_extract_numbers
#' @export
str_first_number <- function(string, leave_as_string = FALSE, decimals = FALSE,
                             leading_decimals = FALSE, negs = FALSE,
                             sci = FALSE) {
  str_nth_number(string,
    n = 1, leave_as_string = leave_as_string,
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci
  )
}

#' @rdname str_extract_numbers
#' @export
str_last_number <- function(string, leave_as_string = FALSE, decimals = FALSE,
                            leading_decimals = FALSE, negs = FALSE,
                            sci = FALSE) {
  str_nth_number(string,
    n = -1, leave_as_string = leave_as_string,
    decimals = decimals, leading_decimals = leading_decimals,
    negs = negs, sci = sci
  )
}
