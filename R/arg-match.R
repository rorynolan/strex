#' Argument Matching.
#'
#' Match `arg` against a series of candidate `choices` where `NULL` means take
#' the first one. `arg` _matches_ an element of `choices` if `arg` is a prefix
#' of that element.
#'
#' `ERROR`s are thrown when a match is not made and where the match is
#' ambiguous. However, sometimes ambiguities are inevitable. Consider the case
#' where `choices = c("ab", "abc")`, then there's no way to choose `"ab"`
#' because `"ab"` is a prefix for `"ab"` and `"abc"`. If this is the case, you
#' need to provide a full match, i.e. using `arg = "ab"` will get you `"ab"`
#' without an error, however `arg = "a"` will throw an ambiguity error.
#'
#' This function inspired by `RSAGA::match.arg.ext()`. Its behaviour is almost
#' identical (the difference is that `RSAGA::match.arg.ext(..., ignore.case =
#' TRUE)` guarantees that the function returns strings in all lower case, but
#' that is not so with `filesstrings::match_arg(..., ignore_case = TRUE)`) but
#' `RSAGA` is a heavy package to depend upon so `filesstrings::match_arg()`
#' might be handy for package developers.
#'
#' This function is designed to be used inside of other functions. It's fine to
#' use it for other purposes, but the error messages might be a bit weird.
#'
#' @param arg A character vector (of length one unless `several_ok = TRUE`).
#' @param choices A character vector of candidate values.
#' @param index Return the index of the match rather than the match itself?
#'   Default no.
#' @param several_ok Allow `arg` to have length greater than one to match
#'   several arguments at once? Default no.
#' @param ignore_case Ignore case while matching. Default no. If this is `TRUE`,
#'   the returned value is the matched element of `choices` (with its original
#'   casing).
#'
#' @examples
#' choices <- c("Apples", "Pears", "Bananas", "Oranges")
#' match_arg(NULL, choices)
#' match_arg("A", choices)
#' match_arg("B", choices, index = TRUE)
#' match_arg(c("a", "b"), choices, several_ok = TRUE, ignore_case = TRUE)
#' match_arg(c("b", "a"), choices, ignore_case = TRUE, index = TRUE,
#'           several_ok = TRUE)
#'
#' @export
str_match_arg <- function(arg, choices, index = FALSE, several_ok = FALSE,
                          ignore_case = FALSE) {
  checkmate::assert_character(choices, min.len = 1)
  checkmate::assert_flag(index)
  checkmate::assert_flag(several_ok)
  checkmate::assert_flag(ignore_case)
  if (is.null(arg)) return(choices[1])
  checkmate::assert_character(arg, min.len = 1)
  first_dup <- anyDuplicated(choices)
  if (first_dup) {
    custom_stop(
      "`choices` must not have duplicate elements. ",
      "Element {first_dup}, of your `choices` (\"{choices[first_dup]}\")
       is a duplicate."
    )
  }
  if (ignore_case) {
    lower_choices <- str_to_lower(choices)
    first_dup <- anyDuplicated(lower_choices)
    if (first_dup) {
      dupair_indices <- c(
        match(lower_choices[first_dup], lower_choices),
        first_dup
      )
      dupair <- choices[dupair_indices]
      custom_stop(
        "`choices` must not have duplicate elements. ",
        "Since you have set `ignore_case = TRUE`, elements
         {dupair_indices[1]} and {dupair_indices[2]} of your `choices`
         (\"{dupair[1]}\" and \"{dupair[2]}\") are effectively duplicates."
      )
    }
  }
  arg_len <- length(arg)
  if ((!several_ok) && arg_len > 1) {
    custom_stop(
      "`arg` must have length 1.",
      "Your `arg` has length {arg_len}.",
      "To use an `arg` with length greater than one,
       use `several_ok = TRUE`."
    )
  }
  if (ignore_case) {
    indices <- match_arg_index(str_to_lower(arg), lower_choices)
  } else {
    indices <- match_arg_index(arg, choices)
  }
  bads <- indices < 0
  if (any(bads)) {
    first_bad_index <- match(T, bads)
    first_bad_type <- indices[first_bad_index]
    stopifnot(first_bad_type %in% -(1:2)) # should never happen
    if (first_bad_type == -1) {
      lch <- length(choices)
      if (lch > 50) {
        choices %<>% {
          .[1:50]
        }
      }
      custom_stop(
        "`arg` must be a prefix of exactly one element of `choices`.",
        "Your{ifelse(lch > 50, \" first 50 \", \" \")}`choices` are
         \"{glue::glue_collapse(choices, sep = \"\\\", \\\"\",
         last = \"\\\" and \\\"\")}\".",
        "Your `arg` \"{arg[first_bad_index]}\" is not a prefix
         of any of your `choices`."
      )
    } else {
      if (ignore_case) {
        two_ambigs <- str_detect(
          tolower(choices),
          str_c("^", tolower(arg)[first_bad_index])
        )
      } else {
        two_ambigs <- str_detect(choices, str_c("^", arg[first_bad_index]))
      }
      two_ambigs %<>% {
        choices[.]
      } %>% {
        .[1:2]
      }
      custom_stop(
        "`arg` must be a prefix of exactly one element of `choices`.",
        "Your `arg` \"{arg[first_bad_index]}\" is a prefix of two or more
         elements of `choices`.",
        "The first two of these are
         \"{two_ambigs[1]}\" and \"{two_ambigs[2]}\"."
      )
    }
  }
  indices %<>% {
    . + 1
  }
  if (index) return(indices)
  choices[indices]
}

#' @rdname str_match_arg
#' @export
match_arg <- str_match_arg
