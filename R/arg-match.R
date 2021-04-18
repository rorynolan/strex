#' Argument Matching.
#'
#' Match `arg` against a series of candidate `choices`. `arg` _matches_ an
#' element of `choices` if `arg` is a prefix of that element.
#'
#' `ERROR`s are thrown when a match is not made and where the match is
#' ambiguous. However, sometimes ambiguities are inevitable. Consider the case
#' where `choices = c("ab", "abc")`, then there's no way to choose `"ab"`
#' because `"ab"` is a prefix for `"ab"` and `"abc"`. If this is the case, you
#' need to provide a full match, i.e. using `arg = "ab"` will get you `"ab"`
#' without an error, however `arg = "a"` will throw an ambiguity error.
#'
#' When `choices` is `NULL`, the `choices` are obtained from a default setting
#' for the formal argument `arg` of the function from which `str_match_arg` was
#' called. This is consistent with `base::match.arg()`. See the examples for
#' details.
#'
#' When `arg` and `choices` are identical and `several_ok = FALSE`, the first
#' element of `choices` is returned. This is consistent with
#' `base::match.arg()`.
#'
#' This function inspired by `RSAGA::match.arg.ext()`. Its behaviour is almost
#' identical (the difference is that `RSAGA::match.arg.ext(..., ignore.case =
#' TRUE)` always returns in all lower case; `strex::match_arg(..., ignore_case =
#' TRUE)` ignores case while matching but returns the element of `choices` in
#' its original case). `RSAGA` is a heavy package to depend upon so
#' `strex::match_arg()` is handy for package developers.
#'
#' This function is designed to be used inside of other functions. It's fine to
#' use it for other purposes, but the error messages might be a bit weird.
#'
#' @param arg A character vector (of length one unless `several_ok = TRUE`).
#' @param choices A character vector of candidate values.
#' @param index Return the index of the match rather than the match itself?
#' @param several_ok Allow `arg` to have length greater than one to match
#'   several arguments at once?
#' @param ignore_case Ignore case while matching. If this is `TRUE`, the
#'   returned value is the matched element of `choices` (with its original
#'   casing).
#'
#' @examples
#' choices <- c("Apples", "Pears", "Bananas", "Oranges")
#' match_arg("A", choices)
#' match_arg("B", choices, index = TRUE)
#' match_arg(c("a", "b"), choices, several_ok = TRUE, ignore_case = TRUE)
#' match_arg(c("b", "a"), choices,
#'   ignore_case = TRUE, index = TRUE,
#'   several_ok = TRUE
#' )
#' myword <- function(w = c("abacus", "baseball", "candy")) {
#'   w <- match_arg(w)
#'   w
#' }
#' myword("b")
#' myword()
#' myword <- function(w = c("abacus", "baseball", "candy")) {
#'   w <- match_arg(w, several_ok = TRUE)
#'   w
#' }
#' myword("c")
#' myword()
#' @family argument matchers
#' @export
str_match_arg <- function(arg, choices = NULL, index = FALSE,
                          several_ok = FALSE, ignore_case = FALSE) {
  if (is.null(choices)) {
    arg_sym <- rlang::enexpr(arg)
    null_choice_err <- FALSE
    if (!rlang::is_symbol(arg_sym)) null_choice_err <- TRUE
    if (!null_choice_err) {
      formal_args <- formals(sys.function(sys_p <- sys.parent()))
      arg_sym %<>% as.character()
      default_arg_names <- formal_args %>% {
        names(.)[as.logical(str_length(as.character(.)))]
      }
      if (arg_sym %in% default_arg_names) {
        choices <- eval(formal_args[[arg_sym]], envir = sys.frame(sys_p))
        if (is.character(choices)) {
          return(
            str_match_arg(arg,
              choices = choices,
              index = index,
              several_ok = several_ok,
              ignore_case = ignore_case
            )
          )
        } else {
          null_choice_err <- TRUE
        }
      } else {
        null_choice_err <- TRUE
      }
    }
    if (null_choice_err) {
      custom_stop(
        "You have used `{fun}()` without specifying a `choices` argument. ",
        "
        The only way to do this is from another function where `arg` has a
        default setting. This is the same as `base::match.arg()`.
        ", "
        See the man page for `{fun}()`, particularly the examples:
        enter `help(\"{fun}\", package = \"strex\")` at the R console.
        ", "
        See also the vignette on argument matching:
        enter `vignette(\"argument-matching\", package = \"strex\")`
        at the R console.
        ",
        .envir = list(fun = as.character(match.call())[[1]])
      )
    }
  }
  arg_sym <- tryCatch(as.character(rlang::ensym(arg)),
    error = function(e) NA_character_
  )
  str_match_arg_basic(
    arg = arg,
    choices = choices,
    index = index,
    several_ok = several_ok,
    ignore_case = ignore_case,
    arg_sym = arg_sym
  )
}

#' @rdname str_match_arg
#' @export
match_arg <- str_match_arg

str_match_arg_basic <- function(arg, choices, index, several_ok, ignore_case,
                                arg_sym) {
  checkmate::assert_character(arg, min.len = 1)
  checkmate::assert_character(choices, min.len = 1)
  checkmate::assert_flag(index)
  checkmate::assert_flag(several_ok)
  checkmate::assert_flag(ignore_case)
  checkmate::assert_character(arg, min.len = 1)
  checkmate::assert_string(arg_sym, na.ok = TRUE)
  arg_sym <- ifelse(is.na(arg_sym), "arg", arg_sym)
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
      custom_stop(
        "`choices` must not have duplicate elements. ",
        "Since you have set `ignore_case = TRUE`, elements
         {dupair_indices[1]} and {dupair_indices[2]} of your `choices`
         (\"{dupair[1]}\" and \"{dupair[2]}\") are effectively duplicates.",
        .envir = list(
          dupair = choices[dupair_indices],
          dupair_indices = dupair_indices
        )
      )
    }
  }
  arg_len <- length(arg)
  if (!several_ok && arg_len > 1) {
    if (isTRUE(all.equal(arg, choices))) {
      return(choices[[1]])
    }
    custom_stop(
      "`{arg_sym}` must have length 1.",
      "Your `{arg_sym}` has length {arg_len}.",
      "To use an `{arg_sym}` with length greater than one,
       use `several_ok = TRUE`."
    )
  }
  if (ignore_case) {
    indices <- match_arg_index(str_to_lower(arg), lower_choices)
  } else {
    indices <- match_arg_index(arg, choices)
  }
  bads <- indices <= 0
  if (any(bads)) {
    first_bad_index <- match(TRUE, bads)
    first_bad_type <- indices[first_bad_index]
    stopifnot(first_bad_type %in% (-seq_len(2))) # should never happen
    if (first_bad_type == -1) {
      custom_stop(
        "`{arg_sym}` must be a prefix of exactly one element of `choices`.",
        "Your{ifelse(lch > 50, \" first 50 \", \" \")}`choices` are
         \"{paste(utils::head(choices, 50), collapse = \"\\\", \\\"\")}\".",
        "Your `{arg_sym}` \"{arg[first_bad_index]}\" is not a prefix
         of any of your `choices`.",
        .envir = list(
          lch = length(choices), choices = choices, arg = arg,
          first_bad_index = first_bad_index, arg_sym = arg_sym
        )
      )
    } else {
      if (ignore_case) {
        ambigs <- choices[
          str_starts(
            tolower(choices),
            str_c("^", tolower(arg)[first_bad_index])
          )
        ]
      } else {
        ambigs <- str_subset(choices, str_c("^", arg[first_bad_index]))
      }
      custom_stop(
        "`{arg_sym}` must be a prefix of exactly one element of `choices`.",
        "Your `{arg_sym}` \"{arg[first_bad_index]}\" is a prefix of two or more
         elements of `choices`.",
        "The first two of these are
         \"{ambigs[1]}\" and \"{ambigs[2]}\"."
      )
    }
  }
  if (index) {
    return(indices)
  }
  choices[indices]
}
