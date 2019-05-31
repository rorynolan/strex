#' Make string numbers comply with alphabetical order.
#'
#' If strings are numbered, their numbers may not *comply* with alphabetical
#' order, e.g. "abc2" comes after `"abc10"` in alphabetical order. We might (for
#' whatever reason) wish to change them such that they come in the order *that
#' we would like*. This function alters the strings such that they comply with
#' alphabetical order, so here `"abc2"` would be renamed to "abc02". It works on
#' file names with more than one number in them e.g. `"abc01def3"` (a string
#' with 2 numbers). All the strings in the character vector `string` must have
#' the same number of numbers, and the non-number bits must be the same.
#'
#' @inheritParams str_after_nth
#'
#' @return A character vector.
#'
#' @examples
#' string <- paste0("abc", 1:12)
#' print(string)
#' str_alphord_nums(string)
#' str_alphord_nums(c("abc9def55", "abc10def7"))
#' str_alphord_nums(c("01abc9def55", "5abc10def777", "99abc4def4"))
#' str_alphord_nums(1:10)
#' \dontrun{
#' str_alphord_nums(c("abc9def55", "abc10xyz7")) # error
#' }
#'
#' @family alphorderers
#' @export
str_alphord_nums <- function(string) {
  if (all_equal(string, character())) {
    return(character())
  }
  checkmate::assert(
    checkmate::check_numeric(string, min.len = 1),
    checkmate::check_character(string, min.len = 1)
  )
  if (is.numeric(string)) string %<>% as.character()
  have_nums <- str_detect(string, "\\d")
  if (!all(have_nums)) {
    bad_index <- match(F, have_nums)
    custom_stop(
      "Some of the input strings have no numbers in them.",
      "
                The first bad string is string number {bad_index},
                which is \"{string[bad_index]}\".
                "
    )
  }
  non_nums <- str_extract_non_numerics(string)
  if (!all_equal(non_nums)) {
    bad_index <- 2
    while (all_equal(non_nums[[1]], non_nums[[bad_index]])) {
      bad_index %<>% {
        . + 1
      }
    }
    custom_stop(
      "The non-number bits of every string must be the same.",
      "
      The first pair of strings with different non-number bits are
      strings 1 and {bad_index}.
      ", "
      They are \"{string[1]}\" and \"{string[bad_index]}\"
      "
    )
  }
  nums <- str_extract_numbers(string, leave_as_string = TRUE)
  nums_lengths <- lengths(nums)
  if (!all_equal(nums_lengths)) {
    bad_index <- match(F, nums_lengths == nums_lengths[1])
    custom_stop(
      "The strings must all have the same number of numbers.",
      "
      Your string number 1 \"{string[1]}\" has {nums_lengths[1]} numbers,
      whereas your string number {bad_index} \"{string[bad_index]}\" has
      {nums_lengths[bad_index]} numbers.
      "
    )
  }
  nums %<>% simplify2array()
  if (!is.matrix(nums)) nums %<>% t()
  ncn <- nums %>% {
    array(str_length(.), dim = dim(.))
  }
  max_lengths <- matrixStats::rowMaxs(ncn)
  min_length <- min(ncn)
  to_prefix <- rep("0", max(max_lengths) - min_length) %>% str_c(collapse = "")
  nums %<>% str_c(to_prefix, .)
  starts <- -rep(max_lengths, ncol(ncn))
  nums %<>% str_sub(starts, -1) %>%
    split(rep(seq_len(ncol(ncn)), each = nrow(ncn)))
  num_first <- str_elem(string, 1) %>% str_can_be_numeric()
  if (!all_equal(num_first)) {
    bad_index <- match(!num_first[1], num_first)
    custom_stop(
      "
      It should either be the case that all strings start with numbers or
      that none of them do.
      ",
      "
      String number 1 \"{string[1]}\"
      {ifelse(num_first[1], 'does', 'does not')} start with a number
      whereas string number {bad_index} \"{string[bad_index]}\"
      {ifelse(num_first[1], 'does not', 'does')}, start with a number.
      "
    )
  }
  if (num_first[1]) {
    interleaves <- interleave_char_lists(nums, non_nums)
  } else {
    interleaves <- interleave_char_lists(non_nums, nums)
  }
  stringi::stri_paste_list(interleaves)
}
