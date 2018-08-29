#' Make string numbers comply with alphabetical order
#'
#' If strings are numbered, their numbers may not *comply* with
#' alphabetical order, i.e. "abc2" comes after "abc10" in alphabetical order. We
#' might (for whatever reason) wish to change them such that they come in the
#' order *that we would like*. This function alters the strings such that
#' they comply with alphabetical order, so here "abc2" would be renamed to
#' "abc02". It works on file names with more than one number in them e.g.
#' "abc01def3" (a string with 2 numbers). All the file names that it works on
#' must have the same number of numbers, and the non-number bits must be the
#' same.
#'
#' @param strings A vector of strings.
#' @examples
#' strings <- paste0("abc", 1:12)
#' strings
#' str_alphord_nums(strings)
#' str_alphord_nums(c("abc9def55", "abc10def7"))
#' str_alphord_nums(c("01abc9def55", "5abc10def777", "99abc4def4"))
#' str_alphord_nums(1:10)
#'
#' \dontrun{
#' str_alphord_nums(c("abc9def55", "abc10xyz7"))}
#' @export
str_alphord_nums <- function(strings) {
  checkmate::assert(
    checkmate::check_numeric(strings),
    checkmate::check_character(strings)
  )
  if (is.numeric(strings)) strings %<>% as.character()
  have_nums <- str_detect(strings, "\\d")
  if (!all(have_nums)) {
    bad_index <- match(F, have_nums)
    custom_stop(
      "Some of the input strings have no numbers in them.",
      "
                The first bad string is string number {bad_index},
                which is \"{strings[bad_index]}\".
                "
    )
  }
  non_nums <- str_extract_non_numerics(strings)
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
      The first pair of your `strings` with different non-number bits are
      strings 1 and {bad_index}.
      ", "
      They are \"{strings[1]}\" and \"{strings[bad_index]}\"
      "
    )
  }
  nums <- str_extract_numbers(strings, leave_as_string = TRUE)
  nums_lengths <- lengths(nums)
  if (!all_equal(nums_lengths)) {
    bad_index <- match(F, nums_lengths == nums_lengths[1])
    custom_stop(
      "The `strings` must all have the same number of numbers.",
      "
      Your string number 1 \"{strings[1]}\" has {nums_lengths[1]} numbers,
      whereas your string number {bad_index} \"{strings[bad_index]}\" has
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
  num_first <- str_elem(strings, 1) %>% str_can_be_numeric()
  if (!all_equal(num_first)) {
    bad_index <- match(!num_first[1], num_first)
    custom_stop(
      "Either all strings or none should start with numbers.",
      "
      String number 1 \"{strings[1]}\"
      {ifelse(num_first[1], 'does', 'does not')} start with a number
      whereas string number {bad_index} \"{strings[bad_index]}\"
      {ifelse(num_first[1], 'does not', 'does')}, start with a number.
      "
    )
  }
  if (num_first[1]) {
    interleaves <- interleave_char_lists(nums, non_nums)
  } else {
    interleaves <- interleave_char_lists(non_nums, nums)
  }
  paste_collapse_list_elems(interleaves)
}
