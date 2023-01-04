#' Convert a list of character vectors to a list of numeric vectors.
#'
#' This is the same as doing `lapply(x, as.numeric)` but faster, and it allows
#' for comma handling.
#'
#' @param x A list of character vectors.
#' @param commas Allow for comma separation?
#'
#' @return A list of numeric vectors.
#'
#' @examples
#' lst_chr_to_dbl(list(c("1", "2,000"), c("1.3", "2.2", "5.9")), commas = TRUE)
#' @noRd
lst_chr_to_dbl <- function(x, commas = FALSE) {
  checkmate::assert_list(x, types = "character")
  checkmate::assert_logical(commas, min.len = 1)
  assert_compatible_lengths(x, commas)
  .Call(C_lst_chr_to_dbl, x, commas)
}

#' Get the indices of the `choices` that are matches for `arg`.
#'
#' @inheritParams match_arg
#'
#' @return A numeric vector.
#'
#' @examples
#' match_arg_index("ab", c("book", "abacus", "pencil"))
#' @noRd
match_arg_index <- function(arg, choices) {
  checkmate::assert_character(arg, min.chars = 1, min.len = 1)
  checkmate::assert_character(choices, min.chars = 1, min.len = 1)
  .Call(C_match_arg_index, arg, choices)
}

#' Make a vector where every other element is from `x` or `y`.
#'
#' The lengths of `x` and `y` must differ by at most 1.
#' If `x` and `y` have the same length, the first element of `x` will be first
#' in the result.
#'
#' @param x,y Character vectors.
#'
#' @return A character vector.
#'
#' @examples
#' interleave_chr_vecs(c("a", "b"), c("x", "y"))
#' interleave_chr_vecs(c("a", "b", "c"), c("x", "y"))
#' interleave_chr_vecs(c("a", "b"), c("x", "y", "z"))
#' @noRd
interleave_chr_vecs <- function(x, y) {
  checkmate::assert_character(x)
  checkmate::assert_character(y)
  if (abs(length(x) - length(y)) > 1) {
    custom_stop(
      "`x` and `y` must have lengths that differ by at most 1.",
      "`x` has length {length(x)}.",
      "`y` has length {length(y)}."
    )
  }
  .Call(C_interleave_chr_vecs, x, y)
}

#' List version of [interleave_chr_vecs].
#'
#' This is a C version of `map2(x, y, ~interleave_chr_vecs(.x, .y))`.
#'
#' @param x,y Lists of character vectors. `x` and `y` must be of equal length.
#'
#' @return A list.
#'
#' @examples
#' interleave_chr_lsts(
#'   list(c("a", "b"), c("a", "b", "c"), c("a", "b")),
#'   list(c("x", "y"), c("x", "y"), c("x", "y", "z"))
#' )
#' @noRd
interleave_chr_lsts <- function(x, y) {
  checkmate::assert_list(x, types = "character")
  checkmate::assert_list(y, types = "character")
  .Call(C_interleave_chr_lsts, x, y)
}

#' Remove empty strings from a character vector.
#'
#' Empty strings are length zero strings i.e. `""`.
#'
#' @param chr_vec A character vector.
#'
#' @return A character vector.
#'
#' @examples
#' chr_vec_remove_empties(c("", "a", "", "", "b", "c", ""))
#' @noRd
chr_vec_remove_empties <- function(chr_vec) {
  checkmate::assert_character(chr_vec)
  .Call(C_chr_vec_remove_empties, chr_vec)
}

#' Remove empty strings from a character vector.
#'
#' Empty strings are length zero strings i.e. `""`.
#'
#' @param chr_lst A list of character vectors.
#'
#' @return A list of character vectors.
#'
#' @examples
#' chr_lst_remove_empties(
#'   list(
#'     c("", "a", "", "", "b", "c", ""),
#'     c("", ""),
#'     c("xy")
#'   )
#' )
#' @noRd
chr_lst_remove_empties <- function(chr_lst) {
  checkmate::assert_list(chr_lst, types = "character")
  .Call(C_chr_lst_remove_empties, chr_lst)
}

#' Get the `n`th element of each of each element of a list of character vectors.
#'
#' @param chr_lst A list of character vectors.
#' @param n An integer vector.
#'
#' @return A character vector.
#'
#' @examples
#' chr_lst_nth_elems(list(c("a", "b"), c("x", "y", "z")), 2:3)
#' @noRd
chr_lst_nth_elems <- function(chr_lst, n) {
  checkmate::assert_list(chr_lst, types = "character")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(chr_lst, n)
  .Call(C_chr_lst_nth_elems, chr_lst, as.integer(n))
}

#' Get the `n`th element of each of each element of a list of real vectors.
#'
#' @param dbl_lst A list of real vectors.
#' @param n An integer vector.
#'
#' @return A character vector.
#'
#' @examples
#' dbl_lst_nth_elems(list(c(1.2, 2.3, 3.4), c(6.7, 8.9)), 2)
#' @noRd
dbl_lst_nth_elems <- function(dbl_lst, n) {
  checkmate::assert_list(dbl_lst, types = "double")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(dbl_lst, n)
  .Call(C_dbl_lst_nth_elems, dbl_lst, as.integer(n))
}

#' Get the `n`th column of each integer matrix in a list.
#'
#' @param int_mat_lst A list of integer matrices.
#' @param n An integer vector.
#'
#' @return A list.
#'
#' @examples
#' int_mat_lst_nth_cols(
#'   list(
#'     matrix(1:4, nrow = 2),
#'     matrix(9:1, nrow = 3)
#'   ),
#'   c(2, 3)
#' )
#' @noRd
int_mat_lst_nth_cols <- function(int_mat_lst, n) {
  checkmate::assert_list(int_mat_lst, "integer")
  checkmate::assert_list(int_mat_lst, "matrix")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(int_mat_lst, n)
  .Call(C_int_mat_lst_nth_cols, int_mat_lst, as.integer(n))
}

#' Get the `n`th row of each integer matrix in a list.
#'
#' @param int_mat_lst A list of integer matrices.
#' @param n An integer vector.
#'
#' @return A list.
#'
#' @examples
#' int_mat_lst_nth_rows(
#'   list(
#'     matrix(1:4, nrow = 2),
#'     matrix(9:1, nrow = 3)
#'   ),
#'   c(2, 3)
#' )
#' @noRd
int_mat_lst_nth_rows <- function(int_mat_lst, n) {
  checkmate::assert_list(int_mat_lst, "integer")
  checkmate::assert_list(int_mat_lst, "matrix")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(int_mat_lst, n)
  .Call(C_int_mat_lst_nth_rows, int_mat_lst, as.integer(n))
}

#' [cbind()] the elements of a list of integer vectors.
#'
#' @param int_lst A list of equal-length integer vectors.
#'
#' @return An integer matrix.
#'
#' @examples
#' int_lst_cbind(list(1:4, 6:9))
#' @noRd
int_lst_cbind <- function(int_lst) {
  checkmate::assert_list(int_lst, types = "integer")
  assert_lst_elems_common_length(int_lst)
  .Call(C_int_lst_cbind, int_lst)
}

#' [rbind()] the elements of a list of integer vectors.
#'
#' @param int_lst A list of equal-length integer vectors.
#'
#' @return An integer matrix.
#'
#' @examples
#' int_lst_rbind(list(1:4, 6:9))
#' @noRd
int_lst_rbind <- function(int_lst) {
  checkmate::assert_list(int_lst, types = "integer")
  assert_lst_elems_common_length(int_lst)
  .Call(C_int_lst_rbind, int_lst)
}

#' [cbind()] the `n`th row of each integer matrix in a list.
#'
#' @param int_mat_lst A list of integer matrices.
#' @param n An integer vector.
#'
#' @return A list.
#'
#' @examples
#' int_mat_lst_cbind_nth_rows(
#'   list(
#'     matrix(1:4, nrow = 2),
#'     matrix(6:9, nrow = 2)
#'   ),
#'   c(1, 2)
#' )
#' @noRd
int_mat_lst_cbind_nth_rows <- function(int_mat_lst, n) {
  checkmate::assert_list(int_mat_lst, "integer")
  checkmate::assert_list(int_mat_lst, "matrix")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(int_mat_lst, n)
  .Call(C_int_mat_lst_cbind_nth_rows, int_mat_lst, as.integer(n))
}

#' [cbind()] the `n`th column of each integer matrix in a list.
#'
#' @param int_mat_lst A list of integer matrices.
#' @param n An integer vector.
#'
#' @return A list.
#'
#' @examples
#' int_mat_lst_cbind_nth_cols(
#'   list(
#'     matrix(1:4, nrow = 2),
#'     matrix(6:9, nrow = 2)
#'   ),
#'   c(1, 2)
#' )
#' @noRd
int_mat_lst_cbind_nth_cols <- function(int_mat_lst, n) {
  checkmate::assert_list(int_mat_lst, "integer")
  checkmate::assert_list(int_mat_lst, "matrix")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(int_mat_lst, n)
  .Call(C_int_mat_lst_cbind_nth_cols, int_mat_lst, as.integer(n))
}

#' [rbind()] the `n`th column of each integer matrix in a list.
#'
#' @param int_mat_lst A list of integer matrices.
#' @param n An integer vector.
#'
#' @return A list.
#'
#' @examples
#' int_mat_lst_rbind_nth_cols(
#'   list(
#'     matrix(1:4, nrow = 2),
#'     matrix(6:9, nrow = 2)
#'   ),
#'   c(1, 2)
#' )
#' @noRd
int_mat_lst_rbind_nth_cols <- function(int_mat_lst, n) {
  checkmate::assert_list(int_mat_lst, "integer")
  checkmate::assert_list(int_mat_lst, "matrix")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(int_mat_lst, n)
  .Call(C_int_mat_lst_rbind_nth_cols, int_mat_lst, as.integer(n))
}

#' [rbind()] the `n`th row of each integer matrix in a list.
#'
#' @param int_mat_lst A list of integer matrices.
#' @param n An integer vector.
#'
#' @return A list.
#'
#' @examples
#' int_mat_lst_rbind_nth_rows(
#'   list(
#'     matrix(1:4, nrow = 2),
#'     matrix(6:9, nrow = 2)
#'   ),
#'   c(1, 2)
#' )
#' @noRd
int_mat_lst_rbind_nth_rows <- function(int_mat_lst, n) {
  checkmate::assert_list(int_mat_lst, "integer")
  checkmate::assert_list(int_mat_lst, "matrix")
  checkmate::assert_integerish(n)
  assert_compatible_lengths(int_mat_lst, n)
  .Call(C_int_mat_lst_rbind_nth_rows, int_mat_lst, as.integer(n))
}

#' Fullocate each element of a list.
#'
#' Fullocation is filling in the output of a call to [stringr::str_locate()].The
#' result is a set of closed intervals whose union spans the whole interval.
#' Looking at the examples is the best way to understand this function.
#'
#' This function does not handle bad input well. The first argument must be the
#' output of a call to [stringr::str_locate_all()].
#'
#' @param int_mat_lst A list of integer matrices. The return of a call to
#'   [stringr::str_locate_all()].
#' @param start,end Integer vectors of length 1 or the same length as
#'   `int_mat_lst`. Start and end points for the fullocation interval.
#'
#' @return A list of integer matrices.
#'
#' @examples
#' int_mat_lst <- list(
#'   rbind(c(2L, 5L), 7:8),
#'   rbind(c(5L, 6L), c(7L, 10L), c(20L, 30L))
#' )
#' lst_fullocate(int_mat_lst, start = c(1, 5), end = c(10, 50))
#' @noRd
lst_fullocate <- function(int_mat_lst, start, end) {
  checkmate::assert_list(int_mat_lst, types = "integer")
  checkmate::assert_list(int_mat_lst, types = "matrix")
  start <- checkmate::assert_integerish(start, coerce = TRUE)
  end <- checkmate::assert_integerish(end, coerce = TRUE)
  assert_compatible_lengths(int_mat_lst, start)
  assert_compatible_lengths(int_mat_lst, end)
  .Call(C_lst_fullocate, int_mat_lst, start, end)
}

#' Do all elements of a list have the same length?
#'
#' @param lst A list.
#' @param l A double. The length of the list.
#'
#' @return A flag.
#'
#' @noRd
lst_elems_common_length <- function(lst, l = as.double(length(lst))) {
  checkmate::assert_list(lst)
  checkmate::assert_number(l)
  .Call(C_lst_elems_common_length, lst, as.double(l))
}

#' [rbind()] the elements of a pairlist of integer vectors.
#'
#' @param prlst A pairlist.
#'
#' @return An integer matrix.
#'
#' @examples
#' int_prlst_rbind(pairlist(1:2, 8:9, 5:4))
#' @noRd
int_prlst_rbind <- function(prlst) {
  checkmate::assert_class(prlst, "pairlist")
  .Call(C_SXP_int_prlst_rbind, prlst, length(prlst))
}

#' [cbind()] the elements of a pairlist of integer vectors.
#'
#' @param prlst A pairlist.
#'
#' @return An integer matrix.
#'
#' @examples
#' int_prlst_rbind(pairlist(1:2, 8:9, 5:4))
#' @noRd
int_prlst_cbind <- function(prlst) {
  checkmate::assert_class(prlst, "pairlist")
  .Call(C_SXP_int_prlst_cbind, prlst, length(prlst))
}

#' Find the maxs of each row of an integer matrix.
#'
#' Faster version of `apply(int_mat, 1, max)`.
#'
#' @param int_mat An integer matrix.
#'
#' @return An integer vector.
#'
#' @noRd
int_mat_row_maxs <- function(int_mat) {
  checkmate::assert_matrix(int_mat, mode = "integer")
  .Call(C_int_mat_row_maxs, int_mat)
}

#' Check if all elements of a vector are equal to a specified value value.
#'
#' @param int_vec An integer vector.
#' @param int_scalar An integer. The value.
#'
#' @return A flag.
#'
#' @noRd
int_vec_all_value <- function(int_vec, int_scalar) {
  int_vec <- checkmate::assert_integerish(int_vec, coerce = TRUE)
  int_scalar <- checkmate::assert_int(int_scalar, coerce = TRUE)
  .Call(C_int_vec_all_value, int_vec, int_scalar)
}

#' C version of `purrr::map(pattern, ~stringi::stri_detect_coll(string, .)`.
#'
#' @inheritParams stringi::stri_detect_coll
#'
#' @return A list of logical vectors. The list is the same length as `pattern`.
#'   The vectors are the same length as `string`.
#'
#' @noRd
str_detect_many_coll <- function(string, pattern) {
  checkmate::assert_character(string, min.chars = 1, min.len = 1)
  checkmate::assert_character(pattern, min.chars = 1, min.len = 1)
  .Call(C_str_detect_many_coll, string, pattern)
}

#' C version of `purrr::map(pattern, ~stringi::stri_detect_fixed(string, .)`.
#'
#' @inheritParams stringi::stri_detect_coll
#'
#' @return A list of logical vectors. The list is the same length as `pattern`.
#'   The vectors are the same length as `string`.
#'
#' @noRd
str_detect_many_fixed <- function(string, pattern) {
  checkmate::assert_character(string, min.chars = 1, min.len = 1)
  checkmate::assert_character(pattern, min.chars = 1, min.len = 1)
  .Call(C_str_detect_many_fixed, string, pattern)
}
