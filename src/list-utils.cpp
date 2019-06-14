#include <Rcpp.h>
using namespace Rcpp;

#include <string>
#include <exception>

#include "stod.h"


// [[Rcpp::export]]
List lst_char_to_num(List x, bool commas) {
  std::size_t n = x.size();
  List out(n);
  for (std::size_t i = 0; i != n; ++i)
    out[i] = char_to_num(x[i], commas);
  return out;
}

//' Remove empty strings from a character list.
//'
//' @param char_list A list of character vectors.
//'
//' @return A list of character vectors.
//'
//' @examples
//' str_list_remove_empties(list(c("a", "", "b"), "gg", c("", 1, "")))
//'
//' @noRd
// [[Rcpp::export]]
List str_list_remove_empties(List char_list) {
  R_xlen_t n = char_list.length();
  List out(n);
  for (R_xlen_t i = 0; i != n; ++i) {
    CharacterVector strings = char_list[i];
    R_xlen_t sz = strings.length();
    LogicalVector non_empty(sz);
    for (R_xlen_t j = 0; j != sz; ++j)
      non_empty[j] = strings[j].size();
    out[i] = strings[non_empty];
  }
  return(out);
}

//' Get the nth element of each vector in a list of numeric or character
//' vectors.
//'
//' These are faster implementations of procedures that could very easily be
//' done with [purrr::map_dbl] or [purrr::map_chr].
//'
//' This is a wrapper function for [str_list_nth_elems_()] that
//' has better error handling.
//'
//' @param char_list A list of character vectors.
//' @param n The index of the element that you want from each vector. If
//'   `char_list` is of length 1, this can be any length and those indices will
//'   be extracted from `char_list[[1]]`. Otherwise, this must either be of
//'   length 1 or the same length as `char_list`. All of this is to say that
//'   the function is vectorised over this argument.
//'
//' @return A list.
//'
//' @examples
//' str_list_nth_elems_helper(list(c("a", "b", "c"), c("d", "f", "a")), 2)
//' num_list_nth_elems_helper(list(1:5, 0:2), 4)
//'
//' @noRd
// [[Rcpp::export]]
CharacterVector str_list_nth_elems_helper(List char_list, IntegerVector n) {
  std::size_t cl_sz = char_list.size(), n_sz = n.size();
  CharacterVector nths;
  if (cl_sz == 1) {
    nths = CharacterVector(n_sz);
    CharacterVector strings = as<CharacterVector>(char_list[0]);
    for (std::size_t i = 0; i != n_sz; ++i) {
      int n_i = n[i];
      if (n_i < 0)
        n_i += strings.size() + 1;
      nths[i] = (((n_i > strings.size()) | (n_i <= 0)) ?
                   NA_STRING :
                   strings[n_i - 1]);
    }
  } else {
    nths = CharacterVector(cl_sz);
    if (n.size() == 1) {
      for (std::size_t i = 0; i < cl_sz; i++) {
        CharacterVector strings = as<CharacterVector>(char_list[i]);
        int n_i = n[0];
        if (n_i < 0)
          n_i += strings.size() + 1;
        nths[i] = (((n_i > strings.size()) | (n_i <= 0)) ?
                     NA_STRING :
                     strings[n_i - 1]);
      }
    } else {
      for (std::size_t i = 0; i < cl_sz; i++) {
        CharacterVector strings = as<CharacterVector>(char_list[i]);
        int n_i = n[i];
        if (n_i < 0)
          n_i += strings.size() + 1;
        nths[i] = (((n_i > strings.size()) | (n_i <= 0)) ?
                     NA_STRING :
                     strings[n_i - 1]);
      }
    }
  }
  return(nths);
}

//' @rdname str_list_nth_elems_
//' @param num_list A list of numeric vectors.
//' @noRd
// [[Rcpp::export]]
NumericVector num_list_nth_elems_(List num_list, IntegerVector n) {
  std::size_t nls = num_list.size(), n_sz = n.size();
  NumericVector nths;
  if (nls == 1) {
    nths = NumericVector(n_sz);
    NumericVector strings = as<NumericVector>(num_list[0]);
    for (std::size_t i = 0; i != n_sz; ++i) {
      int n_i = n[i];
      if (n_i < 0)
        n_i += strings.size() + 1;
      nths[i] = (((n_i > strings.size()) | (n_i <= 0)) ?
                   NA_REAL :
                   strings[n_i - 1]);
    }
  } else {
    nths = NumericVector(nls);
    if (n.size() == 1) {
      for (std::size_t i = 0; i < nls; i++) {
        NumericVector nums = as<NumericVector>(num_list[i]);
        int n_i = n[0];
        if (n_i < 0)
          n_i += nums.size() + 1;
        nths[i] = (((n_i > nums.size()) | (n_i <= 0)) ?
                     NA_REAL :
                     nums[n_i - 1]);
      }
    } else {
      for (std::size_t i = 0; i < nls; i++) {
        NumericVector nums = as<NumericVector>(num_list[i]);
        int n_i = n[i];
        if (n_i < 0)
          n_i += nums.size() + 1;
        nths[i] = (((n_i > nums.size()) | (n_i <= 0)) ?
                     NA_REAL :
                     nums[n_i - 1]);
      }
    }
  }
  return(nths);
}

// [[Rcpp::export]]
List int_lst_first_col(List x) {
  std::size_t n = x.size();
  List out(n);
  for (std::size_t i = 0; i != n; ++i) {
    IntegerMatrix x_i = as<IntegerMatrix>(x[i]);
    out[i] = x_i.column(0);
  }
  return out;
}

//' rbind all elements of a list.
//'
//' Assumes all elements of list are integer matrices with the same number of
//' columns. Undefined behaviour if not.
//'
//' @param x A list of integer matrices all with the same number of rows.
//' @param x_lens The lengths of the elements of x.
//'
//' @return An integer matrix.
//'
//' @noRd
//'
// [[Rcpp::export]]
IntegerMatrix lst_rbind(List x, NumericVector x_lens) {
  const R_xlen_t out_len = sum(x_lens);
  IntegerMatrix x0 = x[0];
  R_xlen_t nc = x0.ncol();
  const R_xlen_t nr = out_len / nc;
  R_xlen_t r = 0;
  IntegerMatrix out(nr, nc);
  for (R_xlen_t i = 0, x_len = x.length(); i != x_len; ++i) {
    IntegerMatrix intmat_i = x[i];
    for (R_xlen_t r_i = 0, nr_i = intmat_i.nrow(); r_i != nr_i; ++r_i) {
      for (R_xlen_t c_i = 0; c_i != nc; ++c_i)
        out(r, c_i) = intmat_i(r_i, c_i);
      ++r;
    }
  }
  return out;
}

//' rbind the nth rows of all elements of a list.
//'
//' Assumes all elements of list are integer matrices with the same number of
//' columns. Undefined behaviour if not.
//'
//' @param x A list of integer matrices all with the same number of rows.
//' @param x_lens The lengths of the elements of x.
//'
//' @return An integer matrix.
//'
//' @noRd
//'
// [[Rcpp::export]]
IntegerMatrix lst_rbind_nth_rows(List x, NumericVector n) {
  const R_xlen_t x_len = x.length(), n_len = n.length();
  IntegerMatrix x0 = x[0];
  R_xlen_t nc = x0.ncol(), nr = (x_len == 1) ? n_len: x_len;
  IntegerMatrix out(nr, nc);
  if (x_len == 1) {
    IntegerMatrix intmat = x[0];
    for (R_xlen_t i = 0; i != n_len; ++i) {
      std::copy(intmat.row(n[i] - 1).cbegin(),
                intmat.row(n[i] - 1).cend(),
                out.row(i).begin());
    }
  } else if (n_len > 1) {
    for (R_xlen_t i = 0; i != x_len; ++i) {
      IntegerMatrix intmat_i = x[i];
      std::copy(intmat_i.row(n[i] - 1).cbegin(),
                intmat_i.row(n[i] - 1).cend(),
                out.row(i).begin());
    }
  } else {
    for (R_xlen_t i = 0; i != x_len; ++i) {
      IntegerMatrix intmat_i = x[i];
      std::copy(intmat_i.row(n[0] - 1).cbegin(),
                intmat_i.row(n[0] - 1).cend(),
                out.row(i).begin());
    }
  }
  return out;
}
