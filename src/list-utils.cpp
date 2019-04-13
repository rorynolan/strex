#include <Rcpp.h>
using namespace Rcpp;


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
  List no_empties = clone(char_list);
  for (int i = 0; i < char_list.length(); i++) {
    CharacterVector strings = as<CharacterVector>(char_list[i]);
    int j = 0;
    while (j < strings.size()) {
      if (strings[j] == "")
        strings.erase(j);
      else
        j++;
    }
    no_empties[i] = strings;
  }
  return(no_empties);
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
//' str_list_nth_elems_(list(c("a", "b", "c"), c("d", "f", "a")), 2)
//' num_list_nth_elems_(list(1:5, 0:2), 4)
//'
//' @noRd
// [[Rcpp::export]]
CharacterVector str_list_nth_elems_(List char_list, IntegerVector n) {
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
IntegerVector intmat_list_bind_nth_rows(List intmat_list, IntegerVector n) {
  IntegerMatrix mat1 = as<IntegerMatrix>(intmat_list[0]);
  std::size_t nc = mat1.ncol();
  std::size_t intmat_list_size = intmat_list.size();
  IntegerMatrix out(intmat_list_size, nc);
  std::copy(mat1.row(n[0]).begin(), mat1.row(n[0]).end(), out.row(0).begin());
  if (intmat_list_size > 1) {
    for (std::size_t i = 1; i != intmat_list_size; ++i) {
      IntegerMatrix mat_i = as<IntegerMatrix>(intmat_list[i]);
      std::copy(mat_i.row(n[i]).begin(), mat_i.row(n[i]).end(),
                out.row(i).begin());
    }
  }
  return out;
}

NumericVector char_to_num(CharacterVector x) {
  std::size_t n = x.size();
  if (n == 0) return NumericVector(0);
  NumericVector out(n);
  for (std::size_t i = 0; i != n; ++i) {
    std::string x_i(x[i]);
    double number = NA_REAL;
    try {
      std::size_t pos;
      number = std::stod(x_i, &pos);
      number = ((pos == x_i.size()) ? number : NA_REAL);
    } catch (const std::invalid_argument& e) {
      ;  // do nothing
    }
    out[i] = number;
  }
  return out;
}

// [[Rcpp::export]]
List lst_char_to_num(List x) {
  std::size_t n = x.size();
  List out(n);
  for (std::size_t i = 0; i != n; ++i)
    out[i] = char_to_num(x[i]);
  return out;
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
