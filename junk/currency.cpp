#include <Rcpp.h>
using namespace Rcpp;

#include "stod.h"


//' Get the character before a located pattern.
//'
//' @param s A string.
//' @param locs The output of `stringr::str_locate()`.
//'
//' @return A character vector.
//'
//' @noRd
CharacterVector get_prev_chars(std::string s, IntegerMatrix locs) {
  R_xlen_t n_locs = locs.nrow();
  CharacterVector out(n_locs);
  for (R_xlen_t i = 0; i != n_locs; ++i) {
    R_xlen_t one_indexed_pos = locs(i, 0);
    if (one_indexed_pos > 1) out[i] = s.substr(one_indexed_pos - 2, 1);
  }
  return out;
}

//' Get the character two before a located pattern.
//'
//' @param s A string.
//' @param locs The output of `stringr::str_locate()`.
//'
//' @return A character vector.
//'
//' @noRd
CharacterVector get_prevprev_chars(std::string s, IntegerMatrix locs) {
  R_xlen_t n_locs = locs.nrow();
  CharacterVector out(n_locs);
  for (R_xlen_t i = 0; i != n_locs; ++i) {
    R_xlen_t one_indexed_pos = locs(i, 0);
    if (one_indexed_pos > 2) out[i] = s[one_indexed_pos - 3];
  }
  return out;
}

//' Get a tibble of currencies from a string.
//'
//' The tibble has two colums: `sym` and `amount`.
//'
//' @param s A string.
//' @param locs The output of `stringr::str_locate()`.
//'
//' @return A tibble.
//'
//' @noRd
DataFrame get_currencies_tbl(std::string s, IntegerMatrix locs) {
  R_xlen_t n_locs = locs.nrow();
  CharacterVector str_amount(n_locs);
  CharacterVector sym = get_prev_chars(s, locs);
  CharacterVector sign = get_prevprev_chars(s, locs);
  for (R_xlen_t i = 0; i != n_locs; ++i)
    str_amount[i] = s.substr(locs(i, 0) - 1, locs(i, 1) - locs(i, 0) + 1);
  NumericVector num_amount = char_to_num(str_amount, true);
  for (R_xlen_t i = 0; i != n_locs; ++i)
    if (sign[i] == "-") num_amount[i] *= -1;
  DataFrame out = DataFrame::create(Named("sym") = sym,
                                    Named("amount") = num_amount,
                                    Named("stringsAsFactors") = false);
  out.attr("class") = CharacterVector::create("tbl_df", "tbl", "data.frame");
  return out;
}

//' Get a list of data frames of currencies from a string.
//'
//' The data frames have two colums: `sym` and `amount`.
//'
//' @param strings A character vector.
//' @param locs_lst The output of `stringr::str_locate_all()`.
//'
//' @return A data frame.
//'
// [[Rcpp::export]]
List get_currencies_tbl_lst(CharacterVector strings, List locs_lst) {
  R_xlen_t n = strings.length();
  List out(n);
  for (R_xlen_t i = 0; i != n; ++i) {
    out[i] = get_currencies_tbl(as<std::string>(strings[i]),
                                as<IntegerMatrix>(locs_lst[i]));
  }
  return(out);
}
