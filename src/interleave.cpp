#include <Rcpp.h>
using namespace Rcpp;

#include "mymath.h"


//' Interleave two vectors of strings.
//'
//' Make a vector of strings where the first element is from `strings1`, the
//' second is from `strings2`, the third is from `strings1`, the fourth is from
//' `strings2`, and so on.
//'
//' `strings1` and `strings2` must be the same length or differ in length only
//' by 1. If `strings2` is longer, it goes first.
//'
//' @param strings1,strings2 Character vectors.
//'
//' @return A character vector.
//'
//' @examples
//' interleave_strings(c("a", "c", "e"), c("b", "d"))
//'
//' @noRd
// [[Rcpp::export]]
CharacterVector interleave_strings(CharacterVector strings1,
                                   CharacterVector strings2) {
  R_xlen_t s1l = strings1.size(), s2l = strings2.size();
  int64_t length_diff = s1l - s2l;
  if (my_abs(length_diff) > 1) {
    return(NA_STRING);
  } else {
    R_xlen_t l = s1l + s2l;
    CharacterVector interleaved(l);
    int i = 0;
    if (length_diff >= 0) {
      while (i < l) {
        if (i % 2 == 0) {
          interleaved[i] = strings1[i / 2];
          i++;
        } else if (i < l) {
          interleaved[i] = strings2[i / 2];
          i++;
        }
      }
    } else {
      while (i < l) {
        if (i % 2 == 0) {
          interleaved[i] = strings2[i / 2];
          i++;
        } else if (i < l) {
          interleaved[i] = strings1[i / 2];
          i++;
        }
      }
    }
    return(interleaved);
  }
}

// [[Rcpp::export]]
List interleave_char_lists(List strings1, List strings2) {
  R_xlen_t l = strings1.size();
  List interleaved(l);
  if (l != strings2.size()) {
    auto err_msg = std::string("`interleave_char_lists()` expects ") +
      "two lists of the same length. You have passed arguments of length " +
      std::to_string(strings1.size()) + " and " +
      std::to_string(strings2.size()) + ".";
    throw std::invalid_argument(err_msg);
  } else {
    for (int i = 0; i < l; i++) {
      interleaved[i] = interleave_strings(as<CharacterVector>(strings1[i]),
                                          as<CharacterVector>(strings2[i]));
    }
  }
  return(interleaved);
}
