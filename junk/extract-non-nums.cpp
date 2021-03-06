#include <Rcpp.h>
using namespace Rcpp;

#include <vector>
#include <string>

#include "substrs.h"


//' Negate `str_locate()`.
//'
//' `str_locate()` tells you where bits of a string matching your pattern are.
//' `str_unlocate()` takes the output of `str_locate()` and gives you the
//' locations of the substrings _not_ located.
//'
//' @param x A two-column integer matrix: the output of str_locate.
//' @param string_length The length of the searched string.
//'
//' @return An even-length integer vector. Each pair of elements defines a
//' substring by the position of the first character and the substring size
//' (this is ideal for feeding into C++'s `string::substr()`).
//'
//' @noRd
std::vector<std::size_t> str_unlocate(IntegerMatrix x,
                                      std::size_t string_length) {
  if (x.length() == 0) return {0, string_length};
  std::vector<std::size_t> locs;
  if (x(0, 0) != 1) {
    locs.push_back(0);
    locs.push_back(x(0, 0) - 1);
  }
  std::size_t nr = x.nrow();
  if (nr > 1) {
    for (std::size_t r = 1; r != nr; ++r) {
      if (x(r, 0) - x(r - 1, 1) > 1) {
        locs.push_back(x(r - 1, 1));
        locs.push_back(x(r, 0) - x(r - 1, 1) - 1);
      }
    }
  }
  if (x(nr - 1, 1) < string_length) {
    locs.push_back(x(nr - 1, 1));
    locs.push_back(string_length - x(nr - 1, 1));
  }
  return locs;
}

//' Get the unlocated parts of strings.
//'
//' Given a character vector and the output from `str_locate_all()`, get the
//' unlocated substrings (not their locations, the actual strings).
//'
//' @param strings A character vector.
//' @param other_locations The output of `str_locate_all()`.
//'
//' @return A list of character vectors, as output by e.g. `str_extract_all()`.
//'
//' @noRd
// [[Rcpp::export]]
List unlocated_substrs(CharacterVector strings, List other_locations) {
  std::size_t n = strings.length();
  List out(n);
  for (int i = 0; i != n; ++i) {
    std::vector<std::size_t> locs = str_unlocate(other_locations[i],
                                                 strings[i].size());
    out[i] = substrs(as<std::string>(strings[i]), locs);
  }
  return out;
}
